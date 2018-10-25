#' Run AquaCrop for a n time step
#' @param Weather weather parameters or n time-step
#' @param InitialiseStruct Crop setting initial structure
#' @return list with \code{NewCond}, \code{Outputs} and \code{ClockStruct} for n time-step
#' @examples
#' Solution(Weather, ClockStruct, InitialiseStruct)


Solution <- function(Weather, InitialiseStruct){

    cc <- list()
    ## Unpack structures ##
    if(InitialiseStruct$ClockStruct$SeasonCounter > 0){
        Crop <- InitialiseStruct$Parameter$Crop[[
            InitialiseStruct$CropChoices[InitialiseStruct$ClockStruct$SeasonCounter]]]
        IrrMngt <- InitialiseStruct$IrrigationManagement[[
            InitialiseStruct$CropChoices[InitialiseStruct$ClockStruct$SeasonCounter]]]
        
    }

    FieldMngt <- InitialiseStruct$FieldManagement
    Soil <- InitialiseStruct$Parameter$Soil
    CO2 <- InitialiseStruct$Parameter$CO2
    Groundwater <- InitialiseStruct$Groundwater
    P <- Weather$Precipitation
    Tmax <- Weather$MaxTemp
    Tmin <- Weather$MinTemp
    Et0 <- Weather$ReferenceET

    ## Store initial conditions in structure for updating ##
    NewCond <- InitialiseStruct$InitialCondition


    ## Check if growing season is active on current time step ##
    if(InitialiseStruct$ClockStruct$SeasonCounter > 0){
        CurrentDate <- InitialiseStruct$ClockStruct$StepStartTime
        PlantingDate <- 
          InitialiseStruct$ClockStruct$PlantingDate[InitialiseStruct$ClockStruct$SeasonCounter]
        HarvestDate <- 
          InitialiseStruct$ClockStruct$HarvestDate[InitialiseStruct$ClockStruct$SeasonCounter]
        if((CurrentDate >= PlantingDate & CurrentDate <= HarvestDate) &
                (NewCond$CropMature == FALSE & NewCond$CropDead == FALSE)){
            GrowingSeason <- TRUE
        } else {
            GrowingSeason <- FALSE
        }
    } else {
        # Not yet reached start of first growing season
        GrowingSeason <- FALSE
    }

  
    ## Increment time counters ##
    if(GrowingSeason == TRUE){
        # Calendar days after planting
        NewCond$DAP <- NewCond$DAP + 1
        # Growing degree days after planting
        GD <- GrowingDegreeDay(Crop, NewCond, Tmax, Tmin)
        GDD <- GD$GDD
        NewCond <- GD$NewCond
    } else {
        # Calendar days after planting
        NewCond$DAP <- 0
        # Growing degree days after planting
        GDD <- 0
        NewCond$GDDcum <- 0
    }
    
    Outputs <- InitialiseStruct$Outputs
    row_day <- InitialiseStruct$ClockStruct$TimeStepCounter
    row_gs <- InitialiseStruct$ClockStruct$SeasonCounter

    if(GrowingSeason == TRUE){
          ## Run simulations ##
          # 1. Check for groundwater table
          NewCond <- CheckGroundwaterTable(Soil, Groundwater, NewCond, 
                                           InitialiseStruct$ClockStruct)
      
          # 2. Pre-irrigation #
      ## NOPE
          GD <- PreIrrigation(Soil, Crop, IrrMngt, NewCond)
          NewCond <- GD$NewCond
          PreIrr <- GD$PreIrr
          # 3. Drainage
      
      
          GD <- Drainage(Soil, NewCond)
          NewCond <- GD$NewCond
          DeepPerc <- GD$DeepPerc
          FluxOut <- GD$FluxOut
      
      
      
          # 4. Surface runoff
          GD <- RainfallPartition(P, Soil, FieldMngt, NewCond)
          Runoff <- GD$Runoff
          Infl <- GD$Infl
          NewCond <- GD$NewCond
      
      
      ## NOPE
          # 5. Irrigation
          GD <- Irrigation(NewCond, IrrMngt, Crop, Soil, InitialiseStruct$ClockStruct, 
                           GrowingSeason, P, Runoff)
          NewCond <- GD$NewCond
          Irr <- GD$Irr
      
      # NOPE
          # 6. Infiltration
          GD <- Infiltration(Soil, NewCond, Infl, Irr, IrrMngt, FieldMngt, FluxOut, 
                             DeepPerc, Runoff)
          NewCond <- GD$NewCond
          DeepPerc <- GD$DeepPerc
          Runoff <- GD$Runoff
          Infl <- GD$Infl
          FluxOut <- GD$FluxOut
      
      
      
          # 7. Capillary rise
          GD <- CapillaryRise(Soil,Groundwater,NewCond,FluxOut)
          NewCond <- GD$NewCond
          CR <- GD$CrTot
          # 8. Check germination
      
      
          NewCond <- Germination(NewCond,Soil,Crop,GDD,GrowingSeason)
      
      ## NOPE
          # 9. Update growth stage
          NewCond <- GrowthStage(Crop, NewCond, GrowingSeason)
      
      # NOPE
          # 10. Root development
          NewCond <- RootDevelopment(Crop, Soil,Groundwater, NewCond, 
                                     GDD, GrowingSeason)
      
      # NOPE
          # 11. Canopy cover development
          NewCond <- CanopyCover(Crop, Soil, NewCond, GDD, Et0, GrowingSeason)
      
      # NOPE
          # 12. Soil evaporation
          GD <- SoilEvaporation(InitialiseStruct$ClockStruct, Soil, Crop, 
                                IrrMngt, FieldMngt, 
                                    NewCond, Et0,Infl, P, Irr, GrowingSeason)
          NewCond <- GD$NewCond
          Es <- GD$EsAct
          EsPot <- GD$EsPot
      
      # NOPE
          # 13. Crop transpiration
          GD <- Transpiration(Soil, Crop,  IrrMngt,NewCond,Et0,CO2,GrowingSeason)
          Tr <- GD$TrAct
          TrPot_NS <- GD$TrPot_NS
          TrPot <- GD$TrPot0
          NewCond <- GD$NewCond
          IrrNet <- GD$IrrNet
      
      
          # 14. Groundwater inflow
          GD <- GroundwaterInflow(Soil, NewCond)
          NewCond <- GD$NewCond
          GwIn <- GD$GwIn
      
      #NOPE
          # 15. Reference harvest index
          NewCond <- HIrefCurrentDay(NewCond, Crop, GrowingSeason)
      
      
      # NOPE
          # 16. Biomass accumulation
          NewCond <- BiomassAccumulation(Crop,NewCond,Tr, TrPot_NS, Et0,
              Tmax,Tmin,GDD,GrowingSeason)
      #NOPE
          # 17. Harvest index
          NewCond <- HarvestIndex(Soil, Crop, NewCond, Et0, Tmax, Tmin, GDD, 
                                  GrowingSeason)
      
          # 18. Crop yield
          if(GrowingSeason == TRUE){
              # Calculate crop yield (tonne/ha)
              NewCond$Y <- (NewCond$B/100) * NewCond$HIadj
              # Check if crop has reached maturity
              if ((Crop$CalendarType == 1 & (NewCond$DAP-NewCond$DelayedCDs) >= 
                   Crop$Maturity) |
                      (Crop$CalendarType == 2 & (NewCond$GDDcum - 
                                                 NewCond$DelayedGDDs) >= Crop$Maturity)){
                  # Crop has reached maturity
                  NewCond$CropMature <- TRUE
              }
          } else {
              # Crop yield is zero outside of growing season
              NewCond$Y <- 0
          }
      
      #NOPE
      
          # 19. Root zone water
          GD <- RootZoneWater(Soil, Crop, NewCond)
          Wr <- GD$Wr
      
          #NOPE
          # 20. Update net irrigation to add any pre irrigation
          IrrNet <- IrrNet + PreIrr
          #NOPE
          NewCond$IrrNetCum <- NewCond$IrrNetCum + PreIrr
      
      
          ## Update model outputs ##
          # Outputs <- InitialiseStruct$Outputs
          # row_day <- InitialiseStruct$ClockStruct$TimeStepCounter
          # row_gs <- InitialiseStruct$ClockStruct$SeasonCounter
          
          #NOPE
          # Irrigation
          if (IrrMngt$IrrMethod == 4){
              # Net irrigation
              IrrDay <- IrrNet
              IrrTot <- NewCond$IrrNetCum
          } else {
              # Irrigation
              IrrDay <- Irr
              IrrTot <- NewCond$IrrCum
          }
      
      
          # Water contents
          Outputs$WaterContents[row_day,4:ncol(Outputs$WaterContents)] <- 
            as.numeric(c(InitialiseStruct$ClockStruct$TimeStepCounter,
              GrowingSeason, NewCond$th, PlantingDate))
          # Water fluxes
          Outputs$WaterFluxes[row_day, 4:ncol(Outputs$WaterFluxes)] <- 
            c(InitialiseStruct$ClockStruct$TimeStepCounter,
              GrowingSeason, Wr,NewCond$zGW, NewCond$SurfaceStorage, IrrTot, Infl, Runoff,
              DeepPerc, CR, GwIn, Es, EsPot,Tr, TrPot, PlantingDate)
          # Crop growth
          Outputs$CropGrowth[row_day,4:ncol(Outputs$CropGrowth)] <- 
            c(InitialiseStruct$ClockStruct$TimeStepCounter,
              GrowingSeason, GDD, NewCond$GDDcum, NewCond$Zroot, NewCond$CC, NewCond$CC_NS,
              NewCond$B, NewCond$B_NS, NewCond$HI, NewCond$HIadj, NewCond$Y, Et0, PlantingDate)
    } else{
      
      Outputs$WaterContents[row_day,4:ncol(Outputs$WaterContents)] <- 
        as.numeric(c(InitialiseStruct$ClockStruct$TimeStepCounter,
                     GrowingSeason, NewCond$th, 0))
      # Water fluxes
      Outputs$WaterFluxes[row_day, 4:ncol(Outputs$WaterFluxes)] <- 
        c(InitialiseStruct$ClockStruct$TimeStepCounter,
          GrowingSeason, rep(0,13), 0)
      # Crop growth
      Outputs$CropGrowth[row_day,4:ncol(Outputs$CropGrowth)] <- 
        c(InitialiseStruct$ClockStruct$TimeStepCounter,
          GrowingSeason, GDD, NewCond$GDDcum, rep(0,9), 0)
      
      
    }  
    
    # Final output (if at end of growing season)
    if(InitialiseStruct$ClockStruct$SeasonCounter > 0){

        if((NewCond$CropMature == TRUE | NewCond$CropDead == TRUE |
           (InitialiseStruct$ClockStruct$StepEndTime == 
            InitialiseStruct$ClockStruct$HarvestDate[InitialiseStruct$ClockStruct$SeasonCounter])) &
                NewCond$HarvestFlag == FALSE){


            # Get planting and harvest dates
            plant_sdate <- which(InitialiseStruct$ClockStruct$TimeSpan ==
                InitialiseStruct$ClockStruct$PlantingDate[InitialiseStruct$ClockStruct$SeasonCounter])
            plant_cdate <- as_date(InitialiseStruct$ClockStruct$PlantingDate[
              InitialiseStruct$ClockStruct$SeasonCounter])
            harvest_cdate <- as_date(InitialiseStruct$ClockStruct$StepStartTime)
            harvest_sdate <- InitialiseStruct$ClockStruct$TimeStepCounter

            # Store final outputs
            Outputs$FinalOutput[row_gs,] <- c(InitialiseStruct$ClockStruct$SeasonCounter,
                InitialiseStruct$CropChoices[InitialiseStruct$ClockStruct$SeasonCounter],
                plant_cdate, plant_sdate, harvest_cdate, harvest_sdate,
                NewCond$Y, IrrTot)

            # Set harvest flag
            NewCond$HarvestFlag <- TRUE
        }
    }


    cc$NewCond <- NewCond
    cc$Outputs <- Outputs
    cc$ClockStruct <- InitialiseStruct$ClockStruct

    return(cc)

}
