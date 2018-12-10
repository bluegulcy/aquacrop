#' Set up initial model conditions
#' @param ParamStruct Crop details
#' @param GwStruct Water table details
#' @param FieldMngtStruct field management structure
#' @param CropChoices crops whose performance is to be modelled
#' @param FileLocation file locations
#' @param ClockStruct crop calendar structure
#' @return \code{InitCondStruct} with intial conditions.
#' @examples
#' ReadModelInitialConditions(ParamStruct, GwStruct, FieldMngtStruct, CropChoices, FileLocation,
#' ClockStruct)

ReadModelInitialConditions <- function(ParamStruct, GwStruct, FieldMngtStruct, CropChoices, FileLocation,
                                           ClockStruct){
      # 

      ## Declare global variables ##


      ## Define initial conditions ##
      InitCondStruct <- list()

      # Counters
      InitCondStruct$AgeDays =  InitCondStruct$AgeDays_NS =
      InitCondStruct$AerDays = InitCondStruct$IrrCum = InitCondStruct$DelayedGDDs =
      InitCondStruct$DelayedCDs = InitCondStruct$PctLagPhase =
      InitCondStruct$tEarlySen = InitCondStruct$GDDcum =
      InitCondStruct$DaySubmerged = InitCondStruct$IrrNetCum =
      InitCondStruct$DAP = InitCondStruct$Epot = InitCondStruct$Tpot <- 0

      # States
      InitCondStruct$PreAdj = InitCondStruct$CropMature = InitCondStruct$CropDead =
          InitCondStruct$Germination = InitCondStruct$PrematSenes =
          InitCondStruct$HarvestFlag <- FALSE

      # Harvest index
      InitCondStruct$Stage = InitCondStruct$Fpre = InitCondStruct$Fpost =
          InitCondStruct$fpost_dwn = InitCondStruct$fpost_upp <- 1

      InitCondStruct$HIcor_Asum = InitCondStruct$HIcor_Bsum =
          InitCondStruct$Fpol = InitCondStruct$sCor1 = InitCondStruct$sCor2 <- 0

      # Growth stage
      InitCondStruct$GrowthStage <- 0

      # Aeration stress (compartment level)
      InitCondStruct$AerDaysComp <- rep(0, 1,ParamStruct$Soil$nComp)

      # Transpiration
      InitCondStruct$TrRatio <- 1

      # Crop growth
      InitCondStruct$CC = InitCondStruct$CCadj = InitCondStruct$CC_NS =
          InitCondStruct$CCadj_NS = InitCondStruct$Zroot = InitCondStruct$B =
          InitCondStruct$B_NS = InitCondStruct$HI = InitCondStruct$HIadj =
          InitCondStruct$CCxAct = InitCondStruct$CCxAct_NS =
          InitCondStruct$CCxW = InitCondStruct$CCxW_NS =
          InitCondStruct$CCxEarlySen = InitCondStruct$CCprev <- 0
      InitCondStruct$rCor <- 1

      if(ClockStruct$SeasonCounter == 0){
          InitCondStruct$Zroot <- 0
          InitCondStruct$CC0adj <- 0
      } else if(ClockStruct$SeasonCounter == 1){
          InitCondStruct$Zroot <- ParamStruct$Crop[[CropChoices[ClockStruct$SeasonCounter]]]$Zmin
          InitCondStruct$CC0adj <- ParamStruct$Crop[[CropChoices[ClockStruct$SeasonCounter]]]$CC0
      }

      # Surface storage between bunds
      if(FieldMngtStruct$Bunds == 0 & FieldMngtStruct$zBund > 0.001){
          # Get initial storage between surface bunds
          InitCondStruct$SurfaceStorage <- FieldMngtStruct$BundWater
          if(InitCondStruct$SurfaceStorage > FieldMngtStruct$zBund){
              InitCondStruct$SurfaceStorage <- FieldMngtStruct$zBund
          }
          InitCondStruct$SurfaceStorageIni <- InitCondStruct$SurfaceStorage
      } else{
          # No surface bunds
          InitCondStruct$SurfaceStorage <- 0
          InitCondStruct$SurfaceStorageIni <- 0
      }

      ## Check for presence of groundwater table ##
      if (GwStruct$WaterTable == 0){# No water table present
          # Set initial groundwater level to dummy value
          InitCondStruct$zGW <- -999
          InitCondStruct$WTinSoil <- FALSE
          # Set adjusted field capacity to default field capacity
          InitCondStruct$th_fc_Adj <- ParamStruct$Soil$Comp$th_fc
      } else if (GwStruct$WaterTable == 1){# Water table is present
          # Set initial groundwater level
          InitCondStruct$zGW <- GwStruct$zGW[(GwStruct$zGW[,1] == 
                                                 ClockStruct$StepStartTime),2]
          # Find compartment mid-points
          zBot <- cumsum(ParamStruct$Soil$Comp$dz)
          zTop <- zBot-ParamStruct$Soil$Comp$dz
          zMid <- (zTop+zBot)/2
          # Check if water table is within modelled soil profile ##
          if(InitCondStruct$zGW >= 0){
              idx <- which(zMid >= InitCondStruct$zG)[1]
              if (isempty(idx)){
                  InitCondStruct$WTinSoil <- FALSE
              } else {
                  InitCondStruct$WTinSoil <- TRUE
              }
          } else {
              InitCondStruct$WTinSoil <- FALSE
          }
          # Adjust compartment field capacity
          compi <- ParamStruct$Soil$nComp
          thfcAdj <- rep(0, 1, compi)
          while(compi >  1){
              layeri <- ParamStruct$Soil$Comp$Layer[compi]
              if (ParamStruct$Soil$Layer$th_fc[layeri] <= 0.1){
                  Xmax <- 1
              } else {
                  if(ParamStruct$Soil$Layer$th_fc[layeri] >= 0.3){
                      Xmax <- 2
                  } else {
                      pF <- 2+0.3*(ParamStruct$Soil$Layer$th_fc[layeri] - 0.1)/0.2
                      Xmax <- (exp(pF*log(10)))/100
                  }
              }
              if(InitCondStruct$zGW < 0 | (InitCondStruct$zGW - zMid[compi]) >= Xmax){
                  for (ii in 1:compi){
                      layerii <- ParamStruct$Soil$Comp$Layer(ii)
                      thfcAdj(ii) <- ParamStruct$Soil$Layer$th_fc[layerii]
                  }
                  compi <- 0
              } else {
                  if (ParamStruct$Soil$Layer$th_fc[layeri] >= 
                      ParamStruct$Soil$Layer$th_s[layeri]){
                      thfcAdj[compi] <- ParamStruct$Soil$Layer$th_fc[layeri]
                  } else {
                      if (zMid[compi] >= InitCondStruct$zGW){
                          thfcAdj[compi] <- ParamStruct$Soil$Layer$th_s[layeri]
                      } else {
                          dV <- ParamStruct$Soil$Layer$th_s[layeri] -
                              ParamStruct$Soil$Layer$th_fc[layeri]
                          dFC <- (dV/(Xmax^2)) * ((zMid[compi] - (InitCondStruct$zGW-Xmax))^2)
                          thfcAdj[compi] <- ParamStruct$Soil$Layer$th_fc[layeri] + dFC
                      }
                  }
                  compi <- compi-1
              }
          }
          # Store adjusted field capacity values
          InitCondStruct$th_fc_Adj <- round((thfcAdj*1000))/1000
      }

      ## Define initial soil water contents ##
      # Read input file
      Location <- FileLocation[['Input']]
      filename <- strcat(Location, FileLocation[['InitialSWCFilename']])
      
      if(!file.exists(filename)){
         print(paste('Error,',  'Initial Soil Water Content file does not exist', 
                     sep = ' '))
         break
      } else {
         
         DataArray <- check_xml_exist(filename)
      }
   
      DataArray <- convert_list_2numeric(DataArray,
                                         c('number_of_input_points'))

      TypeStr <- DataArray[['type_of_value']]
      MethodStr <- DataArray[['method']]
      Data_Pts <- DataArray[['input_data_points']]
      Data_Pts <- data.frame(t(rbind(sapply(1:length(Data_Pts),
                                            function(x) unlist(Data_Pts[[x]])))))
      Data_Pts[['depth_layer']] <- as.numeric(
         as.character(Data_Pts[['depth_layer']]))
      
      if(TypeStr != 'Prop'){
         
            Data_Pts[['value']] <- 
               as.numeric(as.character(Data_Pts[['value']]))
      }

      # Extract data
      Locs <- Data_Pts[['depth_layer']]
      Vals <- rep(1, length(Locs))
      Method <- MethodStr
      Type <- TypeStr
      
      if(Method == 'Depth'){
          Locs <- round((100*Locs))/100
      }

      # Define soil compartment depths and layers
      SoilLayers <- ParamStruct$Soil$Comp$Layer
      SoilDepths <- cumsum(ParamStruct$Soil$Comp$dz)
      SoilDepths <- round((100*SoilDepths))/100

      # Assign data
      if(Type == 'Num'){
          # Values are defined as numbers (m3/m3) so no calculation required
          Vals <- as.numeric(Data_Pts[,'value'])
      } else if(Type == 'Pct'){
          # Values are defined as percentage of TAW. Extract and assign value for
          # each soil layer based on calculated/input soil hydraulic properties
          ValsTmp <- Data_Pts[,'value']
          for(ii in 1:length(ValsTmp)){
              if(Method == 'Depth'){
                  # Find layer at specified depth
                  if(Locs[ii] < SoilDepths[length(Locs)]){
                      LayTmp <- SoilLayers[which(Locs[ii] >= SoilDepths)[1]]
                  } else {
                      LayTmp <- SoilLayers[length(SoilLayers)]
                  }
                  # Calculate moisture content at specified depth
                  Vals[ii] <- ParamStruct$Soil$Layer$th_wp[LayTmp] +
                      ((ValsTmp[ii]/100) * (ParamStruct$Soil$Layer$th_fc[LayTmp] -
                      ParamStruct$Soil$Layer$th_wp[LayTmp]))
              } else if(Method == 'Layer'){
                  # Calculate moisture content at specified layer
                  LayTmp <- Locs[ii]
                  Vals[ii] <- ParamStruct$Soil$Layer$th_wp[LayTmp] +
                      ((ValsTmp[ii] / 100) * (ParamStruct$Soil$Layer$th_fc[LayTmp] -
                      ParamStruct$Soil$Layer$th_wp[LayTmp]))
              }
          }
      } else if(Type == 'Prop'){
          # Values are specified as soil hydraulic properties (SAT, FC, or WP).
          # Extract and assign value for each soil layer
          ValsTmp <- as.character(Data_Pts[,'value'])
          for(ii in 1:length(ValsTmp)){
              if(Method == 'Depth'){
                  # Find layer at specified depth
                  if(Locs[ii] < SoilDepths[length(Locs)]){
                      LayTmp <- SoilLayers[which(Locs[ii] >= SoilDepths)[1]]
                  } else {
                      LayTmp <- SoilLayers[length(SoilLayers)]
                  }
                  # Calculate moisture content at specified depth
                  if(ValsTmp[ii] == 'SAT'){
                      Vals[ii] <- ParamStruct$Soil$Layer$th_s[LayTmp]
                  } else if (ValsTmp[ii] == 'FC'){
                      Vals[ii] <- ParamStruct$Soil$Layer$th_fc[LayTmp]
                  } else if (ValsTmp[ii] == 'WP'){
                      Vals[ii] <- ParamStruct$Soil$Layer$th_wp[LayTmp]
                  }
              } else if (Method == 'Layer'){
                  # Calculate moisture content at specified layer
                  LayTmp <- Locs[ii]
                  if (ValsTmp[ii] == 'SAT'){
                      Vals[ii] <- ParamStruct$Soil$Layer$th_s[LayTmp]
                  } else if (ValsTmp[ii] == 'FC'){
                      Vals[ii] <- ParamStruct$Soil$Layer$th_fc[LayTmp]
                  } else if (ValsTmp[ii] == 'WP'){
                      Vals[ii] <- ParamStruct$Soil$Layer$th_wp[LayTmp]
                  } else{
                     
                     print(paste('Prop: ',ValsTmp[ii], 'is not valid'))
                     break
                  
                  }
              }
          }
      }

      # Interpolate values to all soil compartments
      thini <- rep(0, ParamStruct$Soil$nComp)
      if(Method == 'Layer'){
          for (ii in 1:length(Vals)){
              thini[ParamStruct$Soil$Comp$Layer == Locs[ii]] <- Vals[ii]
          }
          InitCondStruct$th <- thini
      } else if(Method == 'Depth'){
          # Add zero point
          if(Locs[1] > 0){
              Locs <- c(0, Locs)
              Vals <- c(Vals[1], Vals) # 
          }
          # Add end point (bottom of soil profile)
          if(Locs[length(Locs)] < ParamStruct$Soil$zSoil){
              Locs <- c(Locs, ParamStruct$Soil$zSoil)
              Vals <- c(Vals, Vals[length(Vals)])
          }
          # Find centroids of compartments
          comp_top <- c(0, SoilDepths[1: length(SoilDepths)-1])
          comp_bot <- SoilDepths[1:length(SoilDepths)]
          comp_mid <- (comp_top + comp_bot)/2
          # Interpolate initial water contents to each compartment
          # FIXME Approx doesn't work for 1 value
          thini <- approx(Locs, Vals, comp_mid)
          InitCondStruct$th <- thini$y
          
      }

      # If groundwater table is present and calculating water contents based on
      # field capacity, then reset value to account for possible changes in field
      # capacity caused by capillary rise effects
      if (GwStruct$WaterTable == 1){
          if(Type =='Prop' & ValsTmp[1] == 'FC'){
              InitCondStruct$th <- InitCondStruct$th_fc_Adj
          }
      }

      # If groundwater table is present in soil profile then set all water
      # contents below the water table to saturation
      if(InitCondStruct$WTinSoil == TRUE){
          # Find compartment mid-points
          zBot <- cumsum(ParamStruct$Soil$Comp$dz)
          zTop <- zBot-ParamStruct$Soil$Comp$dz
          zMid <- (zTop+zBot)/2
          idx <- find(zMid >= InitCondStruct$zGW,1)
          for(ii in idx:ParamStruct$Soil$nComp){
              layeri <- ParamStruct$Soil$Comp$Layer[ii]
              InitCondStruct$th[ii] <- ParamStruct$Soil$Layer$th_s[layeri]
          }
      }

      InitCondStruct$thini <- InitCondStruct$th

      return(InitCondStruct)
}
