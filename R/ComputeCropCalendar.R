#' Compute additional parameters needed to define crop
#' @param Crop list
#' @param CropName list with crops names
#' @param CropChoices crops to be analysed
#' @param Weather dataset with weather data
#' @param ClockStruct crop calendar
#' @return \code{Crop}.
#' @export
#' @examples
#' ComputeCropCalendar(Crop, CropName, CropChoices, Weather, ClockStruct)


ComputeCropCalendar <- function(Crop, CropName, CropChoices, Weather, ClockStruct){


      ## Define crop calendar mode ##
      Mode <- Crop$CalendarType

      ## Calculate variables ##
      if (Mode == 1){# Growth in calendar days

          # Time from sowing to end of vegetative growth period
          if (Crop$Determinant == 1){
              Crop$CanopyDevEnd <- round(Crop$HIstart + (Crop$Flowering/2))
          } else {
              Crop$CanopyDevEnd <- Crop$Senescence
          }

          # Time from sowing to 10# canopy cover (non-stressed conditions)
          Crop$Canopy10Pct <- round(Crop$Emergence + (log(0.1/Crop$CC0)/Crop$CGC))

          # Time from sowing to maximum canopy cover (non-stressed conditions)
          Crop$MaxCanopy <- round(Crop$Emergence + (log((0.25*Crop$CCx*Crop$CCx/Crop$CC0)/
              (Crop$CCx-(0.98*Crop$CCx)))/Crop$CGC))

          # Time from sowing to end of yield formation
          Crop$HIend <- Crop$HIstart + Crop$YldForm

          # Duplicate calendar values (needed to minimise if statements when
          # switching between GDD and CD runs)
          Crop$EmergenceCD <- Crop$Emergence
          Crop$Canopy10PctCD <- Crop$Canopy10Pct
          Crop$MaxRootingCD <- Crop$MaxRooting
          Crop$SenescenceCD <- Crop$Senescence
          Crop$MaturityCD <- Crop$Maturity
          Crop$MaxCanopyCD <- Crop$MaxCanopy
          Crop$CanopyDevEndCD <- Crop$CanopyDevEnd
          Crop$HIstartCD <- Crop$HIstart
          Crop$HIendCD <- Crop$HIend
          Crop$YldFormCD <- Crop$YldForm
          if (Crop$CropType == 3){
              Crop$FloweringEnd <- Crop$HIstart + Crop$Flowering
              Crop$FloweringEndCD <- Crop$FloweringEnd
              Crop$FloweringCD <- Crop$Flowering
          }
          # Check if converting crop calendar to GDD mode
          if (Crop$SwitchGDD == 1){
              # Extract weather data for first growing season
              # FIXME this computation seems a bit clumsy
              idx <-  which(match(CropChoices, CropName)[1] == 1)
              tSta <- ClockStruct$PlantingDate[idx]
              tSto <- ClockStruct$HarvestDate[idx]
              Dates <- Weather[,1]
              StaRow <- which(Dates == tSta)
              StoRow <- which(Dates == tSto)

              Tmin <- Weather[StaRow:StoRow,2]
              Tmax <- Weather[StaRow:StoRow,3]
              # Calculate GDD's
              if (Crop$GDDmethod == 1){
                  Tmean <- (Tmax + Tmin)/2
                  Tmean[Tmean > Crop$Tupp] <- Crop$Tupp
                  Tmean[Tmean < Crop$Tbase] <- Crop$Tbase
                  GDD <- Tmean - Crop$Tbase
              } else if (Crop$GDDmethod == 2){
                  Tmax[Tmax > Crop$Tupp] <- Crop$Tupp
                  Tmax[Tmax < Crop$Tbase] <- Crop$Tbase
                  Tmin[Tmin > Crop$Tupp] <- Crop$Tupp
                  Tmin[Tmin < Crop$Tbase] <- Crop$Tbase
                  Tmean <- (Tmax + Tmin)/2
                  GDD <- Tmean - Crop$Tbase
              } else if (Crop$GDDmethod == 3){
                  Tmax[Tmax > Crop$Tupp] <- Crop$Tupp
                  Tmax[Tmax < Crop$Tbase] <- Crop$Tbase
                  Tmin[Tmin > Crop$Tupp] <- Crop$Tupp
                  Tmean <- (Tmax + Tmin)/2
                  Tmean[Tmean < Crop$Tbase] <- Crop$Tbase
                  GDD <- Tmean - Crop$Tbase
              }
              GDDcum <- cumsum(GDD)
              # Find GDD equivalent for each crop calendar variable
              # 1. GDD's from sowing to emergence
              Crop$Emergence <- GDDcum[Crop$EmergenceCD]
              # 2. GDD's from sowing to 10# canopy cover
              Crop$Canopy10Pct <- GDDcum[Crop$Canopy10PctCD]
              # 3. GDD's from sowing to maximum rooting
              Crop$MaxRooting <- GDDcum[Crop$MaxRootingCD]
              # 4. GDD's from sowing to maximum canopy cover
              Crop$MaxCanopy <- GDDcum[Crop$MaxCanopyCD]
              # 5. GDD's from sowing to end of vegetative growth
              Crop$CanopyDevEnd <- GDDcum[Crop$CanopyDevEndCD]
              # 6. GDD's from sowing to senescence
              Crop$Senescence <- GDDcum[Crop$SenescenceCD]
              # 7. GDD's from sowing to maturity
              Crop$Maturity <- GDDcum[Crop$MaturityCD]
              # 8. GDD's from sowing to start of yield formation
              Crop$HIstart <- GDDcum[Crop$HIstartCD]
              # 9. GDD's from sowing to start of yield formation
              Crop$HIend <- GDDcum[Crop$HIendCD]
              # 10. Duration of yield formation (GDD's)
              Crop$YldForm <- Crop$HIend - Crop$HIstart
              # 11. Duration of flowering (GDD's) - (fruit/grain crops only)
              if (Crop$CropType == 3){
                  # GDD's from sowing to end of flowering
                  Crop$FloweringEnd <- GDDcum[Crop$FloweringEndCD]
                  # Duration of flowering (GDD's)
                  Crop$Flowering <- Crop$FloweringEnd-Crop$HIstart
              }
              # Convert CGC to GDD mode
              Crop$CGC_CD <- Crop$CGC
              Crop$CGC <- (log((((0.98*Crop$CCx)-Crop$CCx)*Crop$CC0)/(-0.25*(Crop$CCx^2))))/
                  (-(Crop$MaxCanopy - Crop$Emergence))
              # Convert CDC to GDD mode
              Crop$CDC_CD <- Crop$CDC
              tCD <- Crop$MaturityCD - Crop$SenescenceCD
              if (tCD <= 0){
                  tCD <- 1
              }
              CCi <- Crop$CCx*(1-0.05*(exp((Crop$CDC_CD/Crop$CCx)*tCD)-1))
              if (CCi < 0){
                  CCi <- 0
              }
              tGDD <- Crop$Maturity - Crop$Senescence
              if (tGDD <= 0){
                  tGDD <- 5
              }
              Crop$CDC <- (Crop$CCx/tGDD)*log(1 + ((1-CCi/Crop$CCx)/0.05))
              # Set calendar type to GDD mode
              Crop$CalendarType <- 2
          }
      } else if (Mode == 2){

        # Growth in growing degree days
          # Time from sowing to end of vegetative growth period
          if(Crop$Determinant == 1){
              Crop$CanopyDevEnd <- round(Crop$HIstart + (Crop$Flowering/2))
          } else {
              Crop$CanopyDevEnd <- Crop$Senescence
          }

          # Time from sowing to 10# canopy cover (non-stressed conditions)
          Crop$Canopy10Pct <- round(Crop$Emergence + (log(0.1/Crop$CC0)/Crop$CGC))

          # Time from sowing to maximum canopy cover (non-stressed conditions)
          Crop$MaxCanopy <- round(Crop$Emergence + (log((0.25*Crop$CCx*Crop$CCx/Crop$CC0)/
              (Crop$CCx-(0.98*Crop$CCx)))/Crop$CGC))

          # Time from sowing to end of yield formation
          Crop$HIend <- Crop$HIstart + Crop$YldForm

          # Time from sowing to end of flowering (if fruit/grain crop)
          if (Crop$CropType == 3){
              Crop$FloweringEnd <- Crop$HIstart + Crop$Flowering
          }

          # Additional variables
          # Extract weather data for first growing season that crop is planted
          idx <- which(match(CropChoices, CropName)[1] == 1)
          tSta <- ClockStruct$PlantingDate[idx]
          tSto <- ClockStruct$HarvestDate[idx]
          Dates <- Weather[,1]
          StaRow <- which(Dates == tSta)
          StoRow <- which(Dates == tSto)

          Tmin <- Weather[StaRow:StoRow,2]
          Tmax <- Weather[StaRow:StoRow,3]

          # Calculate GDD's
          if (Crop$GDDmethod == 1){
              Tmean <- (Tmax + Tmin)/2
              Tmean[Tmean > Crop$Tupp] <- Crop$Tupp
              Tmean[Tmean < Crop$Tbase] <- Crop$Tbase
              GDD <- Tmean-Crop$Tbase
          } else if (Crop$GDDmethod == 2){
              Tmax[Tmax > Crop$Tupp] <- Crop$Tupp
              Tmax[Tmax < Crop$Tbase] <- Crop$Tbase
              Tmin[Tmin > Crop$Tupp] <- Crop$Tupp
              Tmin[Tmin < Crop$Tbase] <- Crop$Tbase
              Tmean <- (Tmax + Tmin)/2
              GDD <- Tmean - Crop$Tbase
          } else if (Crop$GDDmethod == 3){
              Tmax[Tmax > Crop$Tupp] <- Crop$Tupp
              Tmax[Tmax < Crop$Tbase] <- Crop$Tbase
              Tmin[Tmin > Crop$Tupp] <- Crop$Tupp
              Tmean <- (Tmax + Tmin)/2
              Tmean[Tmean < Crop$Tbase] <- Crop$Tbase
              GDD <- Tmean-Crop$Tbase
          }
          GDDcum <- cumsum(GDD)

          # Find calendar days for some variables
          # 1. Calendar days from sowing to maximum canopy cover
          Crop$MaxCanopyCD <- which(GDDcum > Crop$MaxCanopy)[1]
          # 1. Calendar days from sowing to end of vegetative growth
          Crop$CanopyDevEndCD <- which(GDDcum > Crop$CanopyDevEnd)[1]
          # 2. Calendar days from sowing to start of yield formation
          Crop$HIstartCD <- which(GDDcum > Crop$HIstart)[1]
          # 3. Calendar days from sowing to end of yield formation
          Crop$HIendCD <- which(GDDcum > Crop$HIend)[1]
          # 4. Duration of yield formation in calendar days
          Crop$YldFormCD <- Crop$HIendCD - Crop$HIstartCD
          if (Crop$CropType == 3){
              # 1. Calendar days from sowing to end of flowering
              FloweringEnd <- which(GDDcum > Crop$FloweringEnd)[1]
              # 2. Duration of flowering in calendar days
              Crop$FloweringCD <- FloweringEnd - Crop$HIstartCD
          }
      }

      return(Crop)

}

