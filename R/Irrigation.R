#' Get irrigation depth for current day
#' @param InitCond Crop setting initial structure
#' @param IrrMngt Irrigation management
#' @param Crop parameters for a given crop
#' @param Soil properties of soil
#' @param ClockStruct crop calendar
#' @param GrowingSeason crop developmental stage
#' @param P precipitation
#' @param Runoff water Runoff
#' @return list with \code{NewCond} and \code{Irr} for a n time-step.
#' @examples
#' Irrigation(InitCond, IrrMngt, Crop, Soil, ClockStruct, GrowingSeason, P, Runoff)


Irrigation <- function(InitCond, IrrMngt, Crop, Soil, ClockStruct, GrowingSeason, P, Runoff){


  ## Store intial conditions for updating ##
  cc <- list()
  NewCond <- InitCond

  ## Calculate root zone water content and depletion ##
  GD <- RootZoneWater(Soil, Crop, NewCond)
  Wr <- GD$Wr
  Dr <- GD$Dr
  TAW <- GD$TAW
  thRZ <- GD$thRZ

  ## Determine adjustment for inflows and outflows on current day ##
  if (thRZ$Act > thRZ$Fc){
      rootdepth <- max(InitCond$Zroot,Crop$Zmin)
      AbvFc <- (thRZ$Act-thRZ$Fc) * 1000 * rootdepth
  } else {
      AbvFc <- 0
  }
  WCadj <- InitCond$Tpot + InitCond$Epot - P + Runoff - AbvFc

  ## Determine irrigation depth (mm/day) to be applied ##
  if (GrowingSeason == TRUE){
      # Update growth stage if it is first day of a growing season
      if (NewCond$DAP == 1){
          NewCond$GrowthStage <- 1
      }
      # Run irrigation depth calculation
      if (IrrMngt$IrrMethod == 0){# Rainfed - no irrigation
          Irr <- 0
      } else if(IrrMngt$IrrMethod == 1){# Irrigation - soil moisture
          # Get soil moisture target for current growth stage
          SMT <- IrrMngt$SMT[NewCond$GrowthStage]
          # Determine threshold to initiate irrigation
          IrrThr <- (1-SMT/100)*TAW
          # Adjust depletion for inflows and outflows today
          Dr <- Dr+WCadj
          if (Dr < 0){
              Dr <- 0
          }
          # Check if depletion exceeds threshold
          if (Dr > IrrThr){
              # Irrigation will occur
              IrrReq <- max(0,Dr)
              # Adjust irrigation requirements for application efficiency
              EffAdj <- ((100-IrrMngt$AppEff)+100)/100
              IrrReq <- IrrReq*EffAdj
              # Limit irrigation to maximum depth
              Irr <- min(IrrMngt$MaxIrr,IrrReq)
          } else {
              # No irrigation
              Irr <- 0
          }
      } else if (IrrMngt$IrrMethod == 2){# Irrigation - fixed interval
          # Get number of days in growing season so far (subtract 1 so that
          # always irrigate first on day 1 of each growing season)
          nDays <- NewCond$DAP-1
          # Adjust depletion for inflows and outflows today
          Dr <- Dr+WCadj
          if (Dr < 0){
              Dr <- 0
          }
          if (rem(nDays,IrrMngt$IrrInterval) == 0){
              # Irrigation occurs
              IrrReq <- max(0,Dr)
              # Adjust irrigation requirements for application efficiency
              EffAdj <- ((100-IrrMngt$AppEff)+100)/100
              IrrReq <- IrrReq*EffAdj
              # Limit irrigation to maximum depth
              Irr <- min(IrrMngt$MaxIrr,IrrReq)
          } else {
              # No irrigation
              Irr <- 0
          }
      } else if (IrrMngt$IrrMethod == 3){# Irrigation - pre-defined schedule
          # Get current date
          CurrentDate <- ClockStruct$StepStartTime
          # Find irrigation value corresponding to current date
          Irr <- IrrMngt$IrrigationSch[(IrrMngt$IrrigationSch[,1] == CurrentDate),2]
      } else if (IrrMngt$IrrMethod == 4){# Irrigation - net irrigation
          # Net irrigation calculation performed after transpiration, so
          # irrigation is zero here
          Irr <- 0
      }
      # Update cumulative irrigation counter for growing season
      NewCond$IrrCum <- NewCond$IrrCum+Irr
  } else if (GrowingSeason == FALSE){
      # No irrigation outside growing season
      Irr <- 0
      NewCond$IrrCum <- 0
  }

  cc$NewCond <- NewCond
  cc$Irr <- Irr

  return(cc)

}

