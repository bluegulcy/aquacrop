#' Get water contents in the evaporation layer
#' @param Soil properties of soil
#' @param InitCond Crop setting initial structure
#' @param wevap watwe evaporation
#' @return \code{Wevap} for a n time-step.
#' @examples
#' EvapLayerWaterContent(InitCond, Soil, Wevap)


EvapLayerWaterContent <- function(InitCond, Soil, Wevap){


  ## Determine actual water content (mm) ##
  # Find soil compartments covered by evaporation layer
  comp_sto <- sum(Soil$Comp$dzsum < InitCond$EvapZ)+1
  # Initialise variables
  Wevap$Act <- 0
  Wevap$Sat <- 0
  Wevap$Fc <- 0
  Wevap$Wp <- 0
  Wevap$Dry <- 0

  for (ii in 1:comp_sto){
      # Specify layer number
      layeri <- Soil$Comp$Layer[ii]
      # Determine fraction of soil compartment covered by evaporation layer
      if (Soil$Comp$dzsum[ii] > InitCond$EvapZ){
          factor <- 1-((Soil$Comp$dzsum[ii]-InitCond$EvapZ)/Soil$Comp$dz[ii])
      } else {
          factor <- 1
      }
      # Actual water storage in evaporation layer (mm)
      Wevap$Act <- Wevap$Act+(factor*1000*InitCond$th[ii]*Soil$Comp$dz[ii])
      # Water storage in evaporation layer at saturation (mm)
      Wevap$Sat <- Wevap$Sat+(factor*1000*Soil$Layer$th_s[layeri]*Soil$Comp$dz[ii])
      # Water storage in evaporation layer at field capacity (mm)
      Wevap$Fc <- Wevap$Fc+(factor*1000*Soil$Layer$th_fc[layeri]*Soil$Comp$dz[ii])
      # Water storage in evaporation layer at permanent wilting point (mm)
      Wevap$Wp <- Wevap$Wp+(factor*1000*Soil$Layer$th_wp[layeri]*Soil$Comp$dz[ii])
      # Water storage in evaporation layer at air dry (mm)
      Wevap$Dry <- Wevap$Dry+(factor*1000*Soil$Layer$th_dry[layeri]*Soil$Comp$dz[ii])
  }

  if (Wevap$Act < 0){
      Wevap$Act <- 0
  }


  return(Wevap)
}

