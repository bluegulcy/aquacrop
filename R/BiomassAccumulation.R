#' Calculate biomass accumulation (g m-2)
#' @param Crop Parameters for a given crop
#' @param InitCond Crop setting initial structure
#' @param Tr actual transpiration
#' @param TrPot potential transpiration
#' @param Et0 Evapotranspiration
#' @param Tmax max temp for n time-step
#' @param Tmin min temp for n time-step
#' @param GDD Growing degree days
#' @param GrowingSeason crop developmental stage
#' @return \code{NewCond} for a n time-step.
#' @export
#' @examples
#' BiomassAccumulation(Crop, InitCond, Tr, TrPot, Et0, Tmax, Tmin, GDD, GrowingSeason)


BiomassAccumulation <- function(Crop, InitCond, Tr, TrPot, Et0, Tmax, Tmin, GDD, GrowingSeason){


    ## Store initial conditions in a new structure for updating ##
    NewCond <- InitCond

    ## Calculate biomass accumulation (if in growing season) ##
    if (GrowingSeason == TRUE){
        # Calculate temperature stress
        Kst <- TemperatureStress(Crop,Tmax,Tmin,GDD)

        # Get time for harvest index build-up
        HIt <- NewCond$DAP-NewCond$DelayedCDs-Crop$HIstartCD-1

        if(((Crop$CropType == 2) | (Crop$CropType == 3)) & (NewCond$HIref > 0)){
            # Adjust WP for reproductive stage
            if (Crop$Determinant == 1){
                fswitch <- NewCond$PctLagPhase/100
            } else {
                if (HIt < (Crop$YldFormCD/3)){
                    fswitch <- HIt/(Crop$YldFormCD/3)
                } else {
                    fswitch <- 1
                }
            }
            WPadj <- Crop$WP*(1-(1-Crop$WPy/100)*fswitch)
        } else {
            WPadj <- Crop$WP
        }

        # Adjust WP for CO2 effects
        WPadj <- WPadj*Crop$fCO2

        # Calculate biomass accumulation on current day
        # No water stress
        dB_NS <- WPadj*(TrPot/Et0)*Kst$Bio
        # With water stress
        dB <- WPadj*(Tr/Et0)*Kst$Bio


        if (length(dB) <= 0){
            dB <- 0
        }

        # Update biomass accumulation
        NewCond$B <- NewCond$B+dB
        NewCond$B_NS <- NewCond$B_NS+dB_NS
    } else {
        # No biomass accumulation outside of growing season
        NewCond$B <- 0
        NewCond$B_NS <- 0
    }

    return(NewCond)

}

