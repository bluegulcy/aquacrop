#' # Function to simulate build up of harvest index
#' @param Soil properties of soil
#' @param Crop Parameters for a given crop
#' @param InitCond Crop setting initial structure
#' @param Et0 Evapotranspiration
#' @param Tmax max temp for n time-step
#' @param Tmin min temp for n time-step
#' @param GDD Growing degree days
#' @param GrowingSeason crop developmental stage
#' @return \code{NewCond} for a n time-step.
#' @export
#' @examples
#' HarvestIndex(Soil, Crop, InitCond, Et0, Tmax, Tmin, GDD, GrowingSeason)


HarvestIndex <- function(Soil, Crop, InitCond, Et0, Tmax, Tmin, GDD, GrowingSeason){


## Store initial conditions for updating ##
    NewCond <- InitCond

    ## Calculate harvest index build up (if in growing season) ##
    if (GrowingSeason == TRUE){
        # Calculate root zone water content

        GC <- RootZoneWater(Soil,Crop,NewCond)
        Wr <- GC$Wr
        Dr <- GC$Dr
        TAW <- GC$TAW

        # Calculate water stress
        beta <- TRUE
        Ksw <- WaterStress(Crop, NewCond, Dr, TAW, Et0, beta)

        # Calculate temperature stress
        Kst <- TemperatureStress(Crop, Tmax, Tmin, GDD)

        # Get reference harvest index on current day
        HIi <- NewCond$HIref

        # Get time for harvest index build-up
        HIt <- NewCond$DAP - NewCond$DelayedCDs - Crop$HIstartCD-1

        # Calculate harvest index
        if((NewCond$YieldForm == TRUE) & (HIt >= 0)){
            # Root/tuber or fruit/grain crops
            if(Crop$CropType == 2 | Crop$CropType == 3){

                # Detemine adjustment for water stress before anthesis
                if (InitCond$PreAdj == FALSE){
                    NewCond$PreAdj <- TRUE
                    NewCond <- HIadjPreAnthesis(NewCond,Crop)
                }

                # Determine adjustment for crop pollination failure
                if (Crop$CropType == 3){# Adjustment only for fruit/grain crops
                    if(HIt > 0 & HIt <= Crop$FloweringCD){
                        NewCond <- HIadjPollination(InitCond, Crop, Ksw, Kst, HIt)
                    }
                    HImax <- NewCond$Fpol*Crop$HI0
                } else {
                    # No pollination adjustment for root/tuber crops
                    HImax <- Crop$HI0
                }

                # Determine adjustments for post-anthesis water stress
                if (HIt > 0){
                    NewCond <- HIadjPostAnthesis(NewCond,Crop,Ksw)
                }

                # Limit HI to maximum allowable increase due to pre- and
                # post-anthesis water stress combinations
                HImult <- NewCond$Fpre*NewCond$Fpost
                if (HImult > 1+(Crop$dHI0/100)){
                    HImult <- 1+(Crop$dHI0/100)
                }

                # Determine harvest index on current day, adjusted for stress
                # effects
                if (HImax >= HIi){
                    HIadj <- HImult*HIi
                } else {
                    HIadj <- HImult*HImax
                }
            } else if (Crop$CropType == 1){
                # Leafy vegetable crops - no adjustment, harvest index equal to
                # reference value for current day
                HIadj <- HIi
            }
        } else {
            # No build-up of harvest index if outside yield formation period
            HIi <- InitCond$HI
            HIadj <- InitCond$HIadj
        }
        # Store final values for current time step
        NewCond$HI <- HIi
        NewCond$HIadj <- HIadj
    } else {
        # No harvestable crop outside of a growing season
        NewCond$HI <- 0
        NewCond$HIadj <- 0
    }

    return(NewCond)

}
