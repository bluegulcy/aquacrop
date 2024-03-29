#' Calculate reference (no adjustment for stress effects)
#' harvest index on current day
#' @param InitCond Crop setting initial structure
#' @param Crop Parameters for a given crop
#' @param GrowingSeason crop developmental stage
#' @return \code{NewCond} for a n time-step.
#' @export
#' @examples
#' HIrefCurrentDay(InitCond, Crop, GrowingSeason)



HIrefCurrentDay <- function(InitCond, Crop, GrowingSeason){


    ## Store initial conditions for updating ##
    NewCond <- InitCond

    ## Calculate reference harvest index (if in growing season) ##
    if (GrowingSeason == TRUE){
        # Check if in yield formation period
        if(Crop$CalendarType == 1){
            tAdj <- NewCond$DAP - NewCond$DelayedCDs
        } else if (Crop$CalendarType == 2){
            tAdj <- NewCond$GDDcum - NewCond$DelayedGDDs
        }
        if (tAdj > Crop$HIstart){
            NewCond$YieldForm <- TRUE
        } else {
            NewCond$YieldForm <- FALSE
        }

        # Get time for harvest index calculation
        HIt <- NewCond$DAP - NewCond$DelayedCDs - Crop$HIstartCD-1

        if (HIt <= 0){
            # Yet to reach time for HI build-up
            NewCond$HIref <- 0
            NewCond$PctLagPhase <- 0
        } else {
            if (NewCond$CCprev <= (Crop$CCmin*Crop$CCx)){
                # HI cannot develop further as canopy cover is too small
                NewCond$HIref <- InitCond$HIref
            } else {
                # Check crop type
                if(Crop$CropType == 1 | Crop$CropType == 2){
                    # If crop type is leafy vegetable or root/tuber, then proceed with
                    # logistic growth (i$e. no linear switch)
                    NewCond$PctLagPhase <- 100# No lag phase
                    # Calculate reference harvest index for current day
                    NewCond$HIref <- (Crop$HIini*Crop$HI0)/(Crop$HIini+
                        (Crop$HI0-Crop$HIini)*exp(-Crop$HIGC*HIt))
                    # Harvest index apprAOShing maximum limit
                    if (NewCond$HIref >= (0.9799*Crop$HI0)){
                        NewCond$HIref <- Crop$HI0
                    }
                } else if (Crop$CropType == 3){
                    # If crop type is fruit/grain producing, check for linear switch
                    if (HIt < Crop$tLinSwitch){
                        # Not yet reached linear switch point, therefore proceed with
                        # logistic build-up
                        NewCond$PctLagPhase <- 100*(HIt/Crop$tLinSwitch)
                        # Calculate reference harvest index for current day
                        # (logistic build-up)
                        NewCond$HIref <- (Crop$HIini*Crop$HI0)/(Crop$HIini+
                            (Crop$HI0-Crop$HIini)*exp(-Crop$HIGC*HIt))
                    } else {
                        # Linear switch point has been reached
                        NewCond$PctLagPhase <- 100
                        # Calculate reference harvest index for current day
                        # (logistic portion)
                        NewCond$HIref <- (Crop$HIini*Crop$HI0)/(Crop$HIini+
                            (Crop$HI0-Crop$HIini)*exp(-Crop$HIGC*Crop$tLinSwitch))
                        # Calculate reference harvest index for current day
                        # (total - logistic portion + linear portion)
                        NewCond$HIref <- NewCond$HIref + (Crop$dHILinear*
                            (HIt-Crop$tLinSwitch))
                    }

                }
                # Limit HIref and round off computed value
                if (NewCond$HIref > Crop$HI0){
                    NewCond$HIref <- Crop$HI0
                    #print(paste('1', NewCond$HIref))
                } else if (NewCond$HIref <= (Crop$HIini+0.004)){
                    NewCond$HIref <- 0
                    #print(paste('2', NewCond$HIref))
                } else if ((Crop$HI0-NewCond$HIref) < 0.004){
                    NewCond$HIref <- Crop$HI0
                    #print(paste('3', NewCond$HIref))
                }
            }
        }
    } else {
        # Reference harvest index is zero outside of growing season
        NewCond$HIref <- 0
    }

    return(NewCond)
}

