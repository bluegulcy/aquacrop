#' Calculate number of growing degree days on current day
#' @param InitCond Crop setting initial structure
#' @param Crop Parameters for a given crop
#' @param GrowingSeason crop developmental stage
#' @return \code{NewCond}  for a n time-step.
#' @examples
#' GrowthStage(Crop, InitCond, GrowingSeason)

GrowthStage <- function(Crop, InitCond, GrowingSeason){


    ## Store initial conditions in new structure for updating ##
    NewCond <- InitCond

    ## Get growth stage (if in growing season) ##
    if(GrowingSeason == TRUE){
        # Adjust time for any delayed growth
        if(Crop$CalendarType == 1){
            tAdj <- NewCond$DAP-NewCond$DelayedCDs
        } else if (Crop$CalendarType == 2){
            tAdj <- NewCond$GDDcum-NewCond$DelayedGDDs
        }

        # Update growth stage
        if (tAdj <= Crop$Canopy10Pct){
            NewCond$GrowthStage <- 1
        } else if (tAdj <= Crop$MaxCanopy){
            NewCond$GrowthStage <- 2
        } else if (tAdj <= Crop$Senescence){
            NewCond$GrowthStage <- 3
        } else if (tAdj > Crop$Senescence){
            NewCond$GrowthStage <- 4
        }
    } else {
        # Not in growing season so growth stage is set to dummy value
        NewCond$GrowthStage <- 0
    }

    return(NewCond)
}

