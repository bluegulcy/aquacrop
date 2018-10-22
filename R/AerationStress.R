#' Calculate aeration stress coefficient
#' @param Crop Parameters for a given crop
#' @param InitCond Crop setting initial structure
#' @param thRZ aeration stress (root zone)
#' @return list with \code{NewCond} and \code{Ksa} aeration stress coefficient for a n time-step.
#' @examples
#' AerationStress(Crop, InitCond, thRZ)


AerationStress <- function(Crop, InitCond, thRZ){


    ## Store initial conditions in new structure for updating ##
    cc <- list()
    Ksa <- list()
    NewCond <- InitCond

    ## Determine aeration stress (root zone) ##
    if (thRZ$Act > thRZ$Aer){
        # Calculate aeration stress coefficient
        if (NewCond$AerDays < Crop$LagAer){
            stress <- 1-((thRZ$Sat-thRZ$Act)/(thRZ$Sat-thRZ$Aer))
            Ksa$Aer <- 1-((NewCond$AerDays/3)*stress)
        } else if (NewCond$AerDays >= Crop$LagAer){
            Ksa$Aer <- (thRZ$Sat-thRZ$Act)/(thRZ$Sat-thRZ$Aer)
        }
        # Increment aeration days counter
        NewCond$AerDays <- NewCond$AerDays+1
        if (NewCond$AerDays > Crop$LagAer){
            NewCond$AerDays <- Crop$LagAer
        }
    } else {
        # Set aeration stress coefficient to one (no stress value)
        Ksa$Aer <- 1
        # Reset aeration days counter
        NewCond$AerDays <- 0
    }

    cc$Ksa <- Ksa
    cc$NewCond <- NewCond

    return(cc)

}
