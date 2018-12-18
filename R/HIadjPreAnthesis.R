#' Function to calculate adjustment to harvest index for pre-anthesis water
#' stress
#' @param Crop Parameters for a given crop
#' @param InitCond Crop setting initial structure
#' @return \code{NewCond} for a n time-step.
#' @export
#' @examples
#' HIadjPreAnthesis(InitCond, Crop)


HIadjPreAnthesis <- function(InitCond, Crop){


    ## Store initial conditions in structure for updating ##
    NewCond <- InitCond

    ## Calculate adjustment ##
    # Get parameters
    Br <- InitCond$B/InitCond$B_NS
    Br_range <- log(Crop$dHI_pre)/5.62
    Br_upp <- 1
    Br_low <- 1-Br_range
    Br_top <- Br_upp-(Br_range/3)

    # Get biomass ratios
    ratio_low <- (Br-Br_low)/(Br_top-Br_low)
    ratio_upp <- (Br-Br_top)/(Br_upp-Br_top)

    # Calculate adjustment factor
    if (Br >= Br_low & Br < Br_top){
        NewCond$Fpre <- 1+(((1+sin((1.5-ratio_low)*pi))/2)*(Crop$dHI_pre/100))
    } else if (Br > Br_top & Br <= Br_upp){
        NewCond$Fpre <- 1+(((1+sin((0.5+ratio_upp)*pi))/2)*(Crop$dHI_pre/100))
    } else {
        NewCond$Fpre <- 1
    }

    if (NewCond$CC <= 0.01){
        # No green canopy cover left at start of flowering so no harvestable
        # crop will develop
        NewCond$Fpre <- 0
    }


    return(NewCond)
}

