
#' Function to check and declare model termination
#' @param ClockStruct the Clock list
#' @param InitialiseStruct the crop initial conditions in list format
#' @return \code{ClockStruct}.
#' @export
#' @examples
#' CheckModelTermination(ClockStruct, InitialiseStruct)

CheckModelTermination <- function(ClockStruct, InitialiseStruct){
#



    ## Check if current time-step is the last
    CurrentTime <- ClockStruct$StepEndTime
    if (CurrentTime < ClockStruct$SimulationEndDate){
        ClockStruct$ModelTermination <- FALSE
    } else if (CurrentTime >= ClockStruct$SimulationEndDate){
        ClockStruct$ModelTermination <- TRUE
    }

    ## Check if at the end of last growing season ##
    # Allow model to exit early if crop has reached maturity or died, and in
    # the last simulated growing season
    if(InitialiseStruct$InitialCondition$HarvestFlag == TRUE &
                ClockStruct$SeasonCounter == ClockStruct$nSeasons){
            ClockStruct$ModelTermination <- TRUE
    }


    return(ClockStruct)

}

