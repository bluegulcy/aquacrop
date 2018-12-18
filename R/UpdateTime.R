#' Update current time in model
#' @param InitialiseStruct Crop setting initial structure
#' @param ClockStruct crop calendar
#' @return list with \code{InitialiseStruct} and \code{ClockStruct} for a n time-step
#' @export
#' @examples
#' UpdateTime(ClockStruct, InitialiseStruct)

UpdateTime <- function(ClockStruct, InitialiseStruct){


    ## Define global variables ##

    cc <- list()

    ## Update time ##
    if(ClockStruct$ModelTermination == FALSE){
        if(InitialiseStruct$InitialCondition$HarvestFlag == TRUE &
                ClockStruct$OffSeason == 'N'){
            # End of growing season has been reached and not simulating
            # off-season soil water balance. Advance time to the start of the
            # next growing season.
            # Check if in last growing season
            if(ClockStruct$SeasonCounter < ClockStruct$nSeasons){
                # Update growing season counter
                ClockStruct$SeasonCounter <- ClockStruct$SeasonCounter+1
                # Update time-step counter
                ClockStruct$TimeStepCounter <- which(ClockStruct$TimeSpan ==
                    ClockStruct$PlantingDate[ClockStruct$SeasonCounter])
                # Update start time of time-step
                ClockStruct$StepStartTime <-
                    ClockStruct$TimeSpan[ClockStruct$TimeStepCounter]
                # Update end time of time-step
                ClockStruct$StepEndTime <-
                    ClockStruct$TimeSpan[ClockStruct$TimeStepCounter+1]
                # Reset initial conditions for start of growing season
                InitialiseStruct <- ResetInitialConditions(InitialiseStruct, ClockStruct)
            }
        } else {
            # Simulation considers off-season, so progress by one time-step
            # (one day)
            # Time-step counter
            ClockStruct$TimeStepCounter <- ClockStruct$TimeStepCounter+1
            # Start of time step (beginning of current day)
            ClockStruct$StepStartTime <-
                ClockStruct$TimeSpan[ClockStruct$TimeStepCounter]
            # End of time step (beginning of next day)
            ClockStruct$StepEndTime <-
                ClockStruct$TimeSpan[ClockStruct$TimeStepCounter+1]
            # Check if in last growing season
            if(ClockStruct$SeasonCounter < ClockStruct$nSeasons){
                # Check if upcoming day is the start of a new growing season
                if (ClockStruct$StepStartTime ==
                        ClockStruct$PlantingDate[ClockStruct$SeasonCounter+1]){
                    # Update growing season counter
                    ClockStruct$SeasonCounter <- ClockStruct$SeasonCounter+1
                    # Reset initial conditions for start of growing season
                    InitialiseStruct <- ResetInitialConditions(InitialiseStruct, ClockStruct)
                }
            }
        }
    } else if (ClockStruct$ModelTermination == TRUE){
        ClockStruct$StepStartTime <- ClockStruct$StepEndTime
        ClockStruct$StepEndTime <- ClockStruct$StepEndTime+1
    }

    cc$ClockStruct <- ClockStruct
    cc$InitialiseStruct <- InitialiseStruct

    return(cc)

}
