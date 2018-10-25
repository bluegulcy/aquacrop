#' Read input files and initialise model clock parameters
#' @param FileLocation list with file locations
#' @return \code{ClockStruct} crop calendar.
#' @examples
#' ReadClockParameters(FileLocation)


ReadClockParameters <- function(FileLocation){
  #

  ## Read input file location ##
  Location <- FileLocation[['Input']]

  ## Read clock parameter input file ##
  filename <- paste(Location, FileLocation[['ClockFilename']], sep='')
  ClockStruct <- check_xml_exist(filename)

  ## Define clock parameters ##
  # Initialise time step counter
  ClockStruct$TimeStepCounter <- 1
  # Initialise model termination condition
  ClockStruct$ModelTermination <- FALSE
  # Simulation start time as serial date number
  DateStaV <- as.Date(ClockStruct$SimulationStartTime, format="%Y-%m-%d")
  if(is.na(DateStaV)) { print(paste('Error, revise date format:', 
                                    ClockStruct$SimulationStartTime, sep = ' '))
    break}
  
  ClockStruct$SimulationStartDate <- as_datenum(DateStaV)
  # Simulation end time as serial date number
  DateStoV <- as.Date(ClockStruct$SimulationEndTime,format="%Y-%m-%d")
  if(is.na(DateStoV)) { print(paste('Error, revise date format:', 
                                    ClockStruct$SimulationEndTime, sep = ' '))
    break}
  
  ClockStruct$SimulationEndDate <- as_datenum(DateStoV)
  # Time step (years)
  ClockStruct$TimeStep <- 1
  # Total numbers of time steps (days)
  ClockStruct$nSteps <- ClockStruct$SimulationEndDate -
    ClockStruct$SimulationStartDate
  # Time spans
  TimeSpan <- matrix(NA, 1,ClockStruct$nSteps+1)
  TimeSpan[1] <- ClockStruct$SimulationStartDate
  TimeSpan[length(TimeSpan)] <- ClockStruct$SimulationEndDate
  for (ss in 2:ClockStruct$nSteps){
    TimeSpan[ss] <- TimeSpan[ss-1]+1
  }
  ClockStruct$TimeSpan <- TimeSpan
  # Time at start of current time step
  ClockStruct$StepStartTime <- ClockStruct$TimeSpan[ClockStruct$TimeStepCounter]
  # Time at end of current time step
  ClockStruct$StepEndTime <- ClockStruct$TimeSpan[ClockStruct$TimeStepCounter+1]
  # Number of time-steps (per day) for soil evaporation calculation
  ClockStruct$EvapTimeSteps <- 20

  return(ClockStruct)
}


