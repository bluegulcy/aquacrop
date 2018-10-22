#' Read and process input weather time-series
#' @param FileLocation file locations
#' @param ClockStruct list time
#' @return list with \code{WeatherDB}.
#' @examples
#' ReadWeatherInputs(FileLocation, ClockStruct)
ReadWeatherInputs <- function(FileLocation, ClockStruct){


  ## Read input file location ##
  Location <- FileLocation$Input

  ## Read weather data inputs ##
  # Open file
  filename <- paste(Location, FileLocation$WeatherFilename, sep='')
  Data <- check_file_exist(filename)



  ## Convert dates to serial date format ##
  
  Dates <- apply(Data[, c(3,2,1)], 1, FUN=as_datenum_string)
  
  
  ## Extract data ##
  Tmin <- Data[['mintp']]
  Tmax <- Data[['mxntp']]
  P <- Data[['p']]
  Et0 <- Data[['evp']]

  ## Extract data for simulation period ##
  # Find start and end dates
  StartDate <- ClockStruct$SimulationStartDate
 
  EndDate <- ClockStruct$SimulationEndDate
  
  StartRow <- which(Dates == StartDate)
  if(length(StartRow) == 0){
    
    print(paste('Error, there no weather at the beginning of the simulation: ', 
                as_date(ClockStruct$SimulationStartDate) ))
    break
    
  }
  EndRow <- which(Dates == EndDate)
  if(length(EndRow) == 0){
    
    print(paste('Error, there no weather until the end of the simulation: ', 
                as_date(ClockStruct$SimulationEndDate) ))
    break
    
  }

  # Store data for simulation period
  WeatherDB <- data.frame(Dates=Dates[StartRow:EndRow],Tmin=Tmin[StartRow:EndRow],
                          Tmax=Tmax[StartRow:EndRow],P=P[StartRow:EndRow],
                          Et0=Et0[StartRow:EndRow])

  return(WeatherDB)

}
