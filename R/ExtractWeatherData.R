#' Extract weather data for current time step.
#' @param InitialiseStruct Crop setting initial structure
#' @param ClockStruct crop calendar
#' @return list with \code{Weather} for n time-step
#' @export
#' @examples
#' ExtractWeatherData(ClockStruct, InitialiseStruct)

ExtractWeatherData <- function(InitialiseStruct){

    Weather = list()
    ## Extract weather dataset ##
    WeatherDB <- InitialiseStruct$WeatherStruct

    ## Extract weather data for current time step ##
    # Get current date
    Date <- InitialiseStruct$ClockStruct$StepStartTime
    # Find row corresponding to the current date in dataset
    Row <- WeatherDB[,1] == Date
    if(length(which(Row == TRUE)) == 0){
        print(paste('Error, there is not weather data for ', as_date(Date)))
        break
    }
    
    
    # Get weather variables
    Weather$MinTemp <- WeatherDB[Row, 2]
    Weather$MaxTemp <- WeatherDB[Row, 3]
    Weather$Precipitation <- WeatherDB[Row, 4]
    Weather$ReferenceET <- WeatherDB[Row,5]
    Weather$Dates <- WeatherDB[Row,1]
    
    if(length(Weather[['Dates']]) > 1){
        print(paste('Error, weather data are duplicated for', as_date(Date)))
        break
    }

    return(Weather)
}

