#' Function to initialise AquaCropR
#' @param filename An xml file with locations
#' @return list with \code{FileLocation}.
#' @examples
#' ReadFileLocations('dummy.xml')


Initialise <- function(FileLocation){


  ## Get file locations ##


  ## Define model run time ##
  ClockStruct <- ReadClockParameters(FileLocation)

  ## Read climate data ##
  WeatherStruct <-  ReadWeatherInputs(FileLocation, ClockStruct)

  ## Read model parameter files ##
  # FIXME change strings to numeric
  MP <- ReadModelParameters(FileLocation, ClockStruct)
  CropChoices <- MP[['CropChoices']]
  ParamStruct <- MP[['ParamStruct']]
  ClockStruct <- MP[['ClockStruct']]

  ## Read irrigation management file ##
  IrrMngtStruct <- ReadIrrigationManagement(ParamStruct, FileLocation, ClockStruct)


  ## Read field management file ##
  FieldMngtStruct <- ReadFieldManagement(FileLocation)

  ## Read groundwater table file ##
  GwStruct <- ReadGroundwaterTable(FileLocation, ClockStruct)

  ## Compute additional variables ##
  ParamStruct <- ComputeVariables(ParamStruct, WeatherStruct,
                      ClockStruct, GwStruct, CropChoices, FileLocation)

  ####
  ## Define initial conditions ##
  InitCondStruct <- ReadModelInitialConditions(ParamStruct, GwStruct,
      FieldMngtStruct, CropChoices, FileLocation, ClockStruct)


  ## Pack output structure ##
  InitialiseStruct <- list()
  InitialiseStruct$Parameter <- ParamStruct
  InitialiseStruct$IrrigationManagement <- IrrMngtStruct
  InitialiseStruct$FieldManagement <- FieldMngtStruct
  InitialiseStruct$Groundwater <- GwStruct
  InitialiseStruct$InitialCondition <- InitCondStruct
  InitialiseStruct$CropChoices <- CropChoices
  InitialiseStruct$WeatherStruct <- WeatherStruct
  InitialiseStruct$FileLocation <- FileLocation
  InitialiseStruct$ClockStruct <- ClockStruct

  ## Setup output files ##
  # Define output file location
  FileLoc <- FileLocation$Output
  # Setup blank matrices to store ouLatputs
  InitialiseStruct$Outputs$WaterContents <-
    matrix(0, length(ClockStruct$TimeSpan), 5+ParamStruct$Soil$nComp)

  InitialiseStruct$Outputs$WaterFluxes <- 
    matrix(0, length(ClockStruct$TimeSpan), 19)
  
  InitialiseStruct$Outputs$CropGrowth <- 
    matrix(0, length(ClockStruct$TimeSpan), 17)
  
  InitialiseStruct$Outputs$FinalOutput <- 
    matrix(NA, ClockStruct$nSeasons, 8)
  # Store dates in daily matrices
  Dates <- t(sapply(ClockStruct$TimeSpan, function(x)
    as.vector(as.numeric(unlist(as_date_list(x, '0000-01-01'))))))
  InitialiseStruct$Outputs$WaterContents[,1:3] <- Dates[,1:3]
  InitialiseStruct$Outputs$WaterFluxes[,1:3] <- Dates[,1:3]
  InitialiseStruct$Outputs$CropGrowth[,1:3] <- Dates[,1:3]


  return(InitialiseStruct)

}


