#' Compute additional variables needed to run AOS
#' @param ParamStruct Crop parameters structure
#' @param FileLocation list with file locations
#' @param ClockStruct crop calendar
#' @return \code{ParamStruct}.
#' @examples
#' ReadIrrigationManagement(ParamStruct, FileLocation, ClockStruct)

ReadIrrigationManagement <- function(ParamStruct, FileLocation, ClockStruct){

# Function to read and initialise irrigation management parameters


  Location <- FileLocation[['Input']]

  ## Read irrigation management input files ##
  # Check for number of crop types
  Crops <- names(ParamStruct[['Crop']])
  nCrops <- length(Crops)
  # Create blank structure
  IrrMngtStruct <- list()

  for(ii in 1:nCrops){
      # Open file
      filename <- paste(Location, ParamStruct$Crop[[ii]]$IrrigationFile, sep ='')
      DataArray <- check_xml_exist(filename)
      DataArray <- convert_list_2numeric(DataArray, names(DataArray)[-1])

      # Create and assign numeric variables
      IrrMngtStruct[[Crops[[ii]]]] <- DataArray

      # Consolidate soil moisture targets in to one variable
      IrrMngtStruct[[Crops[[ii]]]][['SMT']] <- c(IrrMngtStruct[[Crops[[ii]]]]$SMT1,
                                                 IrrMngtStruct[[Crops[[ii]]]]$SMT2,
                                                 IrrMngtStruct[[Crops[[ii]]]]$SMT3,
                                                 IrrMngtStruct[[Crops[[ii]]]]$SMT4)

      # If specified, read input irrigation time-series
      if(IrrMngtStruct[[Crops[[ii]]]]$IrrMethod == 3){
          # Load data

          filename <- paste(Location, DataArray[['IrrSchFilename']], sep ='')
          DataArray <- check_file_exist(filename)

          # Extract data
          IrrEvents <- DataArray$irrigation
          # Convert dates to serial date format
          IrrDates <- paste(DataArray$year, DataArray$month, DataArray$day, sep='-')
          IrrDates <- as.vector(sapply(IrrDates, function(x) as_datenum(as.Date(x, "%Y-%m-%d"))))
          # Create full time series
          StartDate <- ClockStruct$SimulationStartDate
          EndDate <- ClockStruct$SimulationEndDate
          Dates <- StartDate:EndDate
          Irr <- rep(0, length(Dates))
          idx <- Dates %in% IrrDates
          Irr[idx == TRUE] <- IrrEvents
          IrrigationSch <- cbind(Dates, Irr)
          IrrMngtStruct[[Crops[[ii]]]]$IrrigationSch <- IrrigationSch
      }
  }

  return(IrrMngtStruct)
}

