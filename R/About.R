#' About how to run the simulation
#' @format These are the steps to run your simulation:
#' \describe{
#' \item{First}{Create all the input files as specified in the 
#' \link{ReadFileLocations} section.}
#' \item{Second}{Use the ReadFileLocations() function to load your files into 
#' the model (e.g. F <- ReadFileLocations('FileLocation.xml'))}
#' \item{Third}{Use the Initialise() function to initialise your variables. 
#' Refer to the \link{Initialise} section to get familiar with the function
#' (e.g. I <- Initialise(F))}
#' \item{Fourth}{Use the PerformTimeStep() function to perform the simulation.
#' Refer to the \link{PerformTimeStep} section to get familiar with the function
#' (e.g. O <- PerformTimeStep(I)). PerformTimeStep will output all the 
#' variables create by the model in the form of a list. Refer to 
#' \link{OutputParameters} for more information about these variables}
#' 
#' 
#' }
#' @examples
#' F <- ReadFileLocations('FileSetup.xml')
#' I <- Initialise(F)
#' O <- PerformTimeStep(I)
#' names(O)
#' 
#' 
#' 
#' Example of FileSetup.xml file:
#' 
#' <?xml version="1.0"?>
#'<FileSetup>
#'<Input>input/</Input>
#'<WeatherFilename>Weather.csv</WeatherFilename>
#'<CO2Filename>MaunaLoaCO2.csv</CO2Filename>
#'<ClockFilename>Clock.xml</ClockFilename>
#'<CropRotationFilename>CropRotation.xml</CropRotationFilename>
#'<FieldManagementFilename>FieldManagement.xml</FieldManagementFilename>
#'<InitialWCFilename>InitialWaterContent.xml</InitialWCFilename>
#'<GroundwaterFilename>WaterTable.xml</GroundwaterFilename> 
#'<SoilFilename>Soil.xml</SoilFilename>
#'<CropRotationCalendarFilename>CropRotationCalendar.xml</CropRotationCalendarFilename>
#'</FileSetup>



About <- function() { 
    
}

