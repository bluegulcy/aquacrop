#' How to use Parameter Optimisation
#' @export
#' @format These are the steps to use parameter optimisation:
#' \describe{
#' \item{First}{Load observed data which should be a n X m matrix were columns 
#' area parameters and rows are observations at a given time point. The following 
#' parameters must be provided per observation:} 
#' \describe{
#' \item{day}{day dd}
#' \item{month}{month mm}
#' \item{year}{year yyy}
#' \item{obsbio}{Biomass (g m-2)}
#'}
#' \item{Second}{Create all the input files as specified in the 
#' \link{ReadFileLocations} section.}
#' \item{Third}{Use the ReadFileLocations() function to load your files into 
#' the model (e.g. F <- ReadFileLocations('FileLocation.xml'))}
#' \item{Forth}{Use the Initialise() function to initialise your variables. 
#' Refer to the \link{Initialise} section to get familiar with the function
#' (e.g. I <- Initialise(F))}
#' \item{Fifth}{Set optimiser parameters. AquaCropR uses DEoptim for parameter 
#' optimisation. Please refer to DEoptim's manual 
#' (https://cran.r-project.org/web/packages/DEoptim/DEoptim.pdf) 
#' to learn to setup optimiser parameters.}
#' \item{Sixth}{Parameter boundaries. A n X 2 matrix should be provided where 
#' rows are parameters (currently CCx and GCG only) and col1 and col2 are 
#' parameters' lower and upper boundaries. Matrix's rownames should be the name 
#' of the parameters to be optimised. See example below. NOTE: make sure you
#' use the correct parameter's name as indicated earlier.}
#' \item{Seventh}{Use the SetOptimiser() function to perform the optimisation
#' Refer to the \link{SetOptimiser} section to get familiar with the function
#' (e.g. O <- SetOptimiser(param_array, InitialiseStruct, emp_data, control)). 
#' SetOptimiser will output the optimised parameters.}
#' \item{Eight}{Use optimised parameters in your Crop.xml file}
#' }
#' @examples
#' ##Read observed data
#' emp_data <- read.csv('input_calibrate/test_output.csv', header = TRUE)

#' ##Set folder where files are located
#' folder_name <- dir(pattern='input_cali*')

#' ##Read file locations
#' FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', 
#'                                       sep=''))
#' ##Initialise structure
#' InitialiseStruct <- Initialise(FileLocation)


#' ##Set optimisation parameters
#' control = DEoptim.control(itermax = 5)

#' ##Set parameters boundaries (lower and upper limits)
#' CGC <- c(0.012494, 0.012494)
#' CCx <- c(0.94, 0.94)

#' param_array <- rbind(CGC, CCx)

#' ##Call optimiser
#' opt_par <- SetOptimiser(param_array, InitialiseStruct, emp_data, control)
#' print(opt_par)

#' 




AboutUsingParameterOptimisation <- function() { 
    
}

