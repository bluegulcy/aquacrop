#' Read input files and initialise model clock parameters
#' @param FileLocation list with file locations
#' @return \code{FieldManagement} list.
#' @examples
#' ReadFieldManagement(FileLocation)

ReadFieldManagement <- function(FileLocation){
# Function to read input files and initialise field management parameters

  ## Get input file location ##
  Location <- FileLocation[['Input']]

  ## Read field management parameter input files ##
  # Open file
  filename <- paste(Location, FileLocation[['FieldManagementFilename']],
                    sep ='')

  FieldManagement <- check_xml_exist(filename)
  FieldManagement <- convert_list_2numeric(FieldManagement)

  return(FieldManagement)
}
