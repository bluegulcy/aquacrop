#' Read input file and initialise groundwater table parameters
#' @param FileLocation list with file locations
#' @param ClockStruct crop calendar
#' @return \code{GwStruct} water table.
#' @export
#' @examples
#' ReadGroundwaterTable(FileLocation, ClockStruct)

ReadGroundwaterTable <- function(FileLocation, ClockStruct){

    
    
    ## Read input file location ##
    Location <- FileLocation[['Input']]
    
    ## Define empty structure ##
    GwStruct <- list()
    
    ## Read groundwater table input file ##
    # Open file
    filename <- paste(Location, FileLocation[['GroundwaterFilename']], sep='')
    DataArray <- check_xml_exist(filename)
    
    
    WT <- DataArray[['watertable_present']]
    Method <- DataArray[['watertable_method']]
    if(WT == 'N'){
        # No water table present (don't read the rest of the input file)
        GwStruct[['WaterTable']] <- 0
        
    } else if (WT =='Y'){
        # Water table is present
        GwStruct[['WaterTable']] <- 1
        GwStruct[['Method']] <- Method
        # Load and extract data
        GwDates <- DataArray$info$cd$Date
        GwDates <- as.numeric(sapply(GwDates, function(x)
            as_datenum(as.Date(x, "%Y-%m-%d"))))
        GwDepths <- as.numeric(DataArray$info$cd$Depth)
        nGwDates <- length(GwDates)
        # Process and interpolate data
        StaDate <- ClockStruct[['SimulationStartDate']]
        EndDate <- ClockStruct[['SimulationEndDate']]
        Dates <- StaDate:EndDate
        nDates <- length(Dates)
        if (length(GwDepths) == 1){
            # Only one value so water table elevation is constant for full
            # simulation period
            GwStruct[['zGW']] <- cbind(Dates, rep(1, nDates) * GwDepths[1])
        } else if (length(GwDepths) > 1){
            if (Method == 'Constant'){
                # No interpolation between dates
                staid <- 1
                GwLev <- rep(0, nDates)
                for (ii in 1:nGwDates){
                    stoid <- which(Dates == GWDates[ii])
                    GwLev[staid:stoid] <- GwDepths[ii]
                    staid <- stoid + 1
                }
                if (staid <= nDates){
                    GwLev[staid:nDates] <- GwLev[staid-1]
                }
            } else if (Method == 'Interp'){
                # Linear interpolation between dates
                # Add start and end points (if they do not exist in input
                # values)
                if((GwDates == StaDate) == FALSE){
                    GwDates <- c(as.numeric(StaDate), as.numeric(GwDates))
                    #' FIXME there is abug here
                    m <- which(GwDates == min(GwDates))
                    GwDepths <- rbind(GwDepths[m], GwDepths)
                }
                if((GwDates == EndDate) == FALSE){
                    GwDates <- c(GwDates,EndDate)
                    m <- which(GwDates == max(GwDates))
                    GwDepths <- c(GwDepths, GwDepths[m])
                }
                # Interpolate daily groundwater depths
                GwLev <- interp1(GwDates,GwDepths, Dates)
            }
            # Assign values to output structure
            GwStruct[['zGW']] <- cbind(Dates, GwLev)
        }
    }
    
    return(GwStruct)
}
