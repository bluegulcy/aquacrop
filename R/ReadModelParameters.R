#' Read input files and initialise soil and crop parameters
#' @param FileLocation list with file locations
#' @param ClockStruct crop calendar
#' @return list with uodated \code{ClockStruct} \code{ParamStruct} and \code{CropChoices}.
#' @examples
#' ReadModelParameters(FileLocation, ClockStruct)

ReadModelParameters <- function(FileLocation, ClockStruct){
    

    ParamStruct <- list()
    ## Read input file location ##
    Location <- FileLocation[['Input']]

    ## Read soil parameter input file ##
    # Open file
    filename <- paste(Location, FileLocation[['SoilFilename']], sep='')
    ParamStruct$Soil <- check_xml_exist(filename)
    ParamStruct$Soil <- convert_list_2numeric(ParamStruct$Soil, 
                                              names(ParamStruct$Soil)[-1:-3])
    ## Read soil profile input file ##
    # Open file
    if(ParamStruct$Soil[['SoilProfileFilename']] != 'NA'){
        filename <- paste(Location, ParamStruct$Soil[['SoilProfileFilename']], sep='')
        SoilProfile <- check_xml_table_exist(filename)
        # Create vector of soil compartments sizes and associated layers
        ParamStruct$Soil$Comp$dz <- conver2num(SoilProfile[['Thickness.m.']])
        ParamStruct$Soil$Comp$dzsum <- c(100*cumsum(ParamStruct$Soil$Comp$dz)/100)
        ParamStruct$Soil$Comp$Layer <- conver2num(SoilProfile[['LayerNo']])
    }
    
    ## Read crop mix input file ##
    filename <- paste(Location, FileLocation[['CropRotationFilename']], sep='')
    CropMix <- check_xml_exist(filename)
    #Number of crops
    nCrops <- CropMix$NumberofCropOptions
    # Crop rotation filename
    Rotation <- CropMix$SpecifiedPlantingCalendar
    # Crop rotation filename
    RotationFilename <- FileLocation$Rotationfilename
    # Crop information (type and filename)
    CropInfo  <- CropMix$CropInfo


    ## Read crop parameter input files ##
    # Create blank structure for multiple crops
    ParamStruct$Crop <- list()
    # FIXME: quantities are presented in character form
    for (ii in 1:nCrops){

        # Open file

        filename <- paste(Location, CropInfo[[ii]]$CropFilename, sep='')
        CropStruct <- check_xml_exist(filename)
        CropStruct <- convert_list_2numeric(CropStruct,
                        names(CropStruct)[c(-match(c('PlantingDate',
                           'HarvestDate'), names(CropStruct)))])

        # Create crop parameter structure
        ParamStruct$Crop[[CropInfo[[ii]]$croptype]] <- CropStruct
        ParamStruct$Crop[[CropInfo[[ii]]$croptype]]$IrrigationFile <- 
            CropInfo[[ii]]$IrrigationFilename

    }

    ## Find planting and harvest dates ##
    if(nCrops > 1 | Rotation =='Y'){
        # Crop rotation occurs during the simulation period
        # Open rotation time-series file
        filename <- paste(Location, FileLocation$CropRotationCalendarFilename, sep='')

        # Load data
        DataArray <- check_file_exist(filename)
        # Extract data
        # Cnage strings to Date format
        PlantDates <- as_datenum(as.Date(DataArray$PlantDate, "%d/%m/%Y"))
        HarvestDates <- as_datenum(as.Date(DataArray$HarvestDate, "%d/%m/%Y"))
        CropChoices <- DataArray$Crop


    } else if (nCrops == 1){
        # Only one crop type considered during simulation - i.e. no rotations
        # either within or between years
        # Get start and end years for full simulation

        SimStaDate <- as.numeric(unlist(strsplit(ClockStruct[['SimulationStartTime']], '-')))
        SimEndDate <- as.numeric(unlist(strsplit(ClockStruct[['SimulationEndTime']], '-')))
        # Get crop structure
        CropTemp <- ParamStruct$Crop[[1]]
        # FIXME Does growing season extend across multiple calendar years
        # It assumes 
        
        DOYP = strftime(as.Date(paste(CropTemp[['PlantingDate']],  '2000', sep ='/'), 
                                "%d/%m/%Y"), "%j" )
        DOYH = strftime(as.Date(paste(CropTemp[['HarvestDate']],  '2000', sep ='/'), 
                                "%d/%m/%Y"), "%j" )
        if(DOYP < DOYH){
          
                YrsPlant <- SimStaDate[1]:SimEndDate[1]
                YrsHarvest <- YrsPlant
         
        } else {
            YrsSim <- SimStaDate[1]:SimEndDate[1]
            YrsPlant <- YrsSim[seq(1, length(YrsSim)-1, by = 1)]
            YrsHarvest <- YrsSim[seq(2, length(YrsSim), by = 1)]
        }
        # Correct for partial first growing season (may occur when simulating
        # off-season soil water balance)
        ocrop = as.Date(format(as.Date(paste(CropTemp[['PlantingDate']], '/',
                                YrsPlant[1], sep=''),
                           format="%d/%m/%Y"), "%Y-%m-%d"))

        # Simulation Start Date
        if(ocrop < ClockStruct[['SimulationStartTime']]) {
            print(paste('Planting date', ocrop, 'is ealier than simulation start', 
                        ClockStruct[['SimulationStartTime']], sep = ' '))
            break
            YrsPlant <- YrsPlant[2:length(YrsPlant)]
            YrsHarvest <- YrsHarvest[2:length(YrsHarvest)]
        }
        # Define blank variables
        PlantDates <- rep(0, length(YrsPlant))
        HarvestDates <- rep(0, length(YrsHarvest))
        CropChoices <- c()
        # Determine planting and harvest dates
        for (ii in 1:length(YrsPlant)){

            # FIXME put dates not numbers
            PlantDates[ii] <- as_datenum(as.Date(paste(CropTemp[['PlantingDate']],'/',
                                                       YrsPlant[ii], sep=''),  "%d/%m/%Y"))
            HarvestDates[ii] <- as_datenum(as.Date(paste(CropTemp[['HarvestDate']],'/',
                                                         YrsHarvest[ii], sep=''),  "%d/%m/%Y"))
            # FIXME is doesn't loop across crop types
            CropChoices[ii] <- CropInfo[[1]]$croptype
        }
    }

    ## Update clock parameters ##
    # Store planting and harvest dates
    ClockStruct[['PlantingDate']] <- PlantDates
    ClockStruct[['HarvestDate']] <- HarvestDates
    ClockStruct[['nSeasons']] <- length(PlantDates)
    # Initialise growing season counter
    if(ClockStruct[['StepStartTime']] == ClockStruct[['PlantingDate']][1]){
        ClockStruct[['SeasonCounter']] <- 1
    } else {
        ClockStruct[['SeasonCounter']] <- 0
    }


    cc <- list()
    cc[['ParamStruct']] <- ParamStruct
    cc[['ClockStruct']] <- ClockStruct
    cc[['CropChoices']] <- CropChoices

    return(cc)
}
