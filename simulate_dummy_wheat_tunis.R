#'Function to read input and output file locations
library('XML')
library('xml2')
library('pracma')
library('kulife')
library('ggplot2')
library('reshape')



source('R/Aqua_library.R')
source('R/ReadFileLocations.R')
source('R/ReadWeatherInputs.R')
source('R/ReadClockParameters.R')
source('R/ReadFieldManagement.R')
source('R/ReadModelParameters.R')
source('R/ReadIrrigationManagement.R')
source('R/ReadGroundwaterTable.R')
source('R/ComputeVariables.R')
source('R/ComputeCropCalendar.R')
source('R/CalculateHILinear.R')
source('R/CalculateHIGC.R')
source('R/ReadModelInitialConditions.R')
source('R/PerformSimulation.R')
source('R/ExtractWeatherData.R')
source('R/Solution.R')
source('R/CheckModelTermination.R')
source('R/GrowingDegreeDay.R')
source('R/CheckGroundwaterTable.R')
source('R/PreIrrigation.R')
source('R/Drainage.R')
source('R/RainfallPartition.R')
source('R/Irrigation.R')
source('R/RootZoneWater.R')
source('R/Infiltration.R')
source('R/CapillaryRise.R')
source('R/Germination.R')
source('R/GrowthStage.R')
source('R/RootDevelopment.R')
source('R/CanopyCover.R')
source('R/WaterStress.R')
source('R/SoilEvaporation.R')
source('R/EvapLayerWaterContent.R')
source('R/Transpiration.R')
source('R/AerationStress.R')
source('R/GroundwaterInflow.R')
source('R/HIrefCurrentDay.R')
source('R/BiomassAccumulation.R')
source('R/TemperatureStress.R')
source('R/HarvestIndex.R')
source('R/CCDevelopment.R')
source('R/AdjustCCx.R')
source('R/CCRequiredTime.R')
source('R/HIadjPreAnthesis.R')
source('R/HIadjPostAnthesis.R')
source('R/UpdateTime.R')
source('R/ResetInitialConditions.R')
source('R/HIadjPollination.R')
source('R/Initialise.R')
source('R/SoilHydraulicProperties.R')

#library('AquaCropR')

folder_names = dir(pattern='input_whe*')
for(folder_name in folder_names){
 
    FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', sep=''))
    InitialiseStruct <- Initialise(FileLocation)
    
    
    Outputs <- PerformSimulation(InitialiseStruct)
    Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
    Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
    Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
    Outputs = setDT(mutate(Outputs, DOY = convertDOY(Outputs$PlantingDate)))
    d <- split(Outputs, by = 'PlantingDate')
    i = lapply(d, function(x) x[as.numeric(which(x$Yield == max(x$Yield)))])
    u = data.frame(t(data.frame(rbind(sapply(i, function(x) x)))))
    
    d <- list()
    d[['RefBio']] <- 'Biomass (g m-2)'
    d[['Yield']] <- 'Yield tonne/ha'
    d[['CC']] <- 'Canopy cover (%)'
    d[['Infl']] <- 'Infiltration (mm)'
    d[['Irr']] <- 'Irrigation (mm)'
    d[['Et0']] <- 'Et0'
    
    for(cname in names(d)){
      tiff(paste(FileLocation$Output, 'Figure_', cname, '.tiff', sep=''),  width = 1000,
           height = 700)
      p <- ggplot(Outputs, aes(x = TotGDD, y = Outputs[[cname]], col = PlantingDate)) +
        geom_line(aes(linetype=PlantingDate, color=PlantingDate), size = 0.7) + 
        theme_bw() +  labs(y = d[[cname]], x = 'total Degree Day') +
        theme(axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
             # legend.text = element_text(size = 12),
              #legend.position="bottom")
              legend.position = "none")
              #legend.key.size = unit(2,"line")) 
      #+  facet_grid(PlantingDate ~ .)
      print(p) 
    
      dev.off()  
    }
    
    tiff(paste(FileLocation$Output, 'Figure_', 'DOY', '.tiff', sep=''),  width = 1000,
         height = 700)
    u[['Year']] = as.numeric(u[['Year']])
    p <- ggplot(u, aes(x = as.numeric(DOY), y = as.numeric(Yield), col = Year)) +
          geom_point(size=4) +  theme_bw() +
          labs(y = 'Yield tonne/ha', x = 'DOY') +
          theme(axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16),
                axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 12),
            legend.position = "none")
      
    print(p)
    dev.off()
}
