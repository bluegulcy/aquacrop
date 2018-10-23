#'Function to read input and output file locations
library('XML')
library('xml2')
library('pracma')
library('kulife')
library('ggplot2')


library('AquaCropR')

#FileLocation = ReadFileLocations('FileSetup.xml')
FileLocation = ReadFileLocations('FileSetup_babycorn.xml')
InitialiseStruct <- Initialise(FileLocation)
Outputs <- PerformSimulation(InitialiseStruct)
Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)


d <- list()
d[['RefBio']] <- 'Biomass (g m-2)'
d[['Yield']] <- 'Yield tonne/ha'
d[['CC']] <- 'Canopy cover (%)'
d[['Infl']] <- 'Infiltration (mm)'
d[['Irr']] <- 'Irrigation (mm)'
d[['Et0']] <- 'Et0'

for(cname in names(d)){
  tiff(paste(FileLocation$Output, '/Figure_', cname, '.tiff', sep=''),  width = 1000,
       height = 700,res=150)
  p <- ggplot(Outputs, aes(x = TotGDD, y = Outputs[[cname]], col = PlantingDate)) +
    geom_line(aes(linetype=PlantingDate, color=PlantingDate), size = 0.7) + 
    theme_bw() +  labs(y = d[[cname]], x = 'total Degree Day') +
    theme(axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.position="bottom",
          legend.key.size = unit(5,"line")) 
  #+  facet_grid(PlantingDate ~ .)
  print(p) 

  dev.off()  
}



  
