#'Function to read input and output file locations
library('XML')
library('xml2')
library('pracma')
library('kulife')
library('ggplot2')
library('reshape')
library('AquaCropR')

folder_names = dir(pattern='input_*')
for(folder_name in folder_names){
 
    FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml', sep=''))
    InitialiseStruct <- Initialise(FileLocation)
    
    
    Outputs <- PerformSimulation(InitialiseStruct)
    Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
    Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
    Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
    Outputs <- setDT(mutate(Outputs, DOY = convertDOY(Outputs$PlantingDate)))
    Outputs_month <- split(Outputs, by = 'PlantingDate')
    i = lapply(Outputs_month, function(x) x[as.numeric(which(x$Yield == max(x$Yield)))][1])
    u = data.frame(t(data.frame(rbind(sapply(i, function(x) x)))))
    
    d <- list()
    d[['RefBio']] <- 'Biomass (g m-2)'
    d[['Yield']] <- 'Yield tonne/ha'
    d[['CC']] <- 'Canopy cover (%)'
    d[['Infl']] <- 'Infiltration (mm)'
    d[['Irr']] <- 'Irrigation (mm)'
    d[['Et0']] <- 'Et0'
    
    for(cname in names(d)){
      tiff(paste(FileLocation$Output, 'Figure_', cname, '.tiff', sep=''),  width = 800,
           height = 600, res = 145)
      p <- ggplot(Outputs, aes(x = TotGDD, y = Outputs[[cname]], col = PlantingDate)) +
        geom_line(aes(linetype=PlantingDate, color=PlantingDate), size = 0.7) + 
        theme_bw() +  labs(y = d[[cname]], x = 'total Degree Day') +
        theme(axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 11.5, angle = 90),
              axis.text.y = element_text(size = 11.5, angle = 90),
             # legend.text = element_text(size = 12),
              #legend.position="bottom")
              legend.position = "none")
              #legend.key.size = unit(2,"line")) 
      #+  facet_grid(PlantingDate ~ .)
      print(p) 
    
      dev.off()  
    }
    
    tiff(paste(FileLocation$Output, 'Figure_', 'DOY', '.tiff', sep=''),  width = 800,
         height = 600, res=150)
    u[['Year']] = as.numeric(u[['Year']])
    p <- ggplot(u, aes(x = as.numeric(DOY), y = as.numeric(Yield), col = Year)) +
          geom_point(size=4) +  theme_bw() +
          labs(y = 'Yield tonne/ha', x = 'DOY') +
          theme(axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16),
                axis.text.x = element_text(size = 12, angle = 90),
                axis.text.y = element_text(size = 12, angle = 90),
            legend.position = "none")
      
    print(p)
    dev.off()
}
