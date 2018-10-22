#' Perform simulation
#' @param InitialiseStruct Crop setting initial structure
#' @return list with \code{Outputs} results.
#' @examples
#' PerfomSimulation(InitialiseStruct)

PerformSimulation <- function(InitialiseStruct){
  

  wc_names <- c()
  
  for(ii in 1:InitialiseStruct$Parameter$Soil$nComp){
    z <- InitialiseStruct$Parameter$Soil$Comp$dzsum[ii] - 
      (InitialiseStruct$Parameter$Soil$Comp$dz[ii]/2)
    wc_names[ii] <- paste(z,'m', sep='')
  }
  
  wc_names <- c('Year','Month','Day','SimDay','Season', wc_names)
  
  
  #water fluxed
  hy_names <- c('Year','Month','Day', 'SimDay','Season','wRZ','zGW','wSurf',
                'Irr','Infl','RO','DP','CR', 'GWin','Es','EsX','Tr','TrX',
                'PlantingDate')
  
  
  # Crop growth (daily)
  cg_names <- c('Year','Month','Day', 'SimDay','Season','GDD','TotGDD',
                'Root Depth',
                'CC','RefCC','Bio', 'RefBio','HI','HIadj','Yield','PlantingDate',
                'Et0')
  
  # # Final output (at end of each growing season)
  fo_names <- c('GSeason','Crop','PlantD', 'PlantSD','HarvestCD','HarvestSD',
                'Yield','TotIrr')
  
  ## Get weather inputs for current time step ##
  #for(i in 1:12){
  while(InitialiseStruct$ClockStruct$ModelTermination == FALSE){

      Weather <- ExtractWeatherData(InitialiseStruct)
      #print(as_date(Weather$Dates))
      
      ## Get model solution ##
      
      AS <- Solution(Weather, InitialiseStruct)
      NewCond <- AS$NewCond
      Outputs <- AS$Outputs
      InitialiseStruct$ClockStruct <- AS$ClockStruct
      ## Update initial conditions and outputs ##
      InitialiseStruct$InitialCondition <- NewCond
      InitialiseStruct$Outputs <- Outputs
      
      ## Check model termination ##
      InitialiseStruct$ClockStruct <- 
        CheckModelTermination(InitialiseStruct$ClockStruct, InitialiseStruct)
      
      
      UT <- UpdateTime(InitialiseStruct$ClockStruct, InitialiseStruct)
      InitialiseStruct <- UT$InitialiseStruct
      InitialiseStruct$ClockStruct <- UT$ClockStruct
      #i = i+1
      #print(i)
      
  }
  

  colnames(Outputs$WaterContents) <- wc_names
  colnames(Outputs$WaterFluxes) <- hy_names
  colnames(Outputs$CropGrowth) <- cg_names
  colnames(Outputs$FinalOutput) <- fo_names
  
  Outputs$CropGrowth <- data.frame(Outputs$CropGrowth)
  Outputs$CropGrowth$PlantingDate <- 
    as_date(Outputs$CropGrowth$PlantingDate)
  
  Outputs$WaterContents <- data.frame(Outputs$WaterContents)
  Outputs$WaterFluxes <- data.frame(Outputs$WaterFluxes)
  Outputs$WaterFluxes$PlantingDate <- 
    as_date(Outputs$WaterFluxes$PlantingDate)
 
  
  return(Outputs)

}
