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
  
  # Water content
  wc_names <- c('Year','Month','Day','SimDay','Season', wc_names, 'PlantingDate')
  
  
  # Water Cluxes
  hy_names <- c('Year','Month','Day', 'SimDay','Season','wRZ','zGW','wSurf',
                'Irr','Infl','RO','DP','CR', 'GWin','Es','EsX','Tr','TrX',
                'PlantingDate')
  
  
  # Crop growth
  cg_names <- c('Year','Month','Day', 'SimDay','Season','GDD','TotGDD',
                'Root Depth',
                'CC','RefCC','Bio', 'RefBio','HI','HIadj','Yield','Et0', 
                'PlantingDate')
  
  # # Final output (at end of each growing season)
  fo_names <- c('GSeason','Crop','PlantD', 'PlantSD','HarvestCD','HarvestSD',
                'Yield','TotIrr')
  
  #i = 1
  ## Get weather inputs for current time step ##
 
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
      # i = i+1
      # print(i)
      
  }
  

  colnames(Outputs$WaterContents) <- wc_names
  colnames(Outputs$WaterFluxes) <- hy_names
  colnames(Outputs$CropGrowth) <- cg_names
  colnames(Outputs$FinalOutput) <- fo_names
  
  Outputs$WaterContents <- data.frame(Outputs$WaterContents)
  Outputs$WaterFluxes <- data.frame(Outputs$WaterFluxes)
  Outputs$CropGrowth <- data.frame(Outputs$CropGrowth)
  results = cbind(Outputs$WaterContents, Outputs$WaterFluxes,Outputs$CropGrowth)
  
  
  results = results[, c(1:17, 24:36, 43:54)]
  results[['PlantingDate']] = as_date(results[['PlantingDate']])
  
  return(results)

}
