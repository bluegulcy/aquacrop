#' Perform Optimisation
#' @param InitialiseStruct Crop setting initial structure
#' @return list with \code{Outputs} results.
#' @export
#' @examples
#' PerformSimulationOptimisation(InitialiseStruct)

PerformSimulationOptimisation <- function(x, InitialiseStruct){
  
 
  for(rn in rownames(param_array)){
     
     InitialiseStruct$Parameter$Crop[[1]][[rn]] <-  
       x[match(rn, rownames(param_array))]
     
  }
   
  
  wc_names <- c()
  
  for(ii in 1:InitialiseStruct$Parameter$Soil$nComp){
    z <- InitialiseStruct$Parameter$Soil$Comp$dzsum[ii] - 
      (InitialiseStruct$Parameter$Soil$Comp$dz[ii]/2)
    wc_names[ii] <- paste(z,'m', sep='')
  }
  
  # Water content
  wc_names <- c('Year','Month','Day','SimDay','Season', wc_names, 'PlantingDate')
  
  
  # Water Fluxes
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
  #for(i in 1:17){
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
      #print(InitialiseStruct$InitialCondition$CropDead)
      #i = i+1
      
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
  results <- setDT(results)
  results <- mutate(results, 
                    datenum = as_datenum(as.Date(paste(Year, Month, Day, 
                                                       sep = '-'))))

  
  results <- merge(results, InitialiseStruct[['obs_data']], by = 'datenum')
  
 

}
