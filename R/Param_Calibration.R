#' Set optimiser
#' @param param_array n x m matrix where n are parameters and m are cols 
#' indicating minimum and maximum value for each parameter 
#' @param InitialiseStruct Crop setting initial structure
#' @param emp_data empiral data to calibrate model
#' @param op_settings optimiser settings
#' @return best paramaters
#' @export
#' @examples
#' set_optimiser(param_array, InitialiseStruct, emp_data, op_settings)
#' 
set_optimiser <- function(param_array, InitialiseStruct, emp_data, op_settings){

  lower <- param_array[,1]
  upper <- param_array[,2]
  emp_data <- mutate(emp_data, datenum =  
                       as_datenum(as.Date(paste(Year, Month, Day, 
                                                sep = '-'))))
  InitialiseStruct[['obs_data']] <- emp_data
  
  Outputs <- DEoptim(Param_calibration, lower, upper,
                     control = op_settings)
  
  for(rn in rownames(param_array)){
    
    InitialiseStruct$Parameter$Crop[[1]][[rn]] <- Outputs$optim$bestmem[[rn]]
    
  }
  
  results <- PerformSimulation(InitialiseStruct)
  results <- merge(results, emp_data, by = 'datenum')
  
  plot(results$Bio.y, col = 'black', ylab = 'Biomass (g m-2)')
  points(results$Bio.x, pch = 14, col = 'red')
  legend("topleft", legend=c("Observed", "Predicted"),
         col=c("black", "red"), lty=1:2, cex=0.8)
  
  return(Outputs$optim$bestmem)
    
}

#' Perform optimisation
#' @param parameters to test in model
#' @examples
#' Param_calibration(param)
Param_calibration <- function(param){
 
 
  out <-  PerformSimulationOptimisation(param, InitialiseStruct)
  res <- caret::postResample(out[['Bio.x']], out[['Bio.y']])
 
  RMSE   <- res[[1]]

 
}
