#' Compute Nitrogen Nutrition Index        
#' @param TIME time point
#' @param DOYEM day of emergence
#' @param EMERG has the crop emerged? yes = 1; no = 0;
#' @param NFGMR NUPGMR (Total N in green matter of the plant) / TBGMR 
#' (Total living vegetative biomass.)
#' @param NRMR Average residual N concentration
#' @param NOPTMR Maximum N content in the plant
#' @param NNI Nutrition Index (NNI)
#' @return NNI

NNINDX <- function(TIME, DOYEM, EMERG, NFGMR, NRMR, NOPTMR, NNI){
 
  
  TINY=0.001
  NFGMR = NUPGMR / TBGMR
  
  if(TIME >=  DOYEM & EMERG == 1) {
    NNI= limit(TINY, 1.0, ((NFGMR-NRMR)/NOTNUL(NOPTMR-NRMR)))
  } else {
    NNI = 0.0
  }
  
  return(NNI)
}