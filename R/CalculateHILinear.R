#' Calculate time to switch to linear harvest index build-up, and 
#' associated linear rate of build-up. Only for fruit/grain crops.
#' @param Crop list
#' @return list with \code{tSwitch} and \code{dHILin}.
#' @export
#' @examples
#' CalculateHILinear(Crop)


CalculateHILinear <- function(Crop){

    ## Determine linear switch point ##
    # Initialise variables
    
    cc <- list()
    ti <- 0
    tmax <- Crop$YldFormCD
    HIest <- 0
    HIprev <- Crop$HIini
    # Iterate to find linear switch point
          while ((HIest <= Crop$HI0) && (ti < tmax)){
              ti <- ti+1
              HInew <- (Crop$HIini * Crop$HI0)/(Crop$HIini+(Crop$HI0 - Crop$HIini)*
                  exp(-Crop$HIGC * ti))
              HIest <- HInew + (tmax-ti) * (HInew - HIprev)
              HIprev <- HInew
          }
          tSwitch <- ti-1
    
          ## Determine linear build-up rate ##
          if (tSwitch > 0){
              HIest <- (Crop$HIini * Crop$HI0)/(Crop$HIini+(Crop$HI0 - Crop$HIini)*
                  exp(-Crop$HIGC * tSwitch))
          } else {
              HIest <- 0
          }
          dHILin <- (Crop$HI0 - HIest)/(tmax - tSwitch)
    
          cc$tSwitch <- tSwitch
          cc$dHILin <- dHILin
          return(cc)

}

