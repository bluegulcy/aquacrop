#' Calculate harvest index growth coefficient.
#' @param Crop list
#' @return list with \code{HIGC} and \code{tHI}.
#' @export
#' @examples
#' CalculateHIGC(Crop)

# Crop = ParamStruct$Crop[[CropNames[ii]]]
CalculateHIGC <- function(Crop){

      cc <- list()

      ## Determine HIGC ##
      # Total  yield formation days
      tHI <- Crop$YldFormCD
      # Iteratively estimate HIGC
      HIGC <- 0.001
      HIest <- 0
      while (HIest <=  (0.98*Crop$HI0)){
          HIGC <- HIGC + 0.001
          HIest <- (Crop$HIini * Crop$HI0) / (Crop$HIini+(Crop$HI0 - Crop$HIini)*
              exp(-HIGC * tHI))
      }
      if (HIest >= Crop$HI0){
          HIGC <- HIGC - 0.001
      }


      cc$HIGC <- HIGC
      cc$tHI <- tHI

      return(cc$HIGC)

}

