#' Function to find time required to reach CC at end of previous day, given
#' current CGC or CDC
#' @param CCprev Prev Cannopy cover
#' @param CCo Initial canopy cover at the time of 90\% crop emergence
#' @param CCx Maximum canopy cover
#' @param CGC Canopy growth coefficient
#' @param CDC Canopy decline coefficient
#' @param dt delta time
#' @param tSum tSum
#' @param Mode Stage
#' @export
#' @return \code{tReq} eequired time for a n time-step.
#' @examples
#' CCRequiredTime(CCprev, CCo, CCx, CGC, CDC, dt, tSum, Mode)


CCRequiredTime <- function(CCprev, CCo, CCx, CGC, CDC, dt, tSum, Mode){


    ## Get CGC and/or time (GDD or CD) required to reach CC on previous day ##
    if (Mode == 'CGC'){
        if (CCprev <= (CCx/2)){
            CGCx <- (log(CCprev/CCo))/(tSum-dt)
        } else {
            CGCx <- (log((0.25*CCx*CCx/CCo)/(CCx-CCprev)))/(tSum-dt)
        }
        tReq <- (tSum-dt)*(CGCx/CGC)
    } else if (Mode == 'CDC'){
        tReq <- (log(1+(1-CCprev/CCx)/0.05))/(CDC/CCx)
    }

  return(tReq)

}

