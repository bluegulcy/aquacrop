#' Adjust CCx value for changes in CGC due to water stress during the growing 
#' season, AquacropR
#' @param CCprev Prev Cannopy cover
#' @param CCo initial canopy cover at the time of 90\% crop emergence
#' @param CCx Maximum canopy cover
#' @param CGC Canopy growth coefficient
#' @param CDC Canopy decline coefficient
#' @param dt Delta time
#' @param tSum  tSum
#' @param Crop Parameters for a given crop
#' @export
#'
#' @return with \code{CCxAdj} for a n time-step.
#' @examples
#' AdjustCCx(CCprev, CCo, CCx, CGC, CDC, dt, tSum, Crop)

AdjustCCx <- function(CCprev, CCo, CCx, CGC, CDC, dt, tSum, Crop){


    ## Get time required to reach CC on previous day ##
    tCCtmp <- CCRequiredTime(CCprev, CCo, CCx, CGC, CDC, dt, tSum, 'CGC')

    ## Determine CCx adjusted ##
    if (tCCtmp > 0){
        tCCtmp <- tCCtmp + (Crop$CanopyDevEnd-tSum)+dt
        CCxAdj <- CCDevelopment(CCo, CCx, CGC, CDC, tCCtmp, 'Growth')
    } else {
        CCxAdj <- 0
    }

    return(CCxAdj)

}

