#' Update CCx and CDC parameter valyes for rewatering in late
#' season of an early declining canopy
#' @param CCprev Prev Canopy Cover
#' @param CDC Canopy decline coefficient
#' @param CCx Maximum canopy cover
#' @param dt dt parameter
#' @return list with \code{CCXadj} and \code{CDCadj} for a n time-step.
#' @export
#' @examples
#' UpdateCCxCDC(CCprev,CDC,CCx,dt)


UpdateCCxCDC <- function(CCprev, CDC, CCx, dt){


    cc <- list()
    ## Get adjusted CCx ##
    CCXadj <- CCprev/(1-0.05*(exp(dt*(CDC/CCx))-1))

    ## Get adjusted CDC ##
    CDCadj <- CDC*(CCXadj/CCx)

    cc$CCXadj <- CCXadj
    cc$CDCadj <- CDCadj

    return(cc)

}

