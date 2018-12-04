#' Calculate canopy cover development by end of the current
#' simulation day
#' @param CCo CCDevelopment
#' @param CCx Maximum canopy cover
#' @param CGC Canopy growth coefficient
#' @param CDC Canopy decline coefficient
#' @param dt Canopy approaching maximum size
#' @param Mode crop calendar mode
#' @return \code{CC} canopy for a n time-step.
#' @examples
#' CCDevelopment(CCo, CCx, CGC, CDC, dt, Mode)

CCDevelopment <- function(CCo, CCx, CGC, CDC, dt, Mode){


    ## Initialise output ##
    CC <- c()

    ## Calculate new canopy cover ##
    if (Mode =='Growth'){
        # Calculate canopy growth
        # Exponential growth stage
        CC <- CCo*exp(CGC*dt)
        if (CC > (CCx/2)){
            # Exponential decay stage
            CC <- CCx-0.25*(CCx/CCo)*CCx*exp(-CGC*dt)
        }
        # Limit CC to CCx
        if (CC > CCx){
            CC <- CCx
        }
    } else if (Mode == 'Decline'){
        # Calculate canopy decline
        if (CCx < 0.001){
            CC <- 0
        } else {
            CC <- CCx*(1-0.05*(exp(dt*(CDC/CCx))-1))
        }
    }

    ## Limit canopy cover to between 0 and 1 ##
    if (CC > 1){
        CC <- 1
    } else if (CC < 0){
        CC <- 0
    }

    return(CC)

}

