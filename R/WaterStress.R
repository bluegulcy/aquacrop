#'  Calculate relative root zone water depletion for each stress type
#' @param Crop Parameters for a given crop
#' @param InitCond Crop setting initial structure
#' @param Dr Root zone water content at air dry
#' @param TAW Total available water
#' @param Et0 Evapotranspiration
#' @param beta beta
#' @return water stress list with \code{Exp}, \code{Sto}, \code{Sen}, \code{Pol} 
#' and \code{StoLin} for a n time-step.
#' @export
#' @examples
#' WaterStress(Crop,InitCond,Dr,TAW,Et0, beta)


WaterStress <- function(Crop, InitCond, Dr, TAW, Et0, beta){

    # Number of stress variables
    nstress <- length(Crop$p_up)

    # Store stress thresholds
    p_up <- Crop$p_up
    p_lo <- Crop$p_lo
    if (Crop$ETadj == 1){
        # Adjust stress thresholds for Et0 on current day (don't do this for
        # pollination water stress coefficient)
        for (ii in 1:3){
            p_up[ii] <- p_up[ii]+(0.04*(5-Et0))*(log10(10-9*p_up[ii]))
            p_lo[ii] <- p_lo[ii]+(0.04*(5-Et0))*(log10(10-9*p_lo[ii]))
        }
    }

    # Adjust senescence threshold if early sensescence is triggered
    if((beta == TRUE) & (InitCond$tEarlySen > 0)){
        p_up[3] <- p_up[3]*(1-Crop$beta/100)
    }

    # Limit values
    p_up[p_up < 0] <- 0
    p_lo[p_lo < 0] <- 0
    p_up[p_up > 1] <- 1
    p_lo[p_lo > 1] <- 1

    # Calculate relative depletion
    Drel <- rep(0, 1, nstress)
    for (ii in 1:nstress){
        if (Dr <= (p_up[ii]*TAW)){
            # No water stress
            Drel[ii] <- 0
        } else if((Dr > (p_up[ii]*TAW)) & (Dr < (p_lo[ii]*TAW))){
            # Partial water stress
            Drel[ii] <- 1-((p_lo[ii]-(Dr/TAW))/(p_lo[ii]-p_up[ii]))
        } else if (Dr >= (p_lo[ii]*TAW)){
            # Full water stress
            Drel[ii] <- 1
        }
    }

    ## Calculate root zone water stress coefficients ##
    Ks <- rep(1,1,3)
    for(ii in 1:3){
        Ks[ii] <- 1-((exp(Drel[ii]*Crop$fshape_w[ii])-1)
            /(exp(Crop$fshape_w[ii])-1))
    }

    Ksw <- list()
    # Water stress coefficient for leaf expansion
    Ksw$Exp <- Ks[1]
    # Water stress coefficient for stomatal closure
    Ksw$Sto <- Ks[2]
    # Water stress coefficient for senescence
    Ksw$Sen <- Ks[3]
    # Water stress coefficient for pollination failure
    Ksw$Pol <- 1-Drel[4]
    # Mean water stress coefficient for stomatal closure
    Ksw$StoLin <- 1-Drel[2]

    return(Ksw)

}
