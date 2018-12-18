#' Calculate capillary rise in the presence of a shallow
#' groundwater table
#' @param Soil properties of soil
#' @param InitCond Crop setting initial structure
#' @return list with \code{NewCond} and \code{GwIn} groundwater inflow for a n time-step.
#' @export
#' @examples
#' GroundwaterInflow(Soil, InitCond)


GroundwaterInflow <- function(Soil, InitCond){


    ## Store initial conditions for updating ##
    cc <- list()
    NewCond <- InitCond
    GwIn <- 0

    ## Perform calculations ##
    if (NewCond$WTinSoil == TRUE){
        # Water table in soil profile. Calculate horizontal inflow.
        # Get groundwater table elevation on current day
        zGW <- NewCond$zGW

        # Find compartment mid-points
        zBot <- cumsum(Soil$Comp$dz)
        zTop <- zBot-Soil$Comp$dz
        zMid <- (zTop+zBot)/2

        # For compartments below water table, set to saturation #
        idx <- which(zMid >= zGW)
        for (ii in idx:Soil$nComp){
            # Get soil layer
            layeri <- Soil$Comp$Layer[ii]
            if (NewCond$th[ii] < Soil$Layer$th_s[layeri]){
                # Update water content
                dth <- Soil$Layer$th_s[layeri] - NewCond$th[ii]
                NewCond$th[ii] <- Soil$Layer$th_s[layeri]
                # Update groundwater inflow
                GwIn <- GwIn+(dth*1000*Soil$Comp$dz[ii])
            }
        }
    }

    cc$NewCond <-  NewCond
    cc$GwIn <- GwIn

    return(cc)

}

