#' Calculate pre-irrigation when in net irrigation mode
#' @param Soil structure characteristics
#' @param Crop crop settings
#' @param IrrMngt irigation management settings
#' @param InitCond Crop setting initial structure
#' @return list with \code{NewCond} and \code{PreIrr} for n time-step
#' @export
#' @examples
#' PreIrrigation((Soil, Crop, IrrMngt, InitCond)



PreIrrigation <- function(Soil, Crop, IrrMngt, InitCond){

    ## Store initial conditions for updating ##
    cc <- list()
    NewCond <- InitCond

    ## Calculate pre-irrigation needs ##
    if (IrrMngt$IrrMethod != 4 | (NewCond$DAP != 1)){
        # No pre-irrigation as not in net irrigation mode or not on first day
        # of the growing season
        PreIrr <- 0
    } else {
        # Determine compartments covered by the root zone
        rootdepth <- max(InitCond$Zroot, Crop$Zmin)
        rootdepth <- round((rootdepth*100))/100
        comp_sto <- which(Soil$Comp$dzsum >= rootdepth)[1]
        # Calculate pre-irrigation requirements
        PreIrr <- 0

        for (ii in 1:comp_sto){
            # Get soil layer
            layeri <- Soil$Comp$Layer[ii]
            # Determine critical water content threshold
            thCrit <- Soil$Layer$th_wp[layeri] + ((IrrMngt$NetIrrSMT/100)*
                (Soil$Layer$th_fc[layeri] - Soil$Layer$th_wp[layeri]))
            # Check if pre-irrigation is required
            if (NewCond$th[ii] < thCrit){
                PreIrr <- PreIrr + ((thCrit - NewCond$th[ii]) * 1000 * Soil$Comp$dz[ii])
                NewCond$th[ii] <- thCrit
            }
        }
    }

    cc$NewCond <- NewCond
    cc$PreIrr <- PreIrr

    return(cc)

}
