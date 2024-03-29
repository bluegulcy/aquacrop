#' Check if crop has germinated
#' @param Soil properties of soil
#' @param InitCond Crop setting initial structure
#' @param Crop Parameters for a given crop
#' @param GDD Growing degree days
#' @param GrowingSeason crop developmental stage
#' @return \code{NewCond}  for a n time-step.
#' @export
#' @examples
#' Germination(InitCond, Soil, Crop, GDD, GrowingSeason)


Germination <- function(InitCond, Soil, Crop, GDD, GrowingSeason){

    ## Store initial conditions in new structure for updating ##
    NewCond <- InitCond

    ## Check for germination (if in growing season) ##
    if (GrowingSeason == TRUE){
        # Find compartments covered by top soil layer affecting germination
        comp_sto <- which(Soil$Comp$dzsum >= Soil$zGerm)[1]

        # Calculate water content in top soil layer
        Wr <- 0
        WrFC <- 0
        WrWP <- 0
        for(ii in 1:comp_sto){
            # Get soil layer
            layeri <- Soil$Comp$Layer[ii]
            # Determine fraction of compartment covered by top soil layer
            if (Soil$Comp$dzsum[ii] > Soil$zGerm){
                factor <- 1-((Soil$Comp$dzsum[ii]-Soil$zGerm)/Soil$Comp$dz[ii])
            } else {
                factor <- 1
            }
            # Increment actual water storage (mm)
            Wr <- Wr+(factor*1000*InitCond$th[ii]*Soil$Comp$dz[ii])
            # Increment water storage at field capacity (mm)
            WrFC <- WrFC+(factor*1000*Soil$Layer$th_fc[layeri]*Soil$Comp$dz[ii])
            # Increment water storage at permanent wilting point (mm)
            WrWP <- WrWP+(factor*1000*Soil$Layer$th_wp[layeri]*Soil$Comp$dz[ii])
        }
        # Limit actual water storage to not be less than zero
        if (Wr < 0){
            Wr <- 0
        }
        # Calculate proportional water content
        WcProp <- 1-((WrFC-Wr)/(WrFC-WrWP))

        # Check if water content is above germination threshold
        if((WcProp >= Crop$GermThr) & (NewCond$Germination == FALSE)){
            # Crop has germinated
            NewCond$Germination <- TRUE
        }

        # Increment delayed growth time counters if germination is yet to occur
        if (NewCond$Germination == FALSE){
            NewCond$DelayedCDs <- InitCond$DelayedCDs+1
            NewCond$DelayedGDDs <- InitCond$DelayedGDDs+GDD
        }
    } else {
        # Not in growing season so no germination calculation is performed.
        NewCond$Germination <- FALSE
        NewCond$DelayedCDs <- 0
        NewCond$DelayedGDDs <- 0
    }

    return(NewCond)
}

