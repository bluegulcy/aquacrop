#' Partition rainfall into surface runoff and infiltration
#  using the curve number approach
#' @param P precipitation
#' @param Soil structure characteristics
#' @param IrrMngt Irrigation management
#' @param InitCond Crop setting initial structure
#' @return list with \code{NewCond}, \code{Runoff} and \code{Infl} for n time-step
#' @examples
#' RainfallPartition(P, Soil, FieldMngt, InitCond)

RainfallPartition <- function(P, Soil, FieldMngt, InitCond){

    

    ## Store initial conditions for updating ##
    cc <- list()
    NewCond <- InitCond

    ## Calculate runoff ##
    if((FieldMngt$Bunds == 0) | (FieldMngt$zBund < 0.001)){
        # No bunds on field
        # Reset submerged days
        NewCond$DaySubmerged <- 0
        if(Soil$AdjCN == 1){# Adjust CN for antecedent moisture
            # Check which compartment cover depth of top soil used to adjust
            # curve number
            comp_sto <- which(Soil$Comp$dzsum >= Soil$zCN)[1]
            if(is.null(comp_sto)){
                comp_sto <- Soil$nComp
            }
            # Calculate weighting factors by compartment
            xx <- 0
            wrel <- rep(0, 1, comp_sto)
            for(ii in 1:comp_sto){
                if(Soil$Comp$dzsum[ii] > Soil$zCN){
                    Soil$Comp$dzsum[ii] <- Soil$zCN
                }
                wx <- 1.016*(1-exp(-4.16*(Soil$Comp$dzsum[ii]/Soil$zCN)))
                wrel[ii] <- wx-xx
                if (wrel[ii] < 0){
                    wrel[ii] <- 0
                } else if (wrel[ii] > 1){
                    wrel[ii] <- 1
                }
                xx <- wx
            }
            # Calculate relative wetness of top soil
            wet_top <- 0
            for(ii in 1:comp_sto){
                layeri <- Soil$Comp$Layer[ii]
                th <- max(Soil$Layer$th_wp[layeri], InitCond$th[ii])
                wet_top <- wet_top+(wrel[ii]*((th-Soil$Layer$th_wp[layeri])/
                    (Soil$Layer$th_fc[layeri]-Soil$Layer$th_wp[layeri])))
            }
            # Calculate adjusted curve number
            if (wet_top > 1){
                wet_top <- 1
            } else if(wet_top < 0){
                wet_top <- 0
            }
            CN <- round(Soil$CNbot + (Soil$CNtop-Soil$CNbot) * wet_top)
        } else {# Curve number is not adjusted
            CN <- Soil$CN
        }
        # Partition rainfall into runoff and infiltration (mm)
        S <- (25400/CN) - 254
        term <- P-((5/100)*S)
        if (term <= 0){
            Runoff <- 0
            Infl <- P
        } else {
            Runoff <- (term ^ 2) / (P+(1-(5/100))*S)
            Infl <- P-Runoff
        }
    } else {
        # Bunds on field, therefore no surface runoff
        Runoff <- 0
        Infl <- P
    }

    cc$Runoff <- Runoff
    cc$Infl <- Infl
    cc$NewCond <- NewCond

    return(cc)
}

