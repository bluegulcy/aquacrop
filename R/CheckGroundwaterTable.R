#' Check for presence of a groundwater table, and, if present,
#' to adjust compartment water contents and field capacities where
#' necessary
#' @param Soil structure of Soil
#' @param Groundwater ground water table
#' @param InitCond Crop setting initial structure
#' @param ClockStruct Model time settings
#' @export
#' @return \code{NewCond} model values for n time-step
#' @examples
#' CheckGroundwaterTable(Soil, Groundwater, InitCond)


CheckGroundwaterTable <- function(Soil, Groundwater, InitCond, ClockStruct){


    ## Store initial conditions for updating ##
    NewCond <- InitCond

    ## Perform calculations (if variable water table is present) ##
    if(Groundwater$WaterTable == 1 & !is.null(Groundwater$Method == 'Variable')){
        # Update groundwater conditions for current day
        NewCond$zGW <- Groundwater$zGW[c(Groundwater$zGW[, 1] ==
                                           ClockStruct$StepStartTime),2]

        # Find compartment mid-points
        zBot <- cumsum(Soil$Comp$dz)
        zTop <- zBot - Soil$Comp$dz
        zMid <- (zTop+zBot)/2

        # Check if water table is within modelled soil profile
        if (NewCond$zGW >= 0){
            if(is.na(which(zMid >= NewCond$zGW)[1])){
                NewCond$WTinSoil <- FALSE
            } else {
                NewCond$WTinSoil <- TRUE
            }
        }

        # If water table is in soil profile, adjust water contents
        if (NewCond$WTinSoil == TRUE){
            idx <- find(zMid >= NewCond$zGW)
            for (ii in idx:Soil$nComp){
                layeri <- Soil$Comp$Layer[ii]
                NewCond$th[ii] <- Soil$Layer$th_s[layeri]
            }
        }

        # Adjust compartment field capacity
        compi <- Soil$nComp
        thfcAdj <- rep(0, 1,compi)
        # Find thFCadj for all compartments
        while(compi >=  1){
            layeri <- Soil$Comp$Layer[compi]
            if(Soil$Layer$th_fc[layeri] <= 0.1){
                Xmax <- 1
            } else {
                if(Soil$Layer$th_fc[layeri] >= 0.3){
                    Xmax <- 2
                } else {
                    pF <- 2+0.3*(Soil$Layer$th_fc[layeri]-0.1)/0.2
                    Xmax <- (exp(pF*log(10)))/100
                }
            }
            if(NewCond$zGW < 0 | ((NewCond$zGW - zMid[compi]) >= Xmax)){
                for(ii in 1:compi){
                    layerii <- Soil$Comp$Layer(ii)
                    thfcAdj(ii) <- Soil$Layer$th_fc(layerii)
                }
                compi <- 0
            } else {
                if(Soil$Layer$th_fc[layeri] >= Soil$Layer$th_s[layeri]){
                    thfcAdj[compi] <- Soil$Layer$th_fc[layeri]
                } else {
                    if(zMid[compi] >= NewCond$zGW){
                        thfcAdj[compi] <- Soil$Layer$th_s[layeri]
                    } else {
                        dV <- Soil$Layer$th_s[layeri] - Soil$Layer$th_fc[layeri]
                        dFC <- (dV/(Xmax^2))*((zMid[compi]-(NewCond$zGW - Xmax))^2)
                        thfcAdj[compi] <- Soil$Layer$th_fc[layeri] + dFC
                    }
                }
                compi <- compi - 1
            }
        }
        # Store adjusted field capacity values
        NewCond$th_fc_Adj <- thfcAdj
    }

    return(NewCond)
}

