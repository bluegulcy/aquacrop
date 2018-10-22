#' Calculate capillary rise from a shallow groundwater table
#' @param Soil properties of soil
#' @param InitCond Crop setting initial structure
#' @param Groundwater ground water table
#' @param FluxOut Flux
#' @return list with \code{NewCond} and \code{CrTot} for a n time-step.
#' @examples
#' CapillaryRise(Soil, Groundwater, InitCond, FluxOut)


CapillaryRise <- function(Soil, Groundwater, InitCond, FluxOut){


    ## Store initial conditions for updating ##
    cc <- list()
    NewCond <- InitCond

    ## Get groundwater table elevation on current day ##
    zGW <- NewCond$zGW

    ## Calculate capillary rise ##
    if (Groundwater$WaterTable == 0){# No water table present
        # Capillary rise is zero
        CrTot <- 0
    } else if(Groundwater$WaterTable == 1){# Water table present
        # Get maximum capillary rise for bottom compartment
        zBot <- sum(Soil$Comp$dz)
        zBotMid <- zBot-(Soil$Comp$dz(Soil$nComp)/2)
        layeri <- Soil$Comp$Layer[Soil$nComp]
        if((Soil$Layer$Ksat[layeri] > 0) & (zGW > 0) & ((zGW-zBotMid) < 4)){
            if(zBotMid >= zGW){
                MaxCR <- 99
            } else {
                MaxCR <- exp((log(zGW-zBotMid)-Soil$Layer$bCR[layeri])/Soil$Layer$aCR[layeri])
                if (MaxCR > 99){
                    MaxCR <- 99
                }
            }
        } else {
            MaxCR <- 0
        }
        # Find top of next soil layer that is not within modelled soil profile
        zTopLayer <- 0
        for (ii in 1:Soil$Comp$Layer[Soil$nComp]){
            # Calculate layer thickness
            LayThk <- Soil$Layer$dz[ii]
            zTopLayer <- zTopLayer+LayThk
        }
        # Check for restrictions on upward flow caused by properties of
        # compartments that are not modelled in the soil water balance
        layeri <- Soil$Comp$Layer[Soil$nComp]
        while((zTopLayer < zGW) & (layeri < Soil$nLayer)){
            layeri <- layeri+1
            if((Soil$Layer$Ksat[layeri] > 0) & (zGW > 0) & ((zGW-zTopLayer) < 4)){
                if (zTopLayer >= zGW){
                    LimCR <- 99
                } else {
                    LimCR <- exp((log(zGW-zTopLayer)-Soil$Layer$bCR[layeri])/
                        Soil$Layer$aCR[layeri])
                    if (LimCR > 99){
                        LimCR <- 99
                    }
                }
            } else {
                LimCR <- 0
            }
            if(MaxCR > LimCR){
                MaxCR <- LimCR
            }
            zTopLayer <- zTopLayer+Soil$Layer$dz[layeri]
        }
        # Calculate capillary rise
        compi <- Soil$nComp# Start at bottom of root zone
        WCr <- 0# Capillary rise counter
        while((round(MaxCR * 1000) >0) & (compi > 0) & (round(FluxOut[compi]*1000)  == 0)){
            # Proceed upwards until maximum capillary rise occurs, soil surface
            # is reached, or encounter a compartment where downward
            # drainage/infiltration has already occurred on current day
            # Find layer of current compartment
            layeri <- Soil$Comp$Layer[compi]
            # Calculate driving force
            if((NewCond$th[compi] >= Soil$Layer$th_wp[layeri]) & (Soil$fshape_cr > 0)){
                Df <- 1-(((NewCond$th[compi]-Soil$Layer$th_wp[layeri])/
                    (NewCond$th_fc_Adj[compi]-Soil$Layer$th_wp[layeri]))^Soil$fshape_cr)
                if (Df > 1){
                    Df <- 1
                } else if (Df < 0){
                    Df <- 0
                }
            } else {
                Df <- 1
            }
            # Calculate relative hydraulic conductivity
            thThr <- (Soil$Layer$th_wp[layeri]+Soil$Layer$th_fc[layeri])/2
            if(NewCond$th[compi] < thThr){
                if((NewCond$th[compi] <= Soil$Layer$th_wp[layeri]) | (thThr <= Soil$Layer$th_wp[layeri])){
                    Krel <- 0
                } else {
                    Krel <- (NewCond$th[compi]-Soil$Layer$th_wp[layeri])/
                        (thThr-Soil$Layer$th_wp[layeri])
                }
            } else {
                Krel <- 1
            }
            # Check if room is available to store water from capillary rise
            dth <- NewCond$th_fc_Adj[compi]-NewCond$th[compi]
            dth <- round((dth*1000))/1000

            # Store water if room is available
            if((dth > 0) & ((zBot - Soil$Comp$dz[compi]/2) < zGW)){
                dthMax <- Krel*Df*MaxCR/(1000*Soil$Comp$dz[compi])
                if (dth >= dthMax){
                    NewCond$th[compi] <- NewCond$th[compi]+dthMax
                    CRcomp <- dthMax*1000*Soil$Comp$dz[compi]
                    MaxCR <- 0
                } else {
                    NewCond$th[compi] <- NewCond$th_fc_Adj[compi]
                    CRcomp <- dth*1000*Soil$Comp$dz[compi]
                    MaxCR <- (Krel*MaxCR)-CRcomp
                }

                WCr <- WCr+CRcomp
            }
            # Update bottom elevation of compartment
            zBot <- zBot-Soil$Comp$dz[compi]
            # Update compartment and layer counters
            compi <- compi-1
            # Update restriction on maximum capillary rise
            if(compi > 0){
                layeri <- Soil$Comp$Layer[compi]
                zBotMid <- zBot-(Soil$Comp$dz[compi]/2)
                if((Soil$Layer$Ksat[layeri] > 0) & (zGW > 0) & ((zGW-zBotMid) < 4)){
                    if(zBotMid >= zGW){
                        LimCR <- 99
                    } else {
                        LimCR <- exp((log(zGW-zBotMid)-Soil$Layer$bCR[layeri])/
                            Soil$Layer$aCR[layeri])
                        if(LimCR > 99){
                            LimCR <- 99
                        }
                    }
                } else {
                    LimCR <- 0
                }

                if(MaxCR > LimCR){
                    MaxCR <- LimCR
                }
            }
        }
        # Store total depth of capillary rise
        CrTot <- WCr
    }
    cc$NewCond <- NewCond
    cc$CrTot <- CrTot

    return(cc)
}

