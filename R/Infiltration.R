#' Infiltrate incoming water (rainfall and irrigation)
#' @param Soil properties of soil
#' @param InitCond Crop setting initial structure
#' @param Infl Infiltration
#' @param Irr Irrigation
#' @param IrrMngt Irrigation management
#' @param FieldMngt Field management
#' @param FluxOut Flux
#' @param DeepPerc0 Deep percolation
#' @param Runoff water Runoff
#' @export
#' @return list with \code{NewCond}, \code{DeepPerc}, \code{RunoffTot}, \code{Infl} and \code{FluxOut} for a n time-step.
#' @examples
#' Infiltration(Soil, InitCond, Infl, Irr, IrrMngt, FieldMngt, FluxOut, DeepPerc0, Runoff0)


Infiltration <- function(Soil, InitCond, Infl, Irr, IrrMngt, FieldMngt, FluxOut, DeepPerc0, Runoff0){


## Store initial conditions in new structure for updating ##

    cc <- list()
    NewCond <- InitCond
    thnew <- NewCond$th

    ## Update infiltration rate for irrigation ##
    # Note: irrigation amount adjusted for specified application efficiency
    Infl <- Infl+(Irr*(IrrMngt$AppEff/100))

    ## Determine surface storage (if bunds are present) ##
    if (FieldMngt$Bunds == 1){
        # Bunds on field
        if (FieldMngt$zBund > 0.001){
            # Bund height too small to be considered
            InflTot <- Infl+NewCond$SurfaceStorage
            if(InflTot > 0){
                # Update surface storage and infiltration storage
                if (InflTot > Soil$Layer$Ksat[1]){
                    # Infiltration limited by saturated hydraulic conductivity
                    # of surface soil layer
                    ToStore <- Soil$Layer$Ksat[1]
                    # Additional water ponds on surface
                    NewCond$SurfaceStorage <- InflTot-Soil$Layer$Ksat[1]
                } else {
                    # All water infiltrates
                    ToStore <- InflTot
                    # Reset surface storage depth to zero
                    NewCond$SurfaceStorage <- 0
                }
                # Calculate additional runoff
                if(NewCond$SurfaceStorage > (FieldMngt$zBund*1000)){
                    # Water overtops bunds and runs off
                    RunoffIni <- NewCond$SurfaceStorage - (FieldMngt$zBund*1000)
                    # Surface storage equal to bund height
                    NewCond$SurfaceStorage <- FieldMngt$zBund*1000
                } else {
                    # No overtopping of bunds
                    RunoffIni <- 0
                }
            } else {
                # No storage or runoff
                ToStore <- 0
                RunoffIni <- 0
            }
        }
    } else if (FieldMngt$Bunds == 0){
        #FIXME put an message when value isn't 1 or 0
        # No bunds on field
        if (Infl > Soil$Layer$Ksat[1]){
            # Infiltration limited by saturated hydraulic conductivity of top
            # soil layer
            ToStore <- Soil$Layer$Ksat[1]
            # Additional water runs off
            RunoffIni <- Infl-Soil$Layer$Ksat[1]
        } else {
            # All water infiltrates
            ToStore <- Infl
            RunoffIni <- 0
        }
    }

    ## Initialise counters ##
    ii <- 0
    Runoff <- 0

    ## Infiltrate incoming water ##
    if (ToStore > 0){
        while ((ToStore > 0) && (ii < Soil$nComp)){
            # Update compartment counter
            ii <- ii+1
            # Get soil layer
            layeri <- Soil$Comp$Layer[ii]

            # Calculate saturated drainage ability
            dthdtS <- Soil$Layer$tau[layeri]*(Soil$Layer$th_s[layeri]-
                Soil$Layer$th_fc[layeri])
            # Calculate drainage factor
            factor <- Soil$Layer$Ksat[layeri]/(dthdtS*1000*Soil$Comp$dz[ii])

            # Calculate drainage ability required
            dthdt0 <- ToStore/(1000*Soil$Comp$dz[ii])

            # Check drainage ability
            if (dthdt0 < dthdtS){
                # Calculate water content, thX, needed to meet drainage dthdt0
                if (dthdt0 <= 0){
                    theta0 <- InitCond$th_fc_Adj[ii]
                } else {
                    A <- 1+((dthdt0*(exp(Soil$Layer$th_s[layeri]-Soil$Layer$th_fc[layeri])-1))
                        /(Soil$Layer$tau[layeri]*(Soil$Layer$th_s[layeri]-Soil$Layer$th_fc[layeri])))
                    theta0 <- Soil$Layer$th_fc[layeri]+log(A)
                }
                # Limit thX to between saturation and field capacity
                if (theta0 > Soil$Layer$th_s[layeri]){
                    theta0 <- Soil$Layer$th_s[layeri]
                } else if (theta0 <= InitCond$th_fc_Adj[ii]){
                    theta0 <- InitCond$th_fc_Adj[ii]
                    dthdt0 <- 0
                }
            } else {
                # Limit water content and drainage to saturation
                theta0 <- Soil$Layer$th_s[layeri]
                dthdt0 <- dthdtS
            }

            # Calculate maximum water flow through compartment ii
            drainmax <- factor*dthdt0*1000*Soil$Comp$dz[ii]
            # Calculate total drainage from compartment ii
            drainage <- drainmax+FluxOut[ii]
            # Limit drainage to saturated hydraulic conductivity
            if (drainage > Soil$Layer$Ksat[layeri]){
                drainmax <- Soil$Layer$Ksat[layeri]-FluxOut[ii]
            }

            # Calculate difference between threshold and current water contents
            diff <- theta0 - InitCond$th[ii]

            if (diff > 0){
                # Increase water content of compartment ii
                thnew[ii] <- thnew[ii]+(ToStore/(1000*Soil$Comp$dz[ii]))
                if (thnew[ii] > theta0){
                    # Water remaining that can infiltrate to compartments below
                    ToStore <- (thnew[ii]-theta0)*1000*Soil$Comp$dz[ii]
                    thnew[ii] <- theta0
                } else {
                    # All infiltrating water has been stored
                    ToStore <- 0
                }
            }
            # Update outflow from current compartment (drainage + infiltration
            # flows)
            FluxOut[ii] <- FluxOut[ii]+ToStore

            # Calculate back-up of water into compartments above
            excess <- ToStore-drainmax
            if (excess < 0){
                excess <- 0
            }

            # Update water to store
            ToStore <- ToStore-excess

            # Redistribute excess to compartments above
            if(excess > 0){
                precomp <- ii+1
                while((excess>0) & (precomp != 1)){
                    # Keep storing in compartments above until soil surface is
                    # reached
                    # Update compartment counter
                    precomp <- precomp-1
                    # Update layer number
                    layeri <- Soil$Comp$Layer[precomp]
                    # Update outflow from compartment
                    FluxOut[precomp] <- FluxOut[precomp]-excess
                    # Update water content
                    thnew[precomp] <- thnew[precomp]+(excess/(Soil$Comp$dz[precomp]*1000))
                    # Limit water content to saturation
                    if (thnew[precomp] > Soil$Layer$th_s[layeri]){
                        # Update excess to store
                        excess <- (thnew[precomp]-Soil$Layer$th_s[layeri])*
                            1000*Soil$Comp$dz[precomp]
                        # Set water content to saturation
                        thnew[precomp] <- Soil$Layer$th_s[layeri]
                    } else {
                        # All excess stored
                        excess <- 0
                    }
                }

                if(excess > 0){
                    # Any leftover water not stored becomes runoff
                    Runoff <- Runoff+excess
                }
            }
        }
        # Infiltration left to store after bottom compartment becomes deep
        # percolation (mm)
        DeepPerc <- ToStore
    } else {
        # No infiltration
        DeepPerc <- 0
        Runoff <- 0
    }

    ## Update total runoff ##
    Runoff <- Runoff+RunoffIni

    ## Update surface storage (if bunds are present) ##
    if(Runoff > RunoffIni){
        if(FieldMngt$Bunds == 1){
            if(FieldMngt$zBund > 0.001){
                # Increase surface storage
                NewCond$SurfaceStorage <- NewCond$SurfaceStorage+(Runoff-RunoffIni)
                # Limit surface storage to bund height
                if(NewCond$SurfaceStorage > (FieldMngt$zBund*1000)){
                    # Additonal water above top of bunds becomes runoff
                    Runoff <- RunoffIni+(NewCond$SurfaceStorage-(FieldMngt$zBund*1000))
                    # Set surface storage to bund height
                    NewCond$SurfaceStorage <- FieldMngt$zBund*1000
                } else {
                    # No additional overtopping of bunds
                    Runoff <- RunoffIni
                }
            }
        }
    }

    ## Store updated water contents ##
    NewCond$th <- thnew

    ## Update deep percolation, surface runoff, and infiltration values ##
    DeepPerc <- DeepPerc+DeepPerc0
    Infl <- Infl-Runoff
    RunoffTot <- Runoff+Runoff0


    cc$NewCond <- NewCond
    cc$DeepPerc <- DeepPerc
    cc$RunoffTot  <- RunoffTot
    cc$Infl  <- Infl
    cc$FluxOut  <- FluxOut

    return(cc)
}
