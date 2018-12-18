#' Calculate root zone expansion
#' @param Crop Parameters for a given crop
#' @param Soil properties of soil
#' @param Groundwater ground water table
#' @param InitCond Crop setting initial structure
#' @param GDD Growing degree days
#' @param GrowingSeason crop developmental stage
#' @return \code{NewCond} for a n time-step.
#' @export
#' @examples
#' RootDevelopment(Crop, Soil, GroundWater, InitCond, GDD, GrowingSeason)


RootDevelopment <- function(Crop, Soil, GroundWater, InitCond, GDD, GrowingSeason){
 

    ## Store initial conditions for updating ##
    cc <- list()
    NewCond <- InitCond


    ## Calculate root expansion (if in growing season) ##
    if (GrowingSeason == TRUE){
        # If today is first day of season, root depth is equal to minimum depth
        if (NewCond$DAP == 1){
            InitCond$Zroot <- Crop$Zmin
        }
        # Adjust time for any delayed development
        if (Crop$CalendarType == 1){
            tAdj <- NewCond$DAP-NewCond$DelayedCDs
        } else if (Crop$CalendarType == 2){
            tAdj <- NewCond$GDDcum-NewCond$DelayedGDDs
        }
        # Calculate root expansion #
        Zini <- Crop$Zmin*(Crop$PctZmin/100)
        t0 <- round((Crop$Emergence/2))
        tmax <- Crop$MaxRooting
        if (Crop$CalendarType == 1){
            tOld <- tAdj-1
        } else if (Crop$CalendarType == 2){
            tOld <- tAdj-GDD
        }

        # Potential root depth on previous day
        if (tOld >= tmax){
            ZrOld <- Crop$Zmax
        } else if (tOld <= t0){
            ZrOld <- Zini
        } else {
            X <- (tOld-t0)/(tmax-t0)

            ZrOld <- Zini+(Crop$Zmax-Zini) * (X^(1/Crop$fshape_r))
        }
        if (ZrOld < Crop$Zmin){
            ZrOld <- Crop$Zmin
        }

        # Potential root depth on current day
        if (tAdj >= tmax){
            Zr <- Crop$Zmax
        } else if (tAdj <= t0){
            Zr <- Zini
        } else {

            X <- (tAdj-t0)/(tmax-t0)

            # NOTE change function
            Zr <- Zini + (Crop$Zmax-Zini) * (X^(1/Crop$fshape_r))
        }
        if (Zr < Crop$Zmin){
            Zr <- Crop$Zmin
        }
        # Determine rate of change
        dZr <- Zr-ZrOld
        # Adjust rate of expansion for any stomatal water stress
        if (NewCond$TrRatio < 0.9999){
            if (Crop$fshape_ex >= 0){
                dZr <- dZr*NewCond$TrRatio
            } else {
                fAdj <- (exp(NewCond$TrRatio*Crop$fshape_ex)-1)/(exp(Crop$fshape_ex)-1)
                dZr <- dZr*fAdj
            }
        }
        # Adjust root expansion for failure to germinate (roots cannot expand
        # if crop has not germinated)
        if (InitCond$Germination == FALSE){
            dZr <- 0
        }

        # Get new rooting depth
        NewCond$Zroot <- InitCond$Zroot+dZr

        # Adjust root depth if restrictive soil layer is present that limits
        # depth of root expansion
        if (Soil$zRes > 0){
            if(NewCond$Zroot > Soil$zRes){
                NewCond$rCor <- (2*(NewCond$Zroot/Soil$zRes)*((Crop$SxTop+Crop$SxBot)/2)
                    -Crop$SxTop)/Crop$SxBot
                NewCond$Zroot <- Soil$zRes
            }
        }



        # Limit rooting depth if groundwater table is present (roots cannot
        # develop below the water table)
        if((GroundWater$WaterTable == 1) & (NewCond$zGW > 0)){

            if (NewCond$Zroot > NewCond$zGW){
                NewCond$Zroot <- NewCond$zGW
                if(NewCond$Zroot < Crop$Zmin){
                    NewCond$Zroot <- Crop$Zmin
                }
            }
        }
    } else {
        # No root system outside of the growing season
        NewCond$Zroot <- 0
    }

    return(NewCond)
}
