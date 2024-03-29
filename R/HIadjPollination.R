#' Calculate adjustment to harvest index for failure of
#' pollination due to water or temperature stress
#' @param Crop Parameters for a given crop
#' @param InitCond Crop setting initial structure
#' @param Ksw Water stress
#' @param Kst Temperature stress
#' @param HIt Harvest index on current day
#' @return with \code{NewCond} for a n time-step.
#' @export
#' @examples
#' HIadjPollination(InitCond, Crop, Ksw, Kst, HIt)

HIadjPollination <- function(InitCond, Crop, Ksw, Kst, HIt){


    ## Store initial conditions for updating ##
    NewCond <- InitCond

    ## Caclulate harvest index adjustment for pollination ##
    # Get fractional flowering
    if (HIt == 0){
        # No flowering yet
        FracFlow <- 0
    } else if (HIt > 0){
        # Fractional flowering on previous day
        t1 <- HIt-1
        if (t1 == 0){
            F1 <- 0
        } else {
            t1Pct <- 100*(t1/Crop$FloweringCD)
            if (t1Pct > 100){
                t1Pct <- 100
            }
            F1 <- 0.00558*exp(0.63*log(t1Pct))-(0.000969*t1Pct)-0.00383
        }
        if (F1 < 0){
            F1 <- 0
        }
        # Fractional flowering on current day
        t2 <- HIt
        if (t2 == 0){
            F2 <- 0
        } else {
            t2Pct <- 100*(t2/Crop$FloweringCD)
            if (t2Pct > 100){
                t2Pct <- 100
            }
            F2 <- 0.00558*exp(0.63*log(t2Pct))-(0.000969*t2Pct)-0.00383
        }
        if (F2 < 0){
            F2 <- 0
        }
        # Weight values
        if (abs(F1-F2) < 0.0000001){
            F <- 0
        } else {
            F <- 100*((F1+F2)/2)/Crop$FloweringCD
        }
        FracFlow <- F
    }
    # Calculate pollination adjustment for current day
    if (InitCond$CC < Crop$CCmin){
        # No pollination can occur as canopy cover is smaller than minimum
        # threshold
        dFpol <- 0
    } else {
        Ks <- min(Ksw$Pol, Kst$PolC, Kst$PolH)
        dFpol <- Ks*FracFlow*(1+(Crop$exc/100))
    }

    # Calculate pollination adjustment to date
    NewCond$Fpol <- InitCond$Fpol + dFpol
    if (NewCond$Fpol > 1){
        # Crop has fully pollinated
        NewCond$Fpol <- 1
    }


    return(NewCond)
}

