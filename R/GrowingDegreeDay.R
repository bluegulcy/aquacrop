#' Calculate number of growing degree days on current day.
#' @param Crop structure
#' @param InitCond initial crop settings
#' @param Tmax max temperature for n time-step
#' @param Tmin min temperature for n time-step
#' @return list with \code{NewCond} and \code{GDD} for n time-step
#' @export
#' @examples
#' GrowingDegreeDay(Crop, InitCond, Tmax, Tmin)

GrowingDegreeDay <- function(Crop, InitCond, Tmax, Tmin){

## Store initial conditions for updating ##
    cc <- list()
    NewCond <- InitCond

    ## Calculate GDDs ##
    if(Crop$GDDmethod == 1){
        # Method 1
        Tmean <- (Tmax + Tmin)/2
        Tmean[Tmean > Crop$Tupp] <- Crop$Tupp
        Tmean[Tmean < Crop$Tbase] <- Crop$Tbase
        GDD <- Tmean - Crop$Tbase
    } else if(Crop$GDDmethod == 2){
        # Method 2
        Tmax[Tmax > Crop$Tupp] <- Crop$Tupp
        Tmax[Tmax < Crop$Tbase] <- Crop$Tbase
        Tmin[Tmin > Crop$Tupp] <- Crop$Tupp
        Tmin[Tmin < Crop$Tbase] <- Crop$Tbase
        Tmean <- (Tmax + Tmin)/2
        GDD <- Tmean - Crop$Tbase
    } else if(Crop$GDDmethod == 3){
        # Method 3
        Tmax[Tmax > Crop$Tupp] <- Crop$Tupp
        Tmax[Tmax < Crop$Tbase] <- Crop$Tbase
        Tmin[Tmin > Crop$Tupp] <- Crop$Tupp
        Tmean <- (Tmax+Tmin)/2
        Tmean[Tmean < Crop$Tbase] <- Crop$Tbase
        GDD <- Tmean - Crop$Tbase
    }

    ## Update cumulative GDD counter ##
    NewCond$GDDcum <- InitCond$GDDcum + GDD
    
    cc$NewCond <- NewCond
    cc$GDD <- GDD
    return(cc)

}

