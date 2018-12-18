#' Calculate actual and total available water in the root
#  zone at current time step
#' @param InitCond Crop setting initial structure
#' @param Crop parameters for a given crop
#' @param Soil properties of soil
#' @return list with \code{Wr}, \code{Dr},  \code{TAW} and \code{thRZ} for a n time-step.
#' @export
#' @examples
#' RootZoneWater(Soil, Crop, InitCond)

RootZoneWater <- function(Soil, Crop, InitCond){
    

    ## Calculate root zone water content and available water ##
    # Compartments covered by the root zone
    cc <- list()
    rootdepth <- max(InitCond$Zroot, Crop$Zmin)
    rootdepth <- round((rootdepth*100))/100

    comp_sto <- sum(Soil$Comp$dzsum < rootdepth)
    # Initialise counters
    Wr <- 0
    WrS <- 0
    WrFC <- 0
    WrWP <- 0
    WrDry <- 0
    WrAer <- 0


    for(ii in 1: comp_sto){

        # Specify layer
        layeri <- Soil$Comp$Layer[ii]


        # Fraction of compartment covered by root zone
        if (Soil$Comp$dzsum[ii] > rootdepth){
            factor <- 1-((Soil$Comp$dzsum[ii] - rootdepth)/Soil$Comp$dz[ii])
        } else {
            factor <- 1
        }
        # Actual water storage in root zone (mm)
        Wr <- Wr+(factor*1000*InitCond$th[ii]*Soil$Comp$dz[ii])
        # Water storage in root zone at saturation (mm)
        WrS <- WrS+(factor*1000*Soil$Layer$th_s[layeri]*Soil$Comp$dz[ii])
        # Water storage in root zone at field capacity (mm)
        WrFC <- WrFC+(factor*1000*Soil$Layer$th_fc[layeri]*Soil$Comp$dz[ii])
        # Water storage in root zone at permanent wilting point (mm)
        WrWP <- WrWP+(factor*1000*Soil$Layer$th_wp[layeri]*Soil$Comp$dz[ii])
        # Water storage in root zone at air dry (mm)
        WrDry <- WrDry+(factor*1000*Soil$Layer$th_dry[layeri]*Soil$Comp$dz[ii])
        # Water storage in root zone at aeration stress threshold (mm)
        WrAer <- WrAer+(factor*1000*(Soil$Layer$th_s[layeri]-(Crop$Aer/100))*Soil$Comp$dz[ii])

    }

    if(Wr < 0){
        Wr <- 0
    }


    thRZ <- list()
    # Actual root zone water content (m3/m3)
    thRZ$Act <- Wr/(rootdepth*1000)
    # Root zone water content at saturation (m3/m3)
    thRZ$Sat <- WrS/(rootdepth*1000)
    # Root zone water content at field capacity (m3/m3)
    thRZ$Fc <- WrFC/(rootdepth*1000)
    # Root zone water content at permanent wilting point (m3/m3)
    thRZ$Wp <- WrWP/(rootdepth*1000)
    # Root zone water content at air dry (m3/m3)
    thRZ$Dry <- WrDry/(rootdepth*1000)
    # Root zone water content at aeration stress threshold (m3/m3)
    thRZ$Aer <- WrAer/(rootdepth*1000)

    ## Calculate total available water (mm) ##
    TAW <- WrFC - WrWP
    if (TAW < 0){
        TAW <- 0
    }

    ## Calculate depletion (mm) ##
    Dr <- WrFC - Wr
    
    if (Dr < 0){
        Dr <- 0
    }

    cc$Wr <- Wr
    cc$Dr <- Dr
    cc$TAW <- TAW
    cc$thRZ <- thRZ

    return(cc)
}

