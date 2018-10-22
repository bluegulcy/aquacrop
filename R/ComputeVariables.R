#' Compute additional variables needed to run AquaCrop
#' @param ParamStruct Crop Structure
#' @param Weather Weather data
#' @param ClockStruct Crop calendar
#' @param GwStruct Ground water table
#' @param CropChoices Crops to be analysed
#' @param FileLocation list with file locations
#' @return \code{ParamStruct}.
#' @examples
#' ComputeVariables(ParamStruct, Weather, ClockStruct, GwStruct, CropChoices, FileLocation)
#'
ComputeVariables <- function(ParamStruct, Weather, ClockStruct, GwStruct, CropChoices, FileLocation){

    ## Compute water contents and saturated hydraulic conductivity ##
    if(ParamStruct[['Soil']][['CalcSHP']] == 0){
        # Read soil texture file
        filename <- paste(FileLocation[['Input']],
                          ParamStruct$Soil[['SoilHydrologyFilename']], sep='')
        DataArray <- convert_list_2numeric(check_xml_exist(filename))
        # Assign data
        ParamStruct$Soil$Layer$dz <- DataArray$LayerThickness
        ParamStruct$Soil$Layer$th_s <- DataArray$thS
        ParamStruct$Soil$Layer$th_fc <- DataArray$thFC
        ParamStruct$Soil$Layer$th_wp <- DataArray$thWP
        ParamStruct$Soil$Layer$Ksat <- DataArray$Ksat
        # Calculate additional variables
        ParamStruct$Soil$Layer$th_dry <- ParamStruct$Soil$Layer$th_wp/2
    } else if (ParamStruct$Soil$CalcSHP == 1){
        # Read soil texture file
        filename <- paste(FileLocation[['Input']], ParamStruct$Soil
                          [['SoilTextureFilename']], sep='')
        DataArray <- convert_list_2numeric(check_xml_exist(filename))

        # Create soil dz vector
        ParamStruct$Soil$Layer$dz <- DataArray$Thickness
        ParamStruct$Soil$Layer$Sand <- DataArray$Sand/100
        ParamStruct$Soil$Layer$Clay <-  DataArray$Clay/100
        ParamStruct$Soil$Layer$OrgMat <-  DataArray$OrgMat
        ParamStruct$Soil$Layer$DF <-  DataArray$DensityFactor
        # Calculate soil hydraulic properties using pedotransfer function
        # method (Saxton et al., 2006)
        #[thdry,thwp,thfc,ths,ksat]


        SHcP<- SoilHydraulicProperties(ParamStruct$Soil)
        thdry <- SHcP$thdry
        thwp <- SHcP$thwp
        thfc <- SHcP$thfc
        ths <- SHcP$ths
        ksat <- SHcP$ksat

        ParamStruct$Soil$Layer$th_dry <- thdry
        ParamStruct$Soil$Layer$th_wp <- thwp
        ParamStruct$Soil$Layer$th_fc <- thfc
        ParamStruct$Soil$Layer$th_s <- ths
        ParamStruct$Soil$Layer$Ksat <- ksat
    }


    ## Assign field capacity values to each soil compartment ##
    for (ii in 1:ParamStruct$Soil$nComp){
        layeri <- ParamStruct$Soil$Comp$Layer[ii]
        ParamStruct$Soil$Comp$th_fc[ii] <- ParamStruct$Soil$Layer$th_fc[layeri]
    }

    ## Calculate capillary rise parameters for all soil layers ##
    # Only do calculation if water table is present. Calculations use equations
    # described in Raes et al. (2012)
    if(GwStruct[['WaterTable']] == 1){
        aCR <- rep(1, ParamStruct$Soil$nLayer)
        bCR <- rep(1, ParamStruct$Soil$nLayer)
        for (ii in 1:ParamStruct$Soil$nLayer){
            thwp <- ParamStruct$Soil$Layer$th_wp[ii]
            thfc <- ParamStruct$Soil$Layer$th_fc[ii]
            ths <- ParamStruct$Soil$Layer$th_s[ii]
            Ksat <- ParamStruct$Soil$Layer$Ksat[ii]
            if(thwp >= 0.04 && thwp <= 0.15 && thfc >= 0.09 &&
              thfc <= 0.28 && ths >= 0.32 && ths <= 0.51) {
                # Sandy soil class
                if (Ksat >= 200 && Ksat <= 2000){
                    aCR[ii] <- -0.3112-(Ksat*(10^-5))
                    bCR[ii] <- -1.4936+(0.2416*log(Ksat))
                } else if(Ksat < 200) {
                    aCR[ii] <- -0.3112-(200*(10^-5))
                    bCR[ii] <- -1.4936+(0.2416*log(200))
                } else if (Ksat > 2000){
                    aCR[ii] <- -0.3112-(2000*(10^-5))
                    bCR[ii] <- -1.4936+(0.2416*log(2000))
                }
            } else if (thwp >= 0.06 && thwp <= 0.20 && thfc >= 0.23 &&
                    thfc <= 0.42 && ths >= 0.42 && ths <= 0.55){
                # Loamy soil class
                if(Ksat >= 100 && Ksat <= 750){
                    aCR[ii] <- -0.4986+(9*(10^-5)*Ksat)
                    bCR[ii] <- -2.132+(0.4778*log(Ksat))
                } else if (Ksat < 100){
                    aCR[ii] <- -0.4986+(9*(10^-5)*100)
                    bCR[ii] <- -2.132+(0.4778*log(100))
                } else if (Ksat > 750){
                    aCR[ii] <- -0.4986+(9*(10^-5)*750)
                    bCR[ii] <- -2.132+(0.4778*log(750))
                }
            } else if (thwp >= 0.16 && thwp <= 0.34 && thfc >= 0.25 &&
                    thfc <= 0.45 && ths >= 0.40 && ths <= 0.53){
                # Sandy clayey soil class
                if (Ksat >= 5 && Ksat <= 150){
                    aCR[ii] <- -0.5677-(4*(10^-5)*Ksat)
                    bCR[ii] <- -3.7189+(0.5922*log(Ksat))
                } else if (Ksat < 5){
                    aCR[ii] <- -0.5677-(4*(10^-5)*5)
                    bCR[ii] <- -3.7189+(0.5922*log(5))
                } else if (Ksat > 150){
                    aCR[ii] <- -0.5677-(4*(10^-5)*150)
                    bCR[ii] <- -3.7189+(0.5922*log(150))
                }
            } else if(thwp >= 0.20 && thwp <= 0.42 && thfc >= 0.40 &&
                    thfc <= 0.58 && ths >= 0.49 && ths <= 0.58) {
                # Silty clayey soil class
                if (Ksat >= 1 && Ksat <= 150){
                    aCR[ii] <- -0.6366+(8*(10^-4)*Ksat)
                    bCR[ii] <- -1.9165+(0.7063*log(Ksat))
                } else if(Ksat < 1){
                    aCR[ii] <- -0.6366+(8*(10^-4)*1)
                    bCR[ii] <- -1.9165+(0.7063*log(1))
                } else if (Ksat > 150){
                    aCR[ii] <- -0.6366+(8*(10^-4)*150)
                    bCR[ii] <- -1.9165+(0.7063*log(150))
                }
            }
        }
        ParamStruct$Soil$Layer$aCR <- aCR
        ParamStruct$Soil$Layer$bCR <- bCR
    }


    ## Calculate drainage characteristic (tau) ##
    # Calculations use equation given by Raes et al. 2012
    for (ii in 1:ParamStruct$Soil$nLayer){
        ParamStruct$Soil$Layer$tau[ii] <- 0.0866*(ParamStruct$Soil$Layer$Ksat[ii]^0.35)
        ParamStruct$Soil$Layer$tau[ii] <- round((100*ParamStruct$Soil$Layer$tau[ii]))/100
        if (ParamStruct$Soil$Layer$tau[ii] > 1){
           ParamStruct$Soil$Layer$tau[ii] <- 1
        } else if (ParamStruct$Soil$Layer$tau[ii] < 0){
            ParamStruct$Soil$Layer$tau[ii] <- 0
        }
    }

    ## Calculate readily evaporable water in surface layer ##
    if (ParamStruct$Soil$AdjREW == 0){
        ParamStruct$Soil$REW <- round((1000*(ParamStruct$Soil$Layer$th_fc[1]-
            ParamStruct$Soil$Layer$th_dry[1])*ParamStruct$Soil$EvapZsurf))
    }

    ## Calculate upper and lower curve numbers ##
    ParamStruct$Soil$CNbot <- round(1.4*(exp(-14*log(10)))+(0.507*ParamStruct$Soil$CN) -
        (0.00374*ParamStruct$Soil$CN^2)+(0.0000867*ParamStruct$Soil$CN^3))
    ParamStruct$Soil$CNtop <- round(5.6*(exp(-14*log(10)))+(2.33*ParamStruct$Soil$CN)-
        (0.0209*ParamStruct$Soil$CN^2)+(0.000076*ParamStruct$Soil$CN^3))

    ## Fit function relating water content to curve number ##
    # Use properties of top soil layer
    xi <- c(ParamStruct$Soil$Layer$th_wp[1], ((ParamStruct$Soil$Layer$th_fc[1] +
        ParamStruct$Soil$Layer$th_wp[1])/2), ParamStruct$Soil$Layer$th_fc[1])
    yi <- c(ParamStruct$Soil$CNbot,ParamStruct$Soil$CN,ParamStruct$Soil$CNtop)

        # FIXME = check equations
    ParamStruct$Soil$CNf <- pchip(xi,yi, x=seq(xi[1], xi[length(xi)], by = 0.01))


    ## Calculate additional parameters for all crop types in mix ##
    CropNames <- names(ParamStruct$Crop)
    nCrops <- size(CropNames)[1]
    for(ii in 1:nCrops){
        # Fractional canopy cover size at emergence

   
        ParamStruct$Crop[[CropNames[ii]]]$CC0 <- round(10000 *
            (ParamStruct$Crop[[CropNames[ii]]]$PlantPop *
               ParamStruct$Crop[[CropNames[ii]]]$SeedSize) * 10^-8)/10000
        # Root extraction terms
        SxTopQ <- ParamStruct$Crop[[CropNames[ii]]]$SxTopQ
        SxBotQ <- ParamStruct$Crop[[CropNames[ii]]]$SxBotQ
        S1 <- ParamStruct$Crop[[CropNames[ii]]]$SxTopQ
        S2 <- ParamStruct$Crop[[CropNames[ii]]]$SxBotQ
        if (S1 == S2){
            SxTop <- S1
            SxBot <- S2
        } else {
            if (SxTopQ < SxBotQ){
                S1 <- SxBotQ
                S2 <- SxTopQ
            }
            xx <- 3*(S2/(S1-S2))
            if (xx < 0.5){
                SS1 <- (4/3.5)*S1
                SS2 <- 0
            } else {
                SS1 <- (xx+3.5)*(S1/(xx+3))
                SS2 <- (xx-0.5)*(S2/xx)
            }
            if (SxTopQ > SxBotQ){
                SxTop <- SS1
                SxBot <- SS2
            } else {
                SxTop <- SS2
                SxBot <- SS1
            }
        }
        ParamStruct$Crop[[CropNames[ii]]]$SxTop <- SxTop
        ParamStruct$Crop[[CropNames[ii]]]$SxBot <- SxBot

        # Water stress thresholds
        ParamStruct$Crop[[CropNames[ii]]]$p_up <- 
            c(ParamStruct$Crop[[CropNames[ii]]]$p_up1,
            ParamStruct$Crop[[CropNames[ii]]]$p_up2, ParamStruct$Crop[[CropNames[ii]]]$p_up3,
            ParamStruct$Crop[[CropNames[ii]]]$p_up4)
        ParamStruct$Crop[[CropNames[ii]]]$p_lo <- 
            c(ParamStruct$Crop[[CropNames[ii]]]$p_lo1,
            ParamStruct$Crop[[CropNames[ii]]]$p_lo2,ParamStruct$Crop[[CropNames[ii]]]$p_lo3,
            ParamStruct$Crop[[CropNames[ii]]]$p_lo4)
        ParamStruct$Crop[[CropNames[ii]]]$fshape_w <- 
            c(ParamStruct$Crop[[CropNames[ii]]]$fshape_w1,
            ParamStruct$Crop[[CropNames[ii]]]$fshape_w2,ParamStruct$Crop[[CropNames[ii]]]$fshape_w3,
            ParamStruct$Crop[[CropNames[ii]]]$fshape_w4)
        fields <- c('p_up1','p_up2','p_up3','p_up4','p_lo1','p_lo2','p_lo3',
            'p_lo4','fshape_w1','fshape_w2','fshape_w3','fshape_w4')

        # FIXME: Check this deleting
        #i <- match(fields, names(ParamStruct$Crop[[CropNames[ii]]]))
        #ParamStruct$Crop[[CropNames[ii]]] <- ParamStruct$Crop[[CropNames[ii]]][-i]




        # Flowering function

        if(ParamStruct$Crop[[CropNames[ii]]]$CropType == 3){

            ParamStruct$Crop[[CropNames[ii]]]$flowerfun  <- flowerfun(xx)

        }

        ParamStruct$Crop[[CropNames[ii]]] <-
            ComputeCropCalendar(ParamStruct$Crop[[CropNames[ii]]], CropNames[ii],
            CropChoices, Weather, ClockStruct)

        # Harvest index growth coefficient
        ParamStruct$Crop[[CropNames[ii]]]$HIGC <- 
            CalculateHIGC(ParamStruct$Crop[[CropNames[ii]]])

        # Days to linear HI switch point
        if(ParamStruct$Crop[[CropNames[ii]]]$CropType == 3){

            # Determine linear switch point and HIGC rate for fruit/grain crops
            HILinear <- CalculateHILinear(ParamStruct$Crop[[CropNames[ii]]])
            tLin <- HILinear$tSwitch
            HIGClin <- HILinear$dHILin

            ParamStruct$Crop[[CropNames[ii]]]$tLinSwitch <- tLin
            ParamStruct$Crop[[CropNames[ii]]]$dHILinear <- HIGClin
        } else {
            # No linear switch for leafy vegetable or root/tiber crops
            ParamStruct.Crop[[CropNames[ii]]]$tLinSwitch <- c()
            ParamStruct.Crop[[CropNames[ii]]]$dHILinear <- c()
        }
    } # end for loop

    

    ## Calculate WP adjustment factor for elevation in CO2 concentration ##
    # Load CO2 data
    filename <- paste(FileLocation$Input, FileLocation$CO2Filename, sep = '')
    CO2Data <- check_file_exist(filename)

    # Years
    Yrs <- CO2Data$year
    # CO2 concentrations (ppm)
    CO2 <- CO2Data$co2


    # Interpolate data
    StaYr <- as_date_list(ClockStruct$SimulationStartDate, '0000-01-01')
    EndYr <- as_date_list(ClockStruct$SimulationEndDate, '0000-01-01')
    YrsVec <- as.numeric(StaYr[[1]][1]):as.numeric(EndYr[[1]][1])
    CO2conc <- approx(Yrs, CO2, YrsVec)$y
    # Store data
    ParamStruct$CO2$Data <- cbind(YrsVec, CO2conc)

    # Define reference CO2 concentration
    ParamStruct$CO2$RefConc <- 369.41

    # Get CO2 concentration for first year
    Yri <- as_date_list(ClockStruct$SimulationStartDate, '0000-01-01')
    ParamStruct$CO2$CurrentConc <- as.numeric(ParamStruct$CO2$Data[(ParamStruct$CO2$Data[,1] ==
                                                           as.numeric(Yri[[1]][1])),2])

    # Get CO2 weighting factor for first year
    CO2ref <- ParamStruct$CO2$RefConc
    CO2conc <- ParamStruct$CO2$CurrentConc
    if (CO2conc <= CO2ref){
        fw <- 0
    } else {
        if (CO2conc >= 550){
            fw <- 1
        } else {
            fw <- 1-((550-CO2conc)/(550-CO2ref))
        }
    }



    # Determine adjustment for each crop in first year of simulation
    for (ii in 1:nCrops){
    # Determine initial adjustment
        fCO2 <- (CO2conc / CO2ref)/(1 + (CO2conc - CO2ref) * ((1 - fw) *
                ParamStruct$Crop[[CropNames[ii]]]$bsted + fw *
                  (( as.numeric(ParamStruct$Crop[[CropNames[ii]]]$bsted) *
        ParamStruct$Crop[[CropNames[ii]]]$fsink) +
          ParamStruct$Crop[[CropNames[ii]]]$bface *
        (1 - ParamStruct$Crop[[CropNames[ii]]]$fsink ))))
        # Consider crop type
        if (as.numeric(ParamStruct$Crop[[CropNames[ii]]]$WP) >= 40){
            # No correction for C4 crops
            ftype <- 0
        } else if (ParamStruct$Crop[[CropNames[ii]]]$WP <= 20){
            # Full correction for C3 crops
            ftype <- 1
        } else {
            ftype <- (40 - ParamStruct$Crop[[CropNames[ii]]]$WP) / (40-20)
        }
    # Total adjustment
        ParamStruct$Crop[[CropNames[ii]]]$fCO2 <- 1 + ftype * (fCO2-1)
    }

    return(ParamStruct)

}

