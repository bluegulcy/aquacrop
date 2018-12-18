#' Simulate canopy growth/decline
#' @param Crop Parameters for a given crop
#' @param Soil properties of soil
#' @param InitCond Crop setting initial structure
#' @param GDD Growing degree days
#' @param Et0 Evapotranspiration
#' @param GrowingSeason crop developmental stage
#' @return \code{NewCond} for a n time-step.
#' @export
#' @examples
#' CanopyCover(Crop, Soil, InitCond, GDD, Et0, GrowingSeason)


CanopyCover <- function(Crop, Soil, InitCond, GDD, Et0, GrowingSeason){

    ## Store initial conditions in a new structure for updating ##
    NewCond <- InitCond
    NewCond$CCprev <- InitCond$CC

    ## Calculate canopy development (if in growing season) ##
    if(GrowingSeason == TRUE){
        GD <- RootZoneWater(Soil,Crop, NewCond)
        Wr <- GD$Wr
        Dr <- GD$Dr
        TAW <- GD$TAW
        thRZ <- GD$thRZ
        # Calculate root zone water content


        # Determine if water stress is occurring
        beta <- TRUE
        Ksw <- WaterStress(Crop, NewCond, Dr, TAW, Et0, beta)

        # Get canopy cover growth time
        if (Crop$CalendarType == 1){
            tCC <- NewCond$DAP
            dtCC <- 1
            tCCadj <- NewCond$DAP-NewCond$DelayedCDs
        } else if (Crop$CalendarType == 2){
            tCC <- NewCond$GDDcum
            dtCC <- GDD
            tCCadj <- NewCond$GDDcum - NewCond$DelayedGDDs
        }

        ## Canopy development (potential) ##
        if((tCC < Crop$Emergence) | (round(tCC) > Crop$Maturity)){
            # No canopy development before emergence/germination or after
            # maturity
            NewCond$CC_NS <- 0
        } else if (tCC < Crop$CanopyDevEnd){
            # Canopy growth can occur
            if (InitCond$CC_NS <= Crop$CC0){
                # Very small initial CC as it is first day or due to senescence.
                # In this case, assume no leaf expansion stress
                NewCond$CC_NS <- Crop$CC0*exp(Crop$CGC*dtCC)
            } else {
                # Canopy growing
                tmp_tCC <- tCC-Crop$Emergence
                NewCond$CC_NS <- CCDevelopment(Crop$CC0,Crop$CCx,
                    Crop$CGC,Crop$CDC,tmp_tCC,'Growth')
            }
            # Update maximum canopy cover size in growing season
            NewCond$CCxAct_NS <- NewCond$CC_NS
        } else if (tCC > Crop$CanopyDevEnd){
            # No more canopy growth is possible or canopy in decline
            # Set CCx for calculation of withered canopy effects
            NewCond$CCxW_NS <- NewCond$CCxAct_NS
            if (tCC < Crop$Senescence){
                # Mid-season stage - no canopy growth
                NewCond$CC_NS <- InitCond$CC_NS
                # Update maximum canopy cover size in growing season
                NewCond$CCxAct_NS <- NewCond$CC_NS
            } else {
                # Late-season stage - canopy decline
                tmp_tCC <- tCC-Crop$Senescence
                NewCond$CC_NS <- CCDevelopment(Crop$CC0,Crop$CCx,
                    Crop$CGC,Crop$CDC,tmp_tCC,'Decline')
            }
        }

        ## Canopy development (actual) ##
        if ((tCCadj < Crop$Emergence) | (round(tCCadj) > Crop$Maturity)) {
            # No canopy development before emergence/germination or after
            # maturity
            NewCond$CC <- 0
        } else if (tCCadj < Crop$CanopyDevEnd){
            #print('canopy')
            # Canopy growth can occur
            if (InitCond$CC <= NewCond$CC0adj){
                # Very small initial CC as it is first day or due to senescence. In
                # this case, assume no leaf expansion stress
                NewCond$CC <- NewCond$CC0adj*exp(Crop$CGC*dtCC)
            } else {
                # Canopy growing
                if (InitCond$CC >= (0.9799*Crop$CCx)){
                    # Canopy apprAOShing maximum size
                    tmp_tCC <- tCC-Crop$Emergence
                    NewCond$CC <- CCDevelopment(Crop$CC0,Crop$CCx,
                        Crop$CGC, Crop$CDC,tmp_tCC, 'Growth')
                    NewCond$CC0adj <- Crop$CC0
                } else {
                    # Adjust canopy growth coefficient for leaf expansion water
                    # stress effects
                    CGCadj <- Crop$CGC*Ksw$Exp
                    if (CGCadj > 0){
                        # Adjust CCx for change in CGC
                        CCXadj <- AdjustCCx(InitCond$CC, NewCond$CC0adj, Crop$CCx,
                            CGCadj, Crop$CDC, dtCC, tCCadj, Crop)
                        if (CCXadj > 0){
                            if (abs(InitCond$CC-Crop$CCx) < 0.00001){
                                # ApprAOShing maximum canopy cover size
                                tmp_tCC <- tCC-Crop$Emergence
                                NewCond$CC <- CCDevelopment(Crop$CC0,Crop$CCx,
                                    Crop$CGC,Crop$CDC,tmp_tCC,'Growth')
                            } else {
                                # Determine time required to reach CC on previous,
                                # day, given CGCAdj value
                                tReq <- CCRequiredTime(InitCond$CC,NewCond$CC0adj,
                                    CCXadj,CGCadj,Crop$CDC,dtCC,tCCadj,'CGC')
                                # Calclate GDD's for canopy growth
                                tmp_tCC <- tReq+dtCC
                                if (tmp_tCC > 0){
                                    # Determine new canopy size
                                    NewCond$CC <- CCDevelopment(NewCond$CC0adj,CCXadj,
                                        CGCadj,Crop$CDC,tmp_tCC,'Growth')
                                } else {
                                    # No canopy growth
                                    NewCond$CC <- InitCond$CC
                                }
                            }
                        } else {
                            # No canopy growth
                            NewCond$CC <- InitCond$CC
                        }
                    } else {
                        # No canopy growth
                        NewCond$CC <- InitCond$CC
                        # Update CC0 if current canopy cover is less than
                        # initial canopy cover size at planting
                        if (NewCond$CC < NewCond$CC0adj){
                            NewCond$CC0adj <- NewCond$CC
                        }
                    }
                }
            }
            
            if (NewCond$CC > InitCond$CCxAct){
                # Update actual maximum canopy cover size during growing season
                NewCond$CCxAct <- NewCond$CC
            }
            
        } else if (tCCadj > Crop$CanopyDevEnd){
            # No more canopy growth is possible or canopy is in decline
            if (tCCadj < Crop$Senescence){
                # Mid-season stage - no canopy growth
                NewCond$CC <- InitCond$CC
                if (NewCond$CC > InitCond$CCxAct){
                    # Update actual maximum canopy cover size during growing
                    # season
                    NewCond$CCxAct <- NewCond$CC
                }
            } else {
                # Late-season stage - canopy decline
                # Adjust canopy decline coefficient for difference between actual
                # and potential CCx
                CDCadj <- Crop$CDC*(NewCond$CCxAct/Crop$CCx)
                # Determine new canopy size
                tmp_tCC <- tCCadj-Crop$Senescence
                NewCond$CC <- CCDevelopment(NewCond$CC0adj, NewCond$CCxAct,
                    Crop$CGC, CDCadj, tmp_tCC, 'Decline')
            }
            # Check for crop growth termination
            if((NewCond$CC < 0.001) & (InitCond$CropDead == FALSE)){
                # Crop has died
                NewCond$CC <- 0
                NewCond$CropDead <- TRUE
            }
        }

        ## Canopy senescence due to water stress (actual) ##
        if (tCCadj >= Crop$Emergence){
            if ((tCCadj < Crop$Senescence) | (InitCond$tEarlySen > 0)){
                # Check for early canopy senescence starting/continuing due to severe
                # water stress
                if (Ksw$Sen < 1){
                    # Early canopy senescence
                    NewCond$PrematSenes <- TRUE
                    if (InitCond$tEarlySen == 0){
                        # No prior early senescence
                        NewCond$CCxEarlySen <- InitCond$CC
                    }
                    # Increment early senescence GDD counter
                    NewCond$tEarlySen <- InitCond$tEarlySen+dtCC
                    # Adjust canopy decline coefficient for water stress
                    beta <- FALSE
                    Ksw <- WaterStress(Crop,NewCond,Dr,TAW,Et0,beta)
                    if (Ksw$Sen > 0.99999){
                        CDCadj <- 0.0001
                    } else {
                        CDCadj <- (1-(Ksw$Sen^8))*Crop$CDC
                    }
                    # Get new canpy cover size after senescence
                    if (NewCond$CCxEarlySen < 0.001){
                        CCsen <- 0
                    } else {
                        # Get time required to reach CC at end of previous day, given
                        # CDCadj
                        tReq <- CCRequiredTime(InitCond$CC,NewCond$CC0adj,
                            NewCond$CCxEarlySen,Crop$CGC,CDCadj,dtCC,tCCadj,'CDC')
                        # Calculate GDD's for canopy decline
                        tmp_tCC <- tReq+dtCC
                        # Determine new canopy size
                        CCsen <- CCDevelopment(NewCond$CC0adj,NewCond$CCxEarlySen,
                            Crop$CGC,CDCadj,tmp_tCC,'Decline')
                    }

                    # Update canopy cover size
                    if (tCCadj < Crop$Senescence){
                        # Limit CC to CCx
                        if (CCsen > Crop$CCx){
                           CCsen <- Crop$CCx
                        }
                        # CC cannot be greater than value on previous day
                        NewCond$CC <- CCsen
                        if (NewCond$CC > InitCond$CC){
                            NewCond$CC <- InitCond$CC
                        }
                        # Update maximum canopy cover size during growing
                        # season
                        NewCond$CCxAct <- NewCond$CC
                        # Update CC0 if current CC is less than initial canopy
                        # cover size at planting
                        if (NewCond$CC < Crop$CC0){
                            NewCond$CC0adj <- NewCond$CC
                        } else {
                            NewCond$CC0adj <- Crop$CC0

                        }
                    } else {
                        # Update CC to account for canopy cover senescence due
                        # to water stress
                        if (CCsen < NewCond$CC){
                            NewCond$CC <- CCsen
                        }
                    }
                    # Check for crop growth termination
                    if((NewCond$CC < 0.001) & (InitCond$CropDead == FALSE)){
                        # Crop has died
                        NewCond$CC <- 0
                        NewCond$CropDead <- TRUE
                    }
                } else {
                    # No water stress
                    NewCond$PrematSenes <- FALSE
                    if((tCCadj > Crop$Senescence) & (InitCond$tEarlySen > 0)){
                        # Rewatering of canopy in late season
                        # Get new values for CCx and CDC
                        tmp_tCC <- tCCadj-dtCC-Crop$Senescence

                        GC <- UpdateCCxCDC(InitCond$CC, Crop$CDC,Crop$CCx,tmp_tCC)
                        CCXadj <- GC$CCXadj
                        CDCadj <- GC$CDCadj

                        # Get new CC value for end of current day
                        tmp_tCC <- tCCadj - Crop$Senescence
                        NewCond$CC <- CCDevelopment(NewCond$CC0adj,CCXadj,
                                                        Crop$CGC,CDCadj,tmp_tCC,'Decline')
                        # Check for crop growth termination
                        if((NewCond$CC < 0.001) & (InitCond$CropDead == FALSE)){
                            NewCond$CC <- 0
                            NewCond$CropDead <- TRUE
                        }
                    }
                    # Reset early senescence counter
                    NewCond$tEarlySen <- 0
                }
                # Adjust CCx for effects of withered canopy
                if (NewCond$CC > InitCond$CCxW){
                    NewCond$CCxW <- NewCond$CC
                }
            }
        }

        ## Calculate canopy size adjusted for micro-advective effects ##
        # Check to ensure potential CC is not slightly lower than actual
        if (NewCond$CC_NS < NewCond$CC){
            NewCond$CC_NS <- NewCond$CC
            if (tCC < Crop$CanopyDevEnd){
                NewCond$CCxAct_NS <- NewCond$CC_NS
            }
        }
        # Actual (with water stress)
        NewCond$CCadj <- (1.72*NewCond$CC)-(NewCond$CC^2)+(0.3*(NewCond$CC^3))
        # Potential (without water stress)
        NewCond$CCadj_NS <- (1.72*NewCond$CC_NS)-(NewCond$CC_NS^2)
            +(0.3*(NewCond$CC_NS^3))
    } else {
        # No canopy outside growing season - set various values to zero
        NewCond$CC <- 0
        NewCond$CCadj <- 0
        NewCond$CC_NS <- 0
        NewCond$CCadj_NS <- 0
        NewCond$CCxW <- 0
        NewCond$CCxAct <- 0
        NewCond$CCxW_NS <- 0
        NewCond$CCxAct_NS <- 0
    }

    return(NewCond)
}
