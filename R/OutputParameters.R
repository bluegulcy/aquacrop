#' OutputParameters to be provided in at the end of the simulation
#' @format A data structure with the following four sublists will be provided as
#' outputs of the simulation:
#' \describe{
#'\item{Water contents output file}{The water contents list reports the simulated 
#'water content (m3 m-3) in each soil compartment at the end of each simulation 
#'day. A variable, 'Season', is also reported that takes a value of 1 on days 
#'during a growing season, and a value of 0 on days outside a growing season. 
#'Note that if the soil water balance is not simulated in the off-season, water 
#'contents on these days will be denoted by zero values}
#'\item{Water fluxes output file}{The water fluxes list provides various 
#'simulated water fluxes and states on each simulation day, including:
#'\describe{
#' \item{wRZ}{Water in the crop root zone (mm)}
#' \item{zGW}{Water table depth (m). A value of -999 indicates no groundwater 
#' table was considered}
#' \item{wSurf}{Ponded water (mm)}
#' \item{Irr}{Irrigation (mm)}
#' \item{Infl}{Infiltration (mm)}
#' \item{DP}{Deep percolation below the base of the soil profile (mm)}
#' \item{CR}{Capillary rise in to the soil profile (mm)}
#' \item{GWin}{Horizontal groundwater inflow to the soil profile (mm)}
#' \item{Es}{Soil evaporation (mm)}
#' \item{EsX}{Potential soil evaporation (mm)}
#' \item{Tr}{Crop transpiration (mm)}
#' \item{TrX}{Potential crop transpiration (mm)}
#' As previously noted, the variable 'Season' denotes whether a growing season 
#' was active on a given day and values of zero are assigned to all 
#' fluxes/states outside of the growing season if the off-season soil water 
#' balance is not simulated.
#'}
#'}
#' \item{Crop growth output file}{The crop growth output list reports various 
#' simulated aspects of crop development on each simulation day, including:
#' \describe{
#' \item{GDD}{Number of growing degree days on the current day}
#' \item{TotGDD}{Cumulative growing degree days in the current season}
#' \item{RootDepth}{Crop effective rooting depth (m)}
#' \item{CC}{Fractional canopy cover}
#' \item{RefCC}{Fractional canopy cover under no water-stress conditions}
#' \item{Bio}{Accumulated aboveground biomass (g m-2)}
#' \item{RefBio}{Accumulated aboveground biomass under no water stress 
#' conditions (g m-2)}
#' \item{HI}{Fractional reference harvest index}
#' \item{HIadj}{Fractional harvest index adjusted for water stress effects}
#' \item{Yield}{Crop yield (tonne ha-1)}
#'}
#' As previously noted, the variable 'Season' denotes whether a growing season 
#' was active on a given day.
#'}
#' \item{Final output file}{The final output list reports summaries and totals 
#' of key simulated variables in each growing season, including:}
#' \describe{
#' \item{PlantD}{Calendar planting date (dd/mm/yyyy)}
#' \item{PlantSD}{Simulation day of planting}
#' \item{HarvestD}{Calendar harvest date (dd/mm/yyyy)}
#' \item{HarvestSD}{Simulation day of harvesting}
#' \item{Yield}{Final crop yield (tonne ha-1)}
#' \item{TotIrr}{Total irrigation use (mm)}
#'}
#'}


OutputParameters <- function() { 

}