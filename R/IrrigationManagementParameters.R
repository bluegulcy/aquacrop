#' Irrigation Management Parameters
#' @format The irrigation management file defines the input variables controlling 
#' irrigation practices in AquaCrop file in xml format should be provided with 
#' the following fields: 
#' \describe{
#' \item{IrrSchFilename}{File name of an irrigation schedule. This file will
#' only be used if triggering irrigation based on an input time series. When 
#' writing the file, the following information should be provided:
#' \describe{
#' \item{day}{}
#' \item{month}{}
#' \item{year}{}
#' \item{irrigationDepth}{mm}
#'}
#'}
#' \item{IrrMethod}{Method of irrigation, where:
#' \describe{
#' \item{0}{rainfed}
#' \item{1}{irrigation based on soil moisture status}
#' \item{2}{irrigation on a fixed interval}
#' \item{3}{pre-specified irrigation time-series}
#' \item{4}{net irrigation}
#'}
#'}
#' \item{IrrInterval}{Time interval between irrigation events (days), if 
#' triggering based on a fixed interval}
#' \item{SMT1}{Percentage of total available water at which irrigation is 
#' initiated in the first of the four main crop growth stages, if triggering 
#' based on soil moisture status.}
#' \item{SMT2}{Percentage of total available water at which irrigation is 
#' initiated in the second of the four main crop growth stages, if triggering 
#' based on soil moisture status.}
#' \item{SMT3}{Percentage of total available water at which irrigation is 
#' initiated in the third of the four main crop growth stages, if triggering 
#' based on soil moisture status.}
#' \item{SMT4}{Percentage of total available water at which irrigation is 
#' initiated in the fourth of the four main crop growth stages, if triggering 
#' based on soil moisture status.}
#' \item{MaxIrr}{Maximum irrigation depth (mm day-1).}
#' \item{AppEff}{Irrigation application efficiency (\%).}
#' \item{NetIrrSMT}{Percentage of total available water to maintain when
#'  in net irrigation mode.}
#' \item{WetSurf}{Soil surface area wetted by irrigation (\%).}
#'}
#'
#'When multiple crop types are considered in a single simulation, a unique 
#'irrigation management file can be created, and is assigned to each 
#'crop type in the crop rotation file.

IrrigationManagementParameters <- function() { 

}