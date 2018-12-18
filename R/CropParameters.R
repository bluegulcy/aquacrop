#' Crop Parameters to be provided in CropFile.
#' @export
#' @format A file in xml format should be provided with the following fields. 
#' Guidance on appropriate
#' parameter values for different crop types can be obtained from the FAO 
#' AquaCrop manual Steduto et sl (2009), Agronomy Journal, 2009, 101, 426-437.
#' \describe{
#' \item{CropType}{Determines the category of crop  Units: -  Default value: 
#' 1 = Leafy vegetable; 2 = Root/tuber; 3 = Fruit/grain}
#' \item{CalendarType}{Determines time units for crop development  Units: -  
#' Default value: 1 = Calendar days; 2 = GDD's}
#' \item{SwitchGDD}{Determines if inputs (when specified in calendar day mode) 
#' are converted to GDD's. Conversion is recommended to ensure accurate phenology. 
#'  Units: -  Default value: 0 = No 1 = Yes}
#' \item{PlantingDate}{Default planting date (may be overwritten)  Units: dd/mm 
#'  Default value: -}
#' \item{HarvestDate}{Default latest harvest date (may be overwritten)  Units: 
#' dd/mm  Default value: -}
#' \item{Emergence}{Time from sowing/transplanting to emergence/transplant 
#' recovery  Units: Days/GDD's  Default value: -}
#' \item{MaxRooting}{Time from sowing/transplanting to maximum root development  
#' Units: Days/GDD's  Default value: -}
#' \item{Senescence}{Time from sowing/transplanting to start of canopy senescence  
#' Units: Days/GDD's  Default value: -}
#' \item{Maturity}{Time from sowing/transplanting to physiological maturity  Units:
#'  Days/GDD's  Default value: -}
#' \item{HIstart}{Time from sowing/transplanting to start of yield formation  Units:
#'  Days/GDD's  Default value: -}
#' \item{Flowering}{Duration of flowering (only for fruit/grain crops)  Units:
#'  Days/GDD's  Default value: -}
#' \item{YldForm}{Duration of yield formation  Units: Days/GDD's  Default value: -}
#' \item{GDDmethod}{Method used to calculate GDD's  Units: -  Default value: -}
#' \item{Tbase}{Base temperature below which crop growth does not occur  Units: 
#' oC  Default value: -}
#' \item{Tupp}{Upper temperature above which crop growth does not occur  Units: 
#' oC  Default value: -}
#' \item{PolHeatStress}{Determines if pollination is affected by heat stress 
#'  Units: -  Default value: 0 = No; 1 = Yes}
#' \item{Tmax up}{Maximum temperature above which pollination begins to fail  
#' Units: oC  Default value: -}
#' \item{Tmaxlo}{Maximum temperature above which pollination fails completely 
#'  Units: oC  Default value: -}
#' \item{PolColdStress}{Determines if pollination is affected by cold stress  
#' Units: -  Default value: 0 = No; 1 = Yes}
#' \item{Tmin up}{Minimum temperature below which pollination begins to fail  
#' Units: oC  Default value: -}
#' \item{Tmin lo}{Minimum temperature below which pollination fails completely  
#' Units: oC  Default value: -}
#' \item{BioHeatStress}{Determines if biomass production is affected by 
#' temperature stress  Units: -  Default value: 0 = No; 1 = Yes}
#' \item{GDD up}{Minimum number of GDD's required for full biomass production  
#' Units: GDD's  Default value: -}
#' \item{GDD lo}{Minimum number of GDD's required for any biomass production 
#' to occur  Units: GDD's  Default value: -}
#' \item{fshape b}{Shape factor describing the reduction in biomass production 
#' due to insufficient GDD's  Units: GDD's  Default value: -}
#' \item{PctZmin}{Percentage of minimum effective rooting depth at 
#' sowing/transplanting  Units: \%  Default value: 70}
#' \item{Zmin}{Minimum effective rooting depth  Units: Metres  Default 
#' value: -}
#' \item{Zmax}{Maximum effective rooting depth  Units: Metres  Default value: -}
#' \item{fshape r}{Shape factor describing the decreasing speed of root 
#' expansion over time  Units: -  Default value: 1.5}
#' \item{fshape ex}{Shape factor describing the effects of water stress 
#' on root expansion  Units: -  Default value: -6}
#' \item{SxTopQ}{Maximum water extraction at the top of the root zone  
#' Units: m3 m-3 day-1  Default value: -}
#' \item{SxBotQ}{Maximum water extraction at the bottom of the root zone  
#' Units: m3 m-3 day-1  Default value: -}
#' \item{a Tr}{Exponent parameter describing the effect of canopy decline 
#' on transpiration/photosynthetic capacity  Units: -  Default value: 1}
#' \item{SeedSize}{Soil surface area covered by an individual seedling at 
#' 90\% emergence  Units: cm2  Default value: -}
#' \item{PlantPop}{Plant population  Units: plants ha-1  Default value: -}
#' \item{CCmin}{Minimum fractional canopy cover size below which yield
#'  formation does not occur  Units: -  Default value: -}
#' \item{CCx}{Maximum fractional canopy cover size  Units: -  Default value: -}
#' \item{CDC}{Canopy decline coefficient  Units: day-1/GDD-1  Default value: -}
#' \item{CGC}{Canopy growth coefficient  Units: day-1/GDD-1  Default value: -}
#' \item{Kcb}{Maximum crop coefficient when canopy is fully developed  Units: -  
#' Default value: -}
#' \item{fage}{Decline of crop coefficient due to ageing of the canopy  Units: 
#' \% day-1  Default value: -}
#' \item{WP}{Water productivity normalised for reference evapotranspiration and 
#' atmospheric carbon dioxide  Units: g m-2  Default value: -}
#' \item{WPy}{Adjustment of water productivity parameter in yield formation stage  
#' Units: \% of WP  Default value: -}
#' \item{fsink}{Crop sink strength coefficient  Units: -  Default value: -}
#' \item{bsted}{Water productivity adjustment parameter for CO2 effects given by 
#' (Steduto et al., 2007)  Units: -  Default value: 0.000138}
#' \item{bface}{Water productivity adjustment parameter for CO2 effects given by 
#' FACE experiments  Units: -  Default value: 0.001165}
#' \item{HI0}{Reference harvest index  Units: -  Default value: -}
#' \item{HIini}{Initial harvest index  Units: -  Default value: -}
#' \item{dHI pre}{Possible increase of harvest index due to pre-anthesis water 
#' stress  Units: \%  Default value: -}
#' \item{a HI}{Coefficient describing the positive impact on harvest index of 
#' restricted vegetative growth post-anthesis  Units: -  Default value: -}
#' \item{b HI}{Coefficient describing the negative impact on harvest index of 
#' stomatal closure post-anthesis  Units: -  Default value: -}
#' \item{dHI0}{Maximum possible increase in harvest index above reference value  
#' Units: \%  Default value: -}
#' \item{Determinant}{Crop determinacy, which affects period of potential vegetative 
#' growth  Units: -  Default value: 0 = Indeterminant}
#' \item{exc}{Excess of potential fruits that is produced by the crop  Units: \%  
#' Default value: -}
#' \item{MaxFlowPct}{Percentage of total flowering period at which peak flowering 
#' occurs  Units: \%  Default value: 33.33}
#' \item{p up1}{Upper soil water depletion threshold for water stress effects on 
#' canopy expansion  Units: -  Default value: -}
#' \item{p up2}{Upper soil water depletion threshold for water stress effects on 
#' stomatal control  Units: -  Default value: -}
#' \item{p up3}{Upper soil water depletion threshold for water stress effects on 
#' canopy senescence  Units: -  Default value: -}
#' \item{p up4}{Upper soil water depletion threshold for water stress effects on 
#' crop pollination  Units: -  Default value: -}
#' \item{p lo1}{Lower soil water depletion threshold for water stress effects on 
#' canopy expansion  Units: -  Default value: -}
#' \item{p lo2}{Lower soil water depletion threshold for water stress effects on 
#' stomatal control  Units: -  Default value: -}
#' \item{p lo3}{Lower soil water depletion threshold for water stress effects on 
#' canopy senescence  Units: -  Default value: -}
#' \item{p lo4}{Lower soil water depletion threshold for water stress effects on 
#' crop pollination  Units: -  Default value: -}
#' \item{fshape w1}{Shape factor describing water stress effects on canopy expansion  
#' Units: -  Default value: -}
#' \item{fshape w2}{Shape factor describing water stress effects on stomatal control  
#' Units: -  Default value: -}
#' \item{fshape w3}{Shape factor describing water stress effects on canopy senescence 
#'  Units: -  Default value: -}
#' \item{fshape w4}{Shape factor describing water stress effects on crop pollination  
#' Units: -  Default value: -}
#' \item{ETadj}{Determines if water stress thresholds are adjusted for variations in 
#' daily 
#' reference evapotranspiration  Units:   Default value: 0 = No 1 = Yes}
#' \item{Aer}{Water deficit below saturation at which aeration stress begins to occur  Units: \%  
#' Default value: 5}
#' \item{LagAer}{Lag before aeration stress affects crop growth  Units: days  Default value: 3}
#' \item{beta}{Reduction to p lo3 parameter when early canopy senescence is triggered due 
#' to water stress  Units: \%  Default value: 12}
#' \item{GermThr}{Proportion of total available water needed in the root zone for the crop 
#' to germinate  Units: -  Default value: 0.2}
#' }
#' 
#' 
#' 


CropParameters <- function() { 
  
}