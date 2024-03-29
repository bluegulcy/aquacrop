% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadFileLocations.R
\name{ReadFileLocations}
\alias{ReadFileLocations}
\title{Read files input and output file locations}
\format{An xml file should be provided with the following fields 
\describe{
\item{Input}{Files location}
\item{ClockFilename}{Name of clock file (xml format) which is 
used to set the duration of 
the simulation. When writing the file, the following information 
should be provided:}
\describe{
\item{SimulationStartTime}{Time when the simulation starts 
(yyyy-mm-dd)}
\item{SimulationEndTime}{Time when the simulation ends 
(yyyy-mm-dd)} 
\item{OffSeason}{Specifies whether (as 'Y' or 'N')  Soil water 
balance is 
simulated outside the growing season}
}

\item{WeatherFilename}{Name of weather input file (csv format) 
which defines time-series of 
daily weather inputs. When writing the file, the following 
information should 
be provided in comma separated format:}
\describe{
\item{day}{day}
\item{month}{month}
\item{year}{year}
\item{mintp}{Day Minimum  Temperature (oC)}
\item{mxtp}{Day Maximum Temperature (oC)}
\item{p}{Daily precipitation (mm)}
\item{evp}{Daily reference evapotranspiration (mm)}
}

\item{CO2Filename}{Name of Annual CO2 file (csv format) which 
contains time-series of
 atmospheric carbon dioxide (CO2) concentrations. When writing 
 the file, 
 the following information should be provided in comma 
 separated format:}
\describe{
\item{year}{year}
\item{co2}{CO2 concentation (ppm)}
}

\item{SoilFilename}{Name of Soil file (xml format) which defines 
the input variables needed 
to parameterise the soil components in the model. When writing the
 file, the following file's names, as well as the variables 
 listed in the \link{SoilParameters} section, should be provided.}
\describe{
\item{SoilProfileFilename}{Name of Soil profile name (xml format) 
which defines the 
discretisation of the soil profile in to compartments and layers.
When writing the file, the following information should be provided.
 \describe{
\item{CompartmentNo}{Soil compartment number}
\item{Thickness}{Compartment thickness (m)}
\item{LayerNo}{Associated soil layer number}
}}
\item{SoilTextureFilename}{Name of Soil Texture file (xml format) 
which calculates soil 
hydraulic properties from textural properties. If the user specifies
 in the soil input file 
to calculate soil hydraulic properties from textural properties, the 
soil texture input file 
must be provided. The soil texture input file defines the textural 
properties of each 
soil layer, which are then assigned to individual soil compartments 
according to the 
discretisation of the soil profile. When writing the file, the 
following information should be provided:
\describe{
\item{LayerNo}{Soil layer number}
\item{Thickness}{ Thickness of the layer (m)}
\item{Sand}{Sand content (\%)}
\item{Clay}{Clay content (\%)}
\item{OrgMat}{t (\% by weight)}
\item{DensityFactor}{default value of 1}
}}
Note. The number of data rows should match the number of soil layers 
specified in the soil 
input file. Using these input values, AquaCrop calculates the hydraulic
 properties of each soil 
layer (water contents at saturation, field capacity, and permanent 
wilting, along with the 
saturated hydraulic conductivity) based on the pedotransfer function 
model https://en.wikipedia.org/wiki/Pedotransfer_function.

\item{SoilHydrologyFilename}{Name of FieldManagement file (xml format). 
If the user specifies 
in the soil input file that soil hydraulic properties are pre-defined,
 a soil hydrology
input file  must be provided. When writing the file, the following 
information 
should be provided:
 \describe{
\item{LayerNo}{Soil layer number}
\item{LayerThickness}{Thickness of the layer (m)}
\item{thS}{Water content at saturation (m3 m-3)}
\item{thF}{Water content at field capacity (m3 m-3)}
\item{thWP}{Water content at permanent wilting point (m3 m-3)}
\item{Ksat}{Saturated hydraulic conductivity (mm day-1)}
}}
Note: The number of data rows should equal exactly the number of 
soil layers 
specified in thesoil input file.
}
\item{CropRotationFilename}{Name of Crop Rotation file (xml format) 
which defines the 
crop types and any specified rotation to be simulated. When writing 
the file, the 
following information 
should be provided:}
\describe{
\item{NumberofCropOptions}{Number of crop types modelled}
\item{SpecifiedPlantingCalendar}{If a crop rotation calendar is 
specified, 
denoted by a 'Y' or 'N' character}
\item{CropInfo}{Information about each crop should be provided through 
these variables:
\describe{
\item{croptype}{Crop name}
\item{CropFilename}{Crop input filename in xml format. The crop file 
defines the input variables needed to parameterise the crop component 
of AquaCrop.
A unique version of the crop input file should be created for each 
individual 
crop type modelled during the simulation period, as defined 
by the variables \strong{NumberofCropOptions} and 
\strong{CropRotationCalendarFilename}. 
When writing the file, the variables listed in \link{CropParameters} 
should be provided}
\item{IrrigationFilename}{Irrigation management input filename in 
xml format. When writing the file, the variables listed in the 
\link{IrrigationManagementParameters} section should be provided.}
}
}
}
A rotation calendar must be specified if more than one crop type is 
considered. 
When a rotation calendar is specified,
the planting and latest harvest dates specified in each crop input file 
will be overwritten by the values given in the rotation calendar
\item{FieldManagementFilename}{Name of FieldManagement file (xml format) 
which defines the 
input variables controlling field management practices in the model. 
When writing the file, 
the following information should be provided
\describe{
\item{Mulches}{If the soil surface is covered by mulches, where '0' 
is 'No' and '1' is 'Yes'.}
\item{MulchPctGS}{Soil surface area (\%) covered by any mulches 
during the growing season.}
\item{MulchPctOS}{Soil surface area (\%) covered by any mulches 
during the off season.}
\item{fMulch}{Factor defining the proportional reduction of soil 
evaporation due
 to presence of mulches.}
\item{Bunds}{If soil bunds are present on the field, where '0' is 
'No' and '1' is 'Yes'.}
\item{zBund}{Height of any soil bunds (m).}
\item{BundWater}{Initial depth of water between any soil bunds (mm).}
}
}
\item{GroundwaterFilename}{Name of Groundwater file (xml format) 
which defines any shallow 
water table conditions that may influence soil moisture levels
\describe{
\item{Watertable_present}{If water table is present, denoted by a 
'Y' or 'N' character}
\item{Watertable_method}{If a water table is present, whether it 
is 'Constant' or 'Variable'.}
\item{observations}{Observations of groundwater, where each 
observation contains two variables:
\describe{
\item{date}{(dd/mm/yyyy)}
\item{depth}{Water table depth (m)}
}
}
}
}
\item{InitialWCFilename}{Name of Initial Water Content file 
(xml format) which defines 
the initial moisture conditions throughout the soil profile 
at the start of the 
simulation, and also at the beginning of each growing season 
if the model does not 
simulate the soil water balance in the off-season. When writing 
the file, 
the following information should be provided:
\describe{
\item{type_of_value}{Format in which soil moisture input data is
 provided. Options 
available are to specify values based on:
\describe{
\item{Prop}{soil hydraulic properties}
\item{Num}{numerical values}
\item{Pct}{percentages of total available water}
}
}
\item{method}{Method used to calculate compartment water contents. 
If method is 
depth-based ('Depth'), observations will be linearly interpolated to
 the centre of each 
compartment. Alternatively, a layer-based method ('Layer') will apply 
uniform 
values to all compartments within a soil layer}
\item{number_of_input_points}{Number of input soil moisture data points}
\item{input_data_points}{Soil moisture data observations, where each row 
contains two variables:
\describe{
\item{depth_layer}{Point depth (m) or layer number}
\item{value}{Soil moisture value (defined format). If the format of soil 
moisture values is:
\describe{
\item{Prop}{values must be specified as either 'SAT' (Saturation), 'FC' 
(Field capacity), or 'WP' 
(Wilting point)}
\item{Num}{values must have units of m3 m-3}
\item{Pct}{values must have units of \%}
}
}
}
}
}
}
\item{CropRotationCalendarFilename}{Name of Crop Rotation file 
(xml format). If 
the user specifies multiple crop types in the CropRotationFile or 
wishes to 
consider variable planting dates over a multi-season simulation,
 a crop 
rotation input file  must be provided that defines the time series of 
growing seasons 
that will be simulated by AquaCrop. When writing the file, the 
following information 
should be provided:
\describe{
\item{PlantDate}{Planting date (dd/mm/yyyy)}
\item{HarvestDate}{Latest possible harvest date (dd/mm/yyyy)}
\item{Crop}{ Crop type}
}
Data only need to be specified for days when irrigation occurs. 
AquaCrop will automatically apply zero irrigation values to all other
simulation days.
}
}}
\usage{
ReadFileLocations(filename)
}
\arguments{
\item{filename}{An xml file with locations}
}
\value{
list with \code{FileLocation}
}
\description{
Read files input and output file locations
}
\examples{
ReadFileLocations('dummy.xml')
}
