# README #

This README provided the steps for the successful intallation and use of the 
AquaCropR package. The AquaCropR package is the R implementation of the AquaCrop crop 
    growth model developed by FAO (http://www.fao.org/aquacrop) to address food 
    security and assess the effect of the environment and management on crop 
    production. AquaCrop simulates the yield response of herbaceous crops to
    water and is particularly well suited to conditions in which water is a key 
    limiting factor in crop production. The AquaCrop model simulates final crop 
    yield in four steps which consist on the simulation of development of green 
    crop canopy cover, crop transpiration, above-ground biomass, and final crop 
    yield. Temperature and water stresses directly affect one or more of the 
    above processes. 
    Nutrient deficiencies and salinity effects are simulated indirectly by 
    moderating canopy cover development over the season, and by reducing crop
    transpiration and the normalized water productivity. The effect of CO2 
    concentration on biomass is simulated by altering the normalised water 
    productivity. 
    AquaCrop requires a relatively small number of explicit parameter values 
    such as weather and soil propertiers, and crop management practices. 
    Crop associated variables are normally known and are available for the 
    majority of crops

### What is this repository for? ###

* Quick summary
This repository holds all the scripts neccesary to install and run AquaCopR. It 
also contains one example which you can use to learn to use AquaCropR. 

* Version 
0.0.0.9000


### How do I get set up on WINDOWS? ###

To install AquaCropR on Windows, follow these steps:

* Clone the AquaCrop repository
(git clone https://bitbucket.org/anyelacamargo/aquacropr.git)
* Run R
*  Install the devtools package (if not available already): install.packages("devtools")
* Load the devtools package: library(devtools)
*  Set working directory to AquaCrop's location: setwd('your location/AquaCropR')
*  Set working directory a level above AquaCropR: setwd('..')
* Install AquaCropR: install('AquaCropR')
* Use the following command to test AquaCropR's installation: ?? AquaCropR
You should be able to see the package's help.

* Dependencies: XML, xml2, pracma, kulife, Rdpack, dplyr

### How do I get set up on LINUX? ###

NOTE: Linux is case sensitive

To install AquaCropR on Linux, follow these steps:

* Clone the AquaCrop repository
(git clone https://bitbucket.org/anyelacamargo/aquacropr.git)
* Run R
*  Install the devtools package (if not available already): install.packages("devtools")
* Load the devtools package: library(devtools)
*  Set working directory to AquaCrop's location: setwd('your location/aquacropr')
*  Set working directory a level above AquaCropR
setwd('..')??
* Install AquaCropR: install('aquacropr')
* Load AquaCropR: library(AquaCropR)
* Use the following command to test AquaCropR's installation: ?? AquaCropR

You should be able to see the package's help.

* Dependencies: XML, xml2, pracma, kulife, Rdpack, dplyr

### How to run tests ##

The following is the procedure to run a validation test set to simulate wheat 
yields. More details on the dataset are given in Section 4.

* Clone AquaExamples repo (https://github.com/anyelacamargo/AquaExamples.git)
* Load R
* setwd('path/AquaExamples) # This is the path to AquaExamples
* source('simulate_dummy_maize')


### Contribution guidelines ###

* Writing tests: Anyela
* Code review: Anyela
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin: Anyela Camargo, anyelavcamargo@gmail.com
* Other community or team contact