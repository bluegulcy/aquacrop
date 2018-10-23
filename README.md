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


### How do I get set up? ###

* Summary of set up
  Run R: install.packages("devtools")
  Install the devtools package: install.packages("devtools")
  Load the devtools package: library(devtools)
  Install the AquaCrop package: install_github("anyelacamargo/aquacropr")
  Load the libary: library(AquaCropR)


* Dependencies: XML, xml2, pracma, kulife, Rdpack, dplyr

### How to run tests ##

Run the file 'simulate_dummy.R'. The scripts points to the file 
'FileSetup_babycorn.xml' which has the name of the setup files describing your 
simulation. These setup files are located in the folder 'input_babycorn folder'.
Once the simulation is finished, a data.frame will be returned which will be
used to create six plots which will be located in the 'output_babycorn' folder.


### Contribution guidelines ###

* Writing tests: Anyela
* Code review: Anyela
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin: Anyela Camargo, anyelavcamargo@gmail.com
* Other community or team contact