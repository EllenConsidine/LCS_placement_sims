# LCS_placement_sims
Comparing distributions of low-cost sensors in terms of accuracy and equity of real-time air quality information

## Obtaining Data



## Analysis

#### Functions used:
* AQI_equation.R -- used to calculate the EPA AQI classifications from the "observed" concentrations
* Calibrate_PA.R -- Preparing the PurpleAir (PA) data and comparing it with nearby reference measurements, to inform the simulation of LCS measurement error
* Sim_functions.R -- for each trial, samples locations to "deploy" LCS, simulates measurement error at those locations, assigns each grid point the PM2.5 information from the nearest monitor/sensor, and calculates metrics comparing the air quality that is reported vs experienced, overall and for marginalized subpopulations, weighted and unweighted by population density.

*There are several lines to change manually between the simulations (with different types and amounts of sensor measurement error (ME):*
1. In the __results__ function in the Sim_functions.R script, uncomment / comment out the line corresponding to the type of ME you're using
2. In the Run_sims.R script, change the variable "name" on line 6, to update all the file names to what you want for that type of ME
3. In the Merging_results.R script, change the variable "prefix" on line 20, to update all the file names to what you want for that type of ME

#### Scripts in order:
* Run_sims.R -- 
* Merging_results.R -- combines the results across the placement strategies and numbers of LCS "deployed", creating two results files weighted and unweighted by population density, respectively
