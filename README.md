# LCS_placement_sims
Investigating factors affecting accuracy and equity of real-time air quality information, resulting from deployment of low-cost sensors. 

## Obtaining Data

#### Scripts used to download and process the data sets upon which the simulations are based:
* __EPA_AQS.R__ -- processes AQS monitoring data from California, setting a few negative values to zero and only keeping daily averages from days with 18 or more hours observed. The AQS summary files can be found on [the EPA website](https://aqs.epa.gov/aqsweb/airdata/download_files.html). We used the PM2.5 88101 and 88502 summary files from 2020. 
* Di et al. PM2.5 exposure estimates: _these data can now be accessed [here](https://sedac.ciesin.columbia.edu/data/set/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016), however, they are in a slightly different format than what we originally received from the authors and used in our code._
  * __QD_locations.R__ -- identifies which grid points in each file created by Di et al. are in California using a spatial overlay. 
  * __QD_get_CA.R__ -- cycles through the daily PM2.5 Di et al. files and extracts the measurements for grid points in California.
  * __Combining_Di_data.R__ -- combines the daily PM2.5 estimates for California into one file (to be read in all at once). 
* __Get_Nearest_PA_locations.R__ -- identifies PurpleAir sensors which are located within 50 meters of an AQS reference monitor in California. Uses a list of PurpleAir located outdoors, which is obtained in PA_historical_data.ipynb (below). 
* __PA_historical_data.ipynb__ -- uses [a wrapper module for the PurpleAir API](https://github.com/ReagentX/purple_air_api/) to download data from outdoor PurpleAir sensors in California. As currently written, the user must specify "parent" or "child" (in the places indicated in the script) to obtain data from PurpleAir channels A or B, respectively. *PurpleAir data (from 2020) was obtained via the API on 1/11/22 (channel A) and 1/23/22 (channel B).*
* __School-Locs.R__ -- extracts the locations of public schools in California from a national shapefile, accessible [here](https://nces.ed.gov/programs/edge/geographic/schoollocations).
* __Road_lengths.R__ -- calculates lengths of major roads/highways within 50, 100, 250, and 500 meters (circular buffers) of each grid point in California, using the National Highway Planning Network shapefile, which can be accessed [here](https://data-usdot.opendata.arcgis.com/datasets/national-highway-planning-network/explore?location=45.117500%2C63.327200%2C3.46).
* __Download-Census-ACS-data.R__ -- downloads and combines sociodemographic variables at the Census block group and Census tract levels with a shapefile of all the block groups, which contains general information such as population density. The script uses the package __tidycensus__. The variables downloaded can be changed in __Census_variables.yml__ or __Census_variables_tracts.yml__
* 


## Analysis

#### Functions used:
* __AQI_equation.R__ -- used to calculate the EPA AQI classifications from the "observed" concentrations.
* __Calibrate_PA.R__ -- Preparing the PurpleAir (PA) data and comparing it with nearby reference measurements, to inform the simulation of LCS measurement error.
* __Sim_functions.R__ -- for each trial, samples locations to "deploy" LCS, simulates measurement error at those locations, assigns each grid point the PM2.5 information from the nearest monitor/sensor, and calculates metrics comparing the air quality that is reported vs experienced, overall and for marginalized subpopulations, weighted and unweighted by population density.

*There are several lines to change manually between the simulations, with different types and amounts of sensor measurement error (ME):*
* In the __results__ function in the __Sim_functions.R__ script, uncomment / comment out the line corresponding to the type of ME you're using.
* In the __Run_sims.R__ script, change the variable "name" on line 6, to update all the file names to what you want for that type of ME.
* In the __Merging_results.R__ script, change the variable "prefix" on line 20, to update all the file names to what you want for that type of ME. Might also want to change the date in the final file names, lines 135-145. 

#### Scripts in order:
1. __Run_sims.R__ -- runs 50 trials per placement strategy and number of LCS "deployed", saving both the results from each trial and the average metrics across the 50 trials. *Note: I ended up running these simulations sequentially due to the queuing structure of our university's cluster, however, they are set up to be easy to run in parallel should that be easier on a different platform.*
2. __Merging_results.R__ -- combines the results across the placement strategies and numbers of LCS "deployed", creating two results files weighted and unweighted by population density, respectively.
3. __Plot_multiple_MEs.R__ -- generates plots for manuscript (can easily run locally after transferring results files from cluster).

Additionally, the __Summarize_MEs.R__ script can be used to calculate the standard deviation (weighted and unweighted by population density) of sensor measurement error when simulating differentially (either from a Normal distribution with mean zero and a standard deviation of 10% of 25% of "true" PM2.5, or drawing from EPA calibration residuals associated with the same decile of "true" PM2.5) at all the locations of PurpleAir sensors.
