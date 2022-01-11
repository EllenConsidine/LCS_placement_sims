# LCS_placement_sims
Comparing distributions of low-cost sensors in terms of accuracy and equity of real-time air quality information

## Obtaining Data

*PurpleAir data (from 2020) obtained via the API on 1/11/22.*

#### Scripts used to process these data sets:
* Di et al. PM2.5 exposure estimates:
  * __QD_locations.R__ -- identifies which grid points in each file created by Di et al. are in California using a spatial overlay. 
  * __QD_get_CA.R__ -- cycles through the daily PM2.5 Di et al. files and extracts the measurements for grid points in California.
  * __Combining_Di_data.R__ -- combines the daily PM2.5 estimates for California into one file (to be read in all at once). 
* __Get_Nearest_PA_locations.R__ -- identifies PurpleAir sensors which are located within 50 meters of an AQS reference monitor in California.
* __PA_historical_data.ipynb__ -- uses a wrapper module for the PurpleAir API to download data from outdoor PurpleAir sensors in California.
* __School-Locs.R__ -- extracts the locations of public schools in California from a national shapefile.
* __Road_lengths.R__ -- calculates lengths of major roads/highways within 50, 100, 250, and 500 meters (circular buffers) of each grid point in California.
* __Download-Census-ACS-data.R__ -- downloads and combines sociodemographic variables at the block group and tract levels with a shapefile of all the block groups, which contains general information such as population density. The variables downloaded can be changed in __Census_variables.yml__ or __Census_variables_tracts.yml__
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
