# LCS_placement_sims
Investigating factors affecting accuracy and equity of real-time air quality information, resulting from deployment of low-cost sensors. Associated publication: [Investigating Use of Low-Cost Sensors to Increase Accuracy and Equity of Real-Time Air Quality Information](https://pubs.acs.org/doi/full/10.1021/acs.est.2c06626).

**Publication abstract:** U.S. Environmental Protection Agency (EPA) air quality (AQ) monitors, the “gold standard” for measuring air pollutants, are sparsely positioned across the U.S. Low-cost sensors (LCS) are increasingly being used by the public to fill in the gaps in AQ monitoring; however, LCS are not as accurate as EPA monitors. In this work, we investigate factors impacting the differences between an individual’s true (unobserved) exposure to air pollution and the exposure reported by their nearest AQ instrument (which could be either an LCS or an EPA monitor). We use simulations based on California data to explore different combinations of hypothetical LCS placement strategies (e.g., at schools or near major roads), for different numbers of LCS, with varying plausible amounts of LCS device measurement errors. We illustrate how real-time AQ reporting could be improved (or, in some cases, worsened) by using LCS, both for the population overall and for marginalized communities specifically. This work has implications for the integration of LCS into real-time AQ reporting platforms.

## Analysis (chronologically, after Obtaining Data)

#### Functions used:
* __AQI_equation.R__ -- used to calculate the EPA AQI classifications from the "observed" concentrations.
* __Calibrate_PA.R__ -- Preparing the PurpleAir (PA) data and comparing it with nearby reference measurements, to inform the simulation of LCS measurement error.
* __Sim_functions.R__ -- for each trial, samples locations to "deploy" LCS, simulates measurement error at those locations, assigns each grid point the PM2.5 information from the nearest monitor/sensor, and calculates metrics comparing the air quality that is reported vs experienced, overall and for marginalized subpopulations, weighted and unweighted by population density.

#### Scripts in order:
1. __Run_sims_split-up.R__ -- runs 100 trials per placement strategy and number of LCS "deployed", saving both the results from each trial and the average metrics across the 100 trials. *Note: scripts to run these in parallel are in the On_cluster folder.*
2. __Merging_results.R__ -- combines the results across the placement strategies and numbers of LCS "deployed", creating two results files weighted and unweighted by population density, respectively.
3. __Make_table1.R__ -- generates tables summarizing the effects of different types and amounts of sensor measurement error when LCS are placed (a) at all PurpleAir locations and (b) at all schools in California.
4. __Plot_multiple_MEs.R__ -- generates plots for manuscript (can easily run locally after transferring results files from cluster).

Additionally, the __Summarize_MEs.R__ script can be used to calculate the standard deviation (weighted and unweighted by population density) of sensor measurement error when simulating differentially (either from a Normal distribution with mean zero and a standard deviation of 10% of 25% of "true" PM2.5, or drawing from EPA calibration residuals associated with the same decile of "true" PM2.5) at all the locations of PurpleAir sensors.

#### All scripts used to generate figures and tables are in the subfolder Generate-Figures-Tables-Info.



## Obtaining Data (chronologically, prior to Analysis)

#### Scripts used to download and process the data sets upon which the simulations are based:
* __EPA_AQS.R__ -- processes AQS monitoring data from California, setting a few negative values to zero and only keeping daily averages from days with 18 or more hours observed. The AQS summary files can be found on [the EPA website](https://aqs.epa.gov/aqsweb/airdata/download_files.html). We used the PM2.5 88101 and 88502 summary files from 2020. 
* Di et al. PM2.5 exposure estimates: _these data can now be accessed [here](https://sedac.ciesin.columbia.edu/data/set/aqdh-pm2-5-concentrations-contiguous-us-1-km-2000-2016), however, they are in a slightly different format than what we originally received from the authors and used in our code._
  * __QD_locations.R__ -- identifies which grid points in each file created by Di et al. are in California using a spatial overlay. 
  * __QD_get_CA.R__ -- cycles through the daily PM2.5 Di et al. files and extracts the measurements for grid points in California.
  * __Combining_Di_data.R__ -- combines the daily PM2.5 estimates for California into one file (to be read in all at once). 
* __Get_Nearest_PA_locations.R__ -- identifies PurpleAir sensors which are located within 50 meters of an AQS reference monitor in California. Uses a list of PurpleAir located outdoors, which is obtained in PA_historical_data.ipynb (below). *All PurpleAir locations (indoor + outdoor) were obtained from their website on 4/16/21.*
* __PA_historical_data.ipynb__ -- uses [a wrapper module for the PurpleAir API](https://github.com/ReagentX/purple_air_api/) to download data from outdoor PurpleAir sensors in California. As currently written, the user must specify "parent" or "child" (in the places indicated in the script) to obtain data from PurpleAir channels A or B, respectively. *PurpleAir data (from 2020) were obtained via the API on 1/11/22 (channel A) and 1/23/22 (channel B).*
* __School-Locs.R__ -- extracts the locations of public schools in California from a national shapefile, accessible [here](https://nces.ed.gov/programs/edge/geographic/schoollocations).
* __Road_lengths.R__ -- calculates lengths of major roads/highways within 50, 100, 250, and 500 meters (circular buffers) of each grid point in California, using the National Highway Planning Network shapefile, which can be accessed [here](https://data-usdot.opendata.arcgis.com/datasets/national-highway-planning-network/explore?location=45.117500%2C63.327200%2C3.46).
* __Download-Census-ACS-data.R__ -- downloads and combines sociodemographic variables at the Census block group and Census tract levels with a shapefile of all the block groups, which contains general information such as population density. The script uses the package __tidycensus__. The variables downloaded can be changed in __Census_variables.yml__ or __Census_variables_tracts.yml__
* __Merge_CA.R__ -- combines static information (locations of monitors and sensors, sociodemographic info, etc.) to use in the simulations. The CalEnviroScreen (CES) data can be downloaded [here](https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30). We used CES 3.0 in this analysis; now, CES 4.0 is available.


## Questions? 
#### Contact Ellen Considine, ellen_considine@g.harvard.edu

