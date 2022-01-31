#### Used to generate SI Table A.3

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

CA_clean<- readRDS("CA_clean_projected.rds")

## For reference: Sacramento County = 067, LA County = 037

sample_city<- function(err_set, num, road_weights = NULL, fips = "067"){
  city_N<- rep(0,50)
  for(i in 1:50){
    set.seed(7*303*i)
    if(is.null(road_weights)){
      these<- sample(err_set, num, replace=FALSE)
    }else{
      these<- sample(err_set, num, replace=FALSE, prob = road_weights)
    }
    city_N[i]<- sum(CA_clean[these, "COUNTYF"] == fips)
  }
  return(round(mean(city_N)))
}

## PA locations and schools:
sample_city(which(CA_clean$PA_site==1), 1000) # 26 out of 115 PA sensors
sample_city(which(CA_clean$School==1), 1000) # 41 out of 309 schools

## Roads:
# Add 0.1 to each location so it has a chance of getting picked
lengths<- CA_clean$Roads_500 + 0.1
rWeights<- lengths/sum(lengths)

sample_city(1:dim(CA_clean)[1], 1000, road_weights = rWeights) # 10

## CES Score:

ces<- readRDS("CA_with_CES_projected.rds")
CES<- ces[which(!is.na(ces$CES_score)),]
cWeights<- CES$CES_score/sum(CES$CES_score)

sample_city(1:dim(CES)[1], 1000, road_weights = cWeights) # 7 

## Pollution Score:

Poll<- ces[which(!is.na(ces$Pollution_score)),]
pWeights<- Poll$Pollution_score/sum(Poll$Pollution_score)

sample_city(1:dim(Poll)[1], 1000, road_weights = pWeights) # 7



##### Imperial County: 025

sample_city(which(CA_clean$PA_site==1), 1000, fips = "025") # 1 out of 3 PA sensors
sample_city(which(CA_clean$School==1), 1000, fips = "025") # 7 out of 53 schools
sample_city(1:dim(CA_clean)[1], 1000, road_weights = rWeights, fips = "025") # 27
sample_city(1:dim(CES)[1], 1000, road_weights = cWeights, fips = "025") # 47 
sample_city(1:dim(Poll)[1], 1000, road_weights = pWeights, fips = "025") # 34


##### LA County: 025

sample_city(which(CA_clean$PA_site==1), 1000, fips = "037") # 106 out of 469 PA sensors
sample_city(which(CA_clean$School==1), 1000, fips = "037") # 204 out of 1,530 schools
sample_city(1:dim(CA_clean)[1], 1000, road_weights = rWeights, fips = "037") # 29
sample_city(1:dim(CES)[1], 1000, road_weights = cWeights, fips = "037") # 25
sample_city(1:dim(Poll)[1], 1000, road_weights = pWeights, fips = "037") # 26


#### Calculate average CES scores for reference

aggregate(CES_score ~ COUNTYF, data = CES, mean)
aggregate(Pollution_score ~ COUNTYF, data = Poll, mean)

# ## Calculate total populations:
# library(dplyr)
# uniq_county<- distinct(CA_clean[,c("COUNTYF", "GEOID", "popultn")])
# 
# aggregate(popultn ~ COUNTYF, data = uniq_county, sum) # LA county seems too small by factor of 2 -- did a Google search to check

## External source: CSAC DataPile -- https://www.counties.org/data-and-research 
pops<- read.csv("Writing/CA_counties_populations.csv")
names(pops)<- c("County", "Year", "Population")

pops[which(pops$County == "Los Angeles"),]


