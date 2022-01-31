#### Generates SI Table B.1 (and SI Table C.1), which is then used to generate Table 1 with Normalize_table_1.R
  #### Also generates SI Figure A.1 and calculates the correlation between the Pollution Score and Di et al. annual averages of PM2.5

library(ggplot2)
library(gridExtra)
library(MetricsWeighted)
library(Hmisc)

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

## Read in data:
avg_PM<- readRDS("CA_2016_averages.rds")
CA_locations<- readRDS("Getting data/CA_locations.rds")
Avg_PM<- data.frame(avg_PM, Lon = CA_locations$Lon, Lat = CA_locations$Lat)
# ggplot(Avg_PM, aes(x = Lon, y = Lat, colour = avg_PM)) +
  # geom_point(cex=0.5)

na_pos<- readRDS("CA_NA_pos.rds")
avg_PM<- avg_PM[!na_pos]
CA_clean<-readRDS("CA_with_CES_projected.rds")

AQS<- which(CA_clean$AQS_site == 1)
PA<- which(CA_clean$PA_site == 1)
Schools<- which(CA_clean$School == 1)
Roads<- CA_clean$Roads_500
CES<- CA_clean$CES_score
Pollution<- CA_clean$Pollution_score

## Exploring relationships between a few variables:
plot(Roads, avg_PM)
plot(Roads, Pollution)
plot(avg_PM, Pollution)

poll_pos<- which(!is.na(Pollution))
cor(avg_PM[poll_pos], Pollution[poll_pos])

## Functions for descriptive stats table (Table 1) in the paper:

ds<- function(vec, w = NULL){
  
  if(!is.null(w)){
    m<- weighted.mean(vec, w, na.rm = TRUE)
    s<- sqrt(wtd.var(vec, w, na.rm = TRUE))
  }else{
    m<- mean(vec, na.rm = TRUE)
    s<- sd(vec, na.rm=TRUE)
  }
  print(paste0(round(m,2), " (", round(s,2), ")"))
}

NHNW_pos<- which((1-CA_clean$nonHisp.white) > quantile(1-CA_clean$nonHisp.white, 0.80))
poverty_pos<- which(CA_clean$poverty > quantile(CA_clean$poverty, 0.80))
CES_na<- which(is.na(CA_clean$CES_score))
Poll_na<- which(is.na(CA_clean$Pollution_score))

calcs<- function(vrbl, na_pos=NULL){
  ## Uncomment to obtain the stats weighted by population density (the table in the main paper):
  # ds(vrbl, CA_clean$ppltn_d)
  # ds(vrbl[NHNW_pos], CA_clean$ppltn_d[NHNW_pos])
  # ds(vrbl[poverty_pos], CA_clean$ppltn_d[poverty_pos])
  # ds(vrbl[which(CA_clean$AQS_site==1)], CA_clean$ppltn_d[which(CA_clean$AQS_site==1)])
  # ds(vrbl[which(CA_clean$PA_site==1)], CA_clean$ppltn_d[which(CA_clean$PA_site==1)])
  # ds(vrbl[which(CA_clean$School==1)], CA_clean$ppltn_d[which(CA_clean$School==1)])
  # ds(vrbl, Roads)
  # ds(vrbl[which(CA_clean$ppltn_d > 500)], Roads[which(CA_clean$ppltn_d > 500)])
  # ds(vrbl[-Poll_na], Pollution[-Poll_na])
  # ds(vrbl[-Poll_na][which(CA_clean$ppltn_d[-Poll_na] > 500)], 
  #    Pollution[-Poll_na][which(CA_clean$ppltn_d[-Poll_na] > 500)])
  # ds(vrbl[-CES_na], CES[-CES_na])
  # ds(vrbl[-CES_na][which(CA_clean$ppltn_d[-CES_na] > 500)], 
  #    CES[-CES_na][which(CA_clean$ppltn_d[-CES_na] > 500)])
  
  ## Stats unweighted by population density (the table in the SI):
  ds(vrbl)
  ds(vrbl[NHNW_pos])
  ds(vrbl[poverty_pos])
  ds(vrbl[which(CA_clean$AQS_site==1)])
  ds(vrbl[which(CA_clean$PA_site==1)])
  ds(vrbl[which(CA_clean$School==1)])
  ds(vrbl, Roads)
  ds(vrbl[-Poll_na], Pollution[-Poll_na])
  ds(vrbl[-CES_na], CES[-CES_na])
}

calcs(avg_PM)
calcs(CA_clean$poverty)
calcs(CA_clean$CES_score)
calcs(1-CA_clean$nonHisp.white)

## Want population density for all of these subsets, unweighted:
vrbl<- CA_clean$ppltn_d
ds(vrbl)
ds(vrbl[NHNW_pos])
ds(vrbl[poverty_pos])
ds(vrbl[which(CA_clean$AQS_site==1)])
ds(vrbl[which(CA_clean$PA_site==1)])
ds(vrbl[which(CA_clean$School==1)])
ds(vrbl, Roads)
ds(vrbl[which(CA_clean$ppltn_d > 500)], Roads[which(CA_clean$ppltn_d > 500)])
ds(vrbl[-Poll_na], Pollution[-Poll_na])
ds(vrbl[-Poll_na][which(CA_clean$ppltn_d[-Poll_na] > 500)], 
   Pollution[-Poll_na][which(CA_clean$ppltn_d[-Poll_na] > 500)])
ds(vrbl[-CES_na], CES[-CES_na])
ds(vrbl[-CES_na][which(CA_clean$ppltn_d[-CES_na] > 500)], 
   CES[-CES_na][which(CA_clean$ppltn_d[-CES_na] > 500)])


## Maps of marginalized group rates in SI Appendix A:
CA_clean$Non.nonHisp.white<- 1 - CA_clean$nonHisp.white
CA_clean$Poverty<- CA_clean$poverty

m1<- ggplot(CA_clean, aes(x = Lon, y = Lat, colour = Non.nonHisp.white)) +
  geom_point(cex=0.5) + ggtitle("Marginalized by Race/Ethnicity")
m2<- ggplot(CA_clean, aes(x = Lon, y = Lat, colour = Poverty)) +
  geom_point(cex=0.5) + ggtitle("Marginalized by Income")

png("Final Plots/Marginalization_indices.png", width = 795, height = 400)

grid.arrange(m1, m2, nrow = 1)

dev.off()
