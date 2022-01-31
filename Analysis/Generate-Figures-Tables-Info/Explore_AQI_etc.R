#### Used to generate SI Table A.2

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

## Setup 

CA_clean<- readRDS("CA_no_NAs_with_SDI2.rds")
DF<- CA_clean
n_obs<- dim(CA_clean)[1]
my_nas0<- readRDS("CA_NA_pos.rds")

days<- 1:366
# days<- c(1, 15, 32, 46, 61, 75, 92, 106,
#          122, 136, 153, 167, 183, 197, 214, 228,
#          245, 259, 275, 289, 306, 320, 336, 350) # 1st and 15th of each month
n_days<- length(days)

Real<- readRDS("Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]


NHNW<- rep((1-DF$nonHisp.white) > quantile(1-DF$nonHisp.white, 0.80), n_days) # top quintile
poverty<- rep(DF$poverty > quantile(DF$poverty, 0.80), n_days) # top quintile
# SD<- rep(DF$SDI > quantile(DF$SDI, 0.80), n_days) # top quintile

NHNW_pos<- which((1-DF$nonHisp.white) > quantile(1-DF$nonHisp.white, 0.80)) # top quintile
poverty_pos<- which(DF$poverty > quantile(DF$poverty, 0.80)) # top quintile

## Overall:
overall<- Real
NHNW.all<- Real[NHNW]
pov.all<- Real[poverty]

summary(overall)
summary(NHNW.all)
summary(pov.all)

## PM2.5 right above or below AQI cutoffs?

# Distribution around cutoffs:

overall.35<- Real[which(Real <= 35.5)]
NHNW.35<- Real[NHNW][which(Real[NHNW] <= 35.5)]
pov.35<- Real[poverty][which(Real[poverty] <= 35.5)]
# SD.35<- Real[SD][which(Real[SD] <= 35.5)]

# windows()
# par(mfrow=c(2,2))
# hist(overall.12)
# hist(NHNW.12)
# hist(pov.12)
# # hist(SD.12)
# dev.off()

summary(overall.35)
summary(NHNW.35) # seems to have higher overall levels, so gets pushed above/below more easily
summary(pov.35)
# summary(SD.12) 

# The upper limits are 6/5
above<- (Real > 12 & Real <= 12*(6/5))|(Real > 35.5 & Real <= 35.5*(6/5))|
                (Real > 55.5 & Real <= 55.5*(6/5))|(Real > 150.5 & Real <= 150.5*(6/5))

above_NHNW<- (Real[NHNW] > 12 & Real[NHNW] <= 12*(6/5))|(Real[NHNW] > 35.5 & Real[NHNW] <= 35.5*(6/5))|
                (Real[NHNW] > 55.5 & Real[NHNW] <= 55.5*(6/5))|(Real[NHNW] > 150.5 & Real[NHNW] <= 150.5*(6/5))

above_pov<- (Real[poverty] > 12 & Real[poverty] <= 12*(6/5))|(Real[poverty] > 35.5 & Real[poverty] <= 35.5*(6/5))|
                   (Real[poverty] > 55.5 & Real[poverty] <= 55.5*(6/5))|(Real[poverty] > 150.5 & Real[poverty] <= 150.5*(6/5))

# above_SD<- which((Real[SD] > 12 & Real[SD] <= 12*(6/5))|(Real[SD] > 35.5 & Real[SD] <= 35.5*(6/5))|
#                     (Real[SD] > 55.5 & Real[SD] <= 55.5*(6/5))|(Real[SD] > 150.5 & Real[SD] <= 150.5*(6/5)))

mean(above)
mean(above_NHNW)
mean(above_pov)
# length(above_SD)/length(Real[SD])

weighted.mean(above, rep(CA_clean$ppltn_d, n_days))
weighted.mean(above_NHNW, rep(CA_clean$ppltn_d[NHNW_pos], n_days))
weighted.mean(above_pov, rep(CA_clean$ppltn_d[poverty_pos], n_days))

# The lower limits are 4/5
below<- (Real < 12 & Real >= 12*(4/5))|(Real < 35.5 & Real >= 35.5*(4/5))|
                (Real < 55.5 & Real >= 55.5*(4/5))|(Real < 150.5 & Real >= 150.5*(4/5))

below_NHNW<- (Real[NHNW] < 12 & Real[NHNW] >= 12*(4/5))|(Real[NHNW] < 35.5 & Real[NHNW] >= 35.5*(4/5))|
                   (Real[NHNW] < 55.5 & Real[NHNW] >= 55.5*(4/5))|(Real[NHNW] < 150.5 & Real[NHNW] >= 150.5*(4/5))

below_pov<- (Real[poverty] < 12 & Real[poverty] >= 12*(4/5))|(Real[poverty] < 35.5 & Real[poverty] >= 35.5*(4/5))|
                    (Real[poverty] < 55.5 & Real[poverty] >= 55.5*(4/5))|(Real[poverty] < 150.5 & Real[poverty] >= 150.5*(4/5))

# below_SD<- which((Real[SD] < 12 & Real[SD] >= 12*(4/5))|(Real[SD] < 35.5 & Real[SD] >= 35.5*(4/5))|
#                    (Real[SD] < 55.5 & Real[SD] >= 55.5*(4/5))|(Real[SD] < 150.5 & Real[SD] >= 150.5*(4/5)))

mean(below)
mean(below_NHNW)
mean(below_pov)
# length(below_SD)/length(Real[SD])

weighted.mean(below, rep(CA_clean$ppltn_d, n_days))
weighted.mean(below_NHNW, rep(CA_clean$ppltn_d[NHNW_pos], n_days))
weighted.mean(below_pov, rep(CA_clean$ppltn_d[poverty_pos], n_days))


## AQI misclassifications from farther away?

library(FNN)
library(MetricsWeighted)

source("Analysis/AQI_equation.R") # includes Real_class
Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]

no_err_set<- which(DF$AQS_site == 1)
err_set<- which(DF$PA == 1)
num<- 1000
set.seed(7*303) # 304, 305
these<- sample(err_set, num, replace=FALSE)
pos<- unique(c(no_err_set,these))
error_pos<- these

dists<- get.knnx(DF[pos,c("Lon", "Lat")], DF[,c("Lon", "Lat")], k=1)$nn.dist
Dists<- rep(dists, 24)

## Now go use code from Sim_functions.R to get Deciles --> Shown_class
NN_pa<- DF[pos,"PA_site"][inds]
NN_PA<- rep(NN_PA, 24)

very_off<- which(abs(Real_class-Shown_class)>1)
very_off_NHNW<- which(abs(Real_class[NHNW]-Shown_class[NHNW])>1)
very_off_pov<- which(abs(Real_class[poverty]-Shown_class[poverty])>1)
# very_off_SD<- which(abs(Real_class[SD]-Shown_class[SD])>1)

summary(Dists)
summary(Dists[very_off])
summary(Dists[NHNW][very_off_NHNW])
summary(Dists[poverty][very_off_pov])
# summary(Dists[SD][very_off_SD])

# windows()
# par(mfrow=c(2,3))
# hist(Dists, breaks = seq(0,2,0.1))
# hist(Dists[very_off], breaks = seq(0,2,0.1))
# hist(Dists[NHNW][very_off_NHNW], breaks = seq(0,2,0.1))
# hist(Dists[poverty][very_off_pov], breaks = seq(0,2,0.1))
# # hist(Dists[SD][very_off_SD], breaks = seq(0,2,0.1))
# dev.off()

# weighted_quantile(eps[NHNW], rep(DF$ppltn_d[NHNW_pos], n_days), probs=0.95)
round(weighted_quantile(Dists, 
                        rep(CA_clean$ppltn_d, n_days),
                        probs=c(0,0.25,0.5,0.75,1)), 4)
round(weighted_quantile(Dists[very_off], 
                        rep(CA_clean$ppltn_d, n_days)[very_off],
                        probs=c(0,0.25,0.5,0.75,1)), 4)
round(weighted_quantile(Dists[NHNW][very_off_NHNW], 
                  rep(CA_clean$ppltn_d[NHNW_pos], n_days)[very_off_NHNW],
                  probs=c(0,0.25,0.5,0.75,1)), 4)
round(weighted_quantile(Dists[poverty][very_off_pov], 
                        rep(CA_clean$ppltn_d[poverty_pos], n_days)[very_off_pov],
                        probs=c(0,0.25,0.5,0.75,1)), 4)
# round(weighted_quantile(Dists[SD][very_off_SD], 
#                         rep(CA_clean$ppltn_d[SD_pos], n_days)[very_off_SD],
#                         probs=c(0,0.25,0.5,0.75,1)), 4)

# library(plotrix)
# 
# windows()
# par(mfrow=c(2,3))
# weighted.hist(Dists, w = rep(CA_clean$ppltn_d, n_days), breaks = seq(0,2,0.1))
# weighted.hist(Dists[very_off], w = rep(CA_clean$ppltn_d, n_days)[very_off], 
#               breaks = seq(0,2,0.1))
# weighted.hist(Dists[NHNW][very_off_NHNW], w = rep(CA_clean$ppltn_d, n_days)[very_off_NHNW], 
#               breaks = seq(0,2,0.1))
# weighted.hist(Dists[poverty][very_off_pov], w = rep(CA_clean$ppltn_d, n_days)[very_off_pov], 
#               breaks = seq(0,2,0.1))
# # weighted.hist(Dists[SD][very_off_SD], w = rep(CA_clean$ppltn_d, n_days)[very_off_SD], 
# #               breaks = seq(0,2,0.1))
# dev.off()


## How many of the misclassifications are due to LCS vs AQS?
sum(NN_PA)/length(NN_PA)
sum(NN_PA[very_off])/length(very_off)
sum(NN_PA[NHNW][very_off_NHNW])/length(very_off_NHNW)
sum(NN_PA[poverty][very_off_pov])/length(very_off_pov)
