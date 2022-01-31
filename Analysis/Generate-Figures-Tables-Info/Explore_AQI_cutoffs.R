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

summary(overall.35)
summary(NHNW.35) # seems to have higher overall levels, so gets pushed above/below more easily
summary(pov.35)

# The upper limits are 6/5
above<- (Real > 12 & Real <= 12*(6/5))|(Real > 35.5 & Real <= 35.5*(6/5))|
                (Real > 55.5 & Real <= 55.5*(6/5))|(Real > 150.5 & Real <= 150.5*(6/5))

above_NHNW<- (Real[NHNW] > 12 & Real[NHNW] <= 12*(6/5))|(Real[NHNW] > 35.5 & Real[NHNW] <= 35.5*(6/5))|
                (Real[NHNW] > 55.5 & Real[NHNW] <= 55.5*(6/5))|(Real[NHNW] > 150.5 & Real[NHNW] <= 150.5*(6/5))

above_pov<- (Real[poverty] > 12 & Real[poverty] <= 12*(6/5))|(Real[poverty] > 35.5 & Real[poverty] <= 35.5*(6/5))|
                   (Real[poverty] > 55.5 & Real[poverty] <= 55.5*(6/5))|(Real[poverty] > 150.5 & Real[poverty] <= 150.5*(6/5))


mean(above)
mean(above_NHNW)
mean(above_pov)

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


mean(below)
mean(below_NHNW)
mean(below_pov)

weighted.mean(below, rep(CA_clean$ppltn_d, n_days))
weighted.mean(below_NHNW, rep(CA_clean$ppltn_d[NHNW_pos], n_days))
weighted.mean(below_pov, rep(CA_clean$ppltn_d[poverty_pos], n_days))
