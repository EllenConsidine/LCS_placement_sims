
library(Hmisc)

setwd("/n/home13/econsidine")

CA_clean<- readRDS("LCS_data/CA_clean_projected.rds")
n_obs<- dim(CA_clean)[1]
my_nas0<- readRDS("LCS_data/CA_NA_pos.rds")

days<- 1:366 # all

n_days<- length(days)

Real<- readRDS("LCS_data/Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]

source("LCS_placement_sims/Analysis/Calibrate_PA.R") # includes Deciles for Real
Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]


ME_010<- list(c(), c(), c()) # one sublist for each group: overall, high % NW, high % poverty
ME_025<- list(c(), c(), c())
ME_epa<- list(c(), c(), c())
Pop_dens<- list(c(), c(), c())

DF<- CA_clean

pdw<- rep(DF$ppltn_d, n_days)

num<- sum(DF$PA_site == 1)

for(i in 1:50){ 
  set.seed(303*i)
  
  error_pos<- sample(which(DF$PA_site == 1), num, replace=FALSE)
  Error_pos<- as.vector(sapply(1:n_days, function(x) (x-1)*n_obs+(error_pos)))
  
  NHNW<- rep((1-DF$nonHisp.white) > quantile(1-DF$nonHisp.white, 0.80), n_days)[Error_pos]
  poverty<- rep(DF$poverty > quantile(DF$poverty, 0.80), n_days)[Error_pos]
  
  e.010<- sapply(Real[Error_pos], function(x) rnorm(1,mean=0,sd=0.1*x))
  e.025<- sapply(Real[Error_pos], function(x) rnorm(1,mean=0,sd=0.25*x))
  e.epa<- sapply(Deciles[Error_pos], function(q) sample(Q_resids[[q]], size=1))
  
  ME_010[[1]]<- append(ME_010[[1]], e.010)
  ME_025[[1]]<- append(ME_025[[1]], e.025)
  ME_epa[[1]]<- append(ME_epa[[1]], e.epa)
  
  ME_010[[2]]<- append(ME_010[[2]], e.010[NHNW])
  ME_025[[2]]<- append(ME_025[[2]], e.025[NHNW])
  ME_epa[[2]]<- append(ME_epa[[2]], e.epa[NHNW])
  
  ME_010[[3]]<- append(ME_010[[3]], e.010[poverty])
  ME_025[[3]]<- append(ME_025[[3]], e.025[poverty])
  ME_epa[[3]]<- append(ME_epa[[3]], e.epa[poverty])
  
  PDW<- pdw[Error_pos]
  
  Pop_dens[[1]]<- append(Pop_dens[[1]], PDW)
  Pop_dens[[2]]<- append(Pop_dens[[2]], PDW[NHNW])
  Pop_dens[[3]]<- append(Pop_dens[[3]], PDW[poverty])
  
}


print("ME 10% differential:")
print("Unweighted:")
sd(ME_010[[1]])
sd(ME_010[[2]])
sd(ME_010[[3]])
print("Weighted:")
sqrt(wtd.var(ME_010[[1]], Pop_dens[[1]]))
sqrt(wtd.var(ME_010[[2]], Pop_dens[[2]]))
sqrt(wtd.var(ME_010[[3]], Pop_dens[[3]]))

print("ME 25% differential:")
print("Unweighted:")
sd(ME_025[[1]])
sd(ME_025[[2]])
sd(ME_025[[3]])
print("Weighted:")
sqrt(wtd.var(ME_025[[1]], Pop_dens[[1]]))
sqrt(wtd.var(ME_025[[2]], Pop_dens[[2]]))
sqrt(wtd.var(ME_025[[3]], Pop_dens[[3]]))

print("EPA residuals:")
print("Unweighted:")
sd(ME_epa[[1]])
sd(ME_epa[[2]])
sd(ME_epa[[3]])
print("Weighted:")
sqrt(wtd.var(ME_epa[[1]], Pop_dens[[1]]))
sqrt(wtd.var(ME_epa[[2]], Pop_dens[[2]]))
sqrt(wtd.var(ME_epa[[3]], Pop_dens[[3]]))
