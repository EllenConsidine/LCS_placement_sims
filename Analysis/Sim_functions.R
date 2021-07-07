## Packages:
library(FNN)
library(MetricsWeighted)
# library(ggplot2)

## Reading in data:

setwd("/n/home13/econsidine")

CA_clean<- readRDS("LCS_data/CA_no_NAs_with_SDI2.rds")
n_obs<- dim(CA_clean)[1]
my_nas0<- readRDS("LCS_data/CA_NA_pos.rds")

# ann_avg<- readRDS("CA_2016_averages.rds")
# ann_avg<- ann_avg[!my_nas0]

days<- 1:366 # all

# days<- c(15, 46, 75, 106,
#          136, 167, 197, 228,
#          259, 289, 320, 350) # 15th of each month

# days<- c(1, 15, 32, 46, 61, 75, 92, 106,
#          122, 136, 153, 167, 183, 197, 214, 228,
#          245, 259, 275, 289, 306, 320, 336, 350) # 1st and 15th of each month

# days<- seq(2,366,by=2) # every other day
n_days<- length(days)

Real<- readRDS("LCS_data/Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]

# load("Analysis/Simulate_PA_ME.RData")
source("LCS_placement_sims/Analysis/Calibrate_PA.R") # includes Deciles for Real
Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
source("LCS_placement_sims/Analysis/AQI_equation.R") # includes Real_class
Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]

### For each trial:

results<- function(DF, pos, error_pos=NULL, err=NULL, Name=NULL, w){
  
  NN<- get.knnx(DF[pos,c("Lon", "Lat")], DF[,c("Lon", "Lat")], k=1)
  IDs<- DF[pos,"ID"][NN$nn.index]
  get<- match(IDs, DF$ID)
  Get<- as.vector(sapply(1:n_days, function(x) (x-1)*n_obs+(get)))
                         
  dists<- NN$nn.dist
  Dists<- rep(dists, 24)
  
  NN_pa<- DF[pos,"PA_site"][NN$nn.index]
  NN_PA<- rep(NN_pa, 24)
  
  RwE<- Real
  
  # Sys.time()
  if(!is.null(error_pos)){ ## Simulating measurement errors from low-cost sensors
    eps<- sapply(Deciles[error_pos], function(q) sample(Q_resids[[q]], size=1))
    # eps<- sapply(Real[error_pos], function(x) rnorm(1,mean=0,sd=err*x))
    RwE[error_pos]<- Real[error_pos]+eps
  } 
  # Sys.time() # 20s
  
  Shown<- RwE[Get]
  rm(RwE)
  Shown[which(Shown < 0)]<- 0
  
  # Sys.time()
  # Calculate AQI / Class (real already calculated):
  Shown_match<- match(round(Shown), AQI_ref$PM)
  # Sys.time() # 15 s
  # Shown_aqi<- AQI_ref$AQI[Shown_match]
  Shown_class<- AQI_ref$Class[Shown_match]
  rm(Shown_match)
  
  # Calculate differences:
  eps<- abs(Shown-Real)
  
  Results<- rep(0,39) 
  # MAE and RMSE for overall, non-white, poverty #, SDI
  # % Real > Shown, % Shown > Real, and % class difference > 1, for those 4 groups;
  # 95 percentile and frequency > |10| of PM2.5 differences, for those 4 groups
  
  ## Non-white
  NHNW<- rep((1-DF$nonHisp.white) > quantile(1-DF$nonHisp.white, 0.80), n_days) # top quintile
  NHNW_pos<- which((1-DF$nonHisp.white) > quantile(1-DF$nonHisp.white, 0.80)) # top quintile
  
  ## % poverty:
  poverty<- rep(DF$poverty > quantile(DF$poverty, 0.80), n_days) # top quintile
  poverty_pos<- which(DF$poverty > quantile(DF$poverty, 0.80)) # top quintile
                 
  ## Large misclassifications:
  very_off<- abs(Real_class-Shown_class)>1
  very_off_NHNW<- abs(Real_class[NHNW]-Shown_class[NHNW])>1
  very_off_pov<- abs(Real_class[poverty]-Shown_class[poverty])>1
  
  # ## Social deprivation:
  # # SD_pos<- which(CA_clean$SDI > quantile(CA_clean$SDI, 0.75)) # top quartile
  # SD<- rep(DF$SDI > quantile(DF$SDI, 0.80), n_days) # top quintile
  # SD_pos<- which(DF$SDI > quantile(DF$SDI, 0.80)) # top quintile
  
  
  if(w){
    
    PDW<- rep(DF$ppltn_d, n_days)
    PDW_NHNW<- PDW[NHNW]
    PDW_pov<- PDW[poverty]
    
    # Sys.time()
    Results[1]<- weighted.mean(eps, PDW)
    Results[2]<- sqrt(weighted.mean((eps)^2, PDW))
    Results[3]<- weighted.mean(eps[NHNW], PDW_NHNW)
    Results[4]<- sqrt(weighted.mean((eps[NHNW])^2, PDW_NHNW))
    Results[5]<- weighted.mean(eps[poverty], PDW_pov)
    Results[6]<- sqrt(weighted.mean((eps[poverty])^2, PDW_pov))
    # Results[7]<- weighted.mean(eps[SD],
    #                            rep(DF$ppltn_d[SD_pos], n_days))
    # Results[8]<- sqrt(weighted.mean((eps[SD])^2,
    #                                 rep(DF$ppltn_d[SD_pos], n_days)))
    # Sys.time() # 129s
    
    ## AQI class:
    Sys.time()
    Results[7]<- weighted.mean(Real_class > Shown_class, PDW)
    Results[8]<- weighted.mean(Real_class < Shown_class, PDW)
    Results[13]<- weighted.mean(very_off, PDW)
    
    Results[9]<- weighted.mean(Real_class[NHNW] > Shown_class[NHNW], PDW_NHNW)
    Results[10]<- weighted.mean(Real_class[NHNW] < Shown_class[NHNW], PDW_NHNW)
    Results[14]<- weighted.mean(very_off_NHNW, PDW_NHNW)
    
    Results[11]<- weighted.mean(Real_class[poverty] > Shown_class[poverty], PDW_pov)
    Results[12]<- weighted.mean(Real_class[poverty] < Shown_class[poverty], PDW_pov)
    Results[15]<- weighted.mean(very_off_pov, PDW_pov)
    
    # Results[15]<- weighted.mean(Real_class[SD] > Shown_class[SD], rep(DF$ppltn_d[SD_pos], n_days))
    # Results[16]<- weighted.mean(Real_class[SD] < Shown_class[SD], rep(DF$ppltn_d[SD_pos], n_days))
    # Results[20]<- weighted.mean(abs(Real_class[SD]-Shown_class[SD])>1, rep(DF$ppltn_d[SD_pos], n_days))
    # Sys.time() # 140s
    
    ## Largest errors: 
    # Sys.time()
    Results[16]<- weighted_quantile(eps, PDW, probs=0.95)
    Results[17]<- weighted.mean(eps > 10, PDW)
    
    Results[18]<- weighted_quantile(eps[NHNW], PDW_NHNW, probs=0.95)
    Results[19]<- weighted.mean(eps[NHNW] > 10, PDW_NHNW)
    
    Results[20]<- weighted_quantile(eps[poverty], PDW_pov, probs=0.95)
    Results[21]<- weighted.mean(eps[poverty] > 10, PDW_pov)
    
    # Results[27]<- weighted_quantile(eps[SD], rep(DF$ppltn_d[SD_pos], n_days), probs=0.95)
    # Results[28]<- sum(rep(DF$ppltn_d[SD_pos], n_days)[which(eps[SD] > 10)])/sum(rep(DF$ppltn_d[SD_pos], n_days))
    # Sys.time()
    
    Results[22]<- weighted.mean(dists, DF$ppltn_d)
    Results[23]<- weighted.mean(dists[NHNW_pos], DF$ppltn_d[NHNW_pos])
    Results[24]<- weighted.mean(dists[poverty_pos], DF$ppltn_d[poverty_pos])
    Results[25]<- weighted.mean(Dists[very_off], PDW[very_off])
    Results[26]<- weighted.mean(Dists[NHNW][very_off_NHNW], PDW_NHNW[very_off_NHNW])
    Results[27]<- weighted.mean(Dists[poverty][very_off_pov], PDW_pov[very_off_pov])
    
    Results[28]<- weighted_quantile(dists, DF$ppltn_d, probs=0.5)
    Results[29]<- weighted_quantile(dists[NHNW_pos], DF$ppltn_d[NHNW_pos], probs=0.5)
    Results[30]<- weighted_quantile(dists[poverty_pos], DF$ppltn_d[poverty_pos], probs=0.5)
    Results[31]<- weighted_quantile(Dists[very_off], PDW[very_off], probs=0.5)
    Results[32]<- weighted_quantile(Dists[NHNW][very_off_NHNW], PDW_NHNW[very_off_NHNW], probs=0.5)
    Results[33]<- weighted_quantile(Dists[poverty][very_off_pov], PDW_pov[very_off_pov], probs=0.5)
    
    Results[34]<- weighted.mean(NN_pa, DF$ppltn_d)
    Results[35]<- weighted.mean(NN_pa[NHNW_pos], DF$ppltn_d[NHNW_pos])
    Results[36]<- weighted.mean(NN_pa[poverty_pos], DF$ppltn_d[poverty_pos])
    Results[37]<- weighted.mean(NN_PA[very_off], PDW[very_off])
    Results[38]<- weighted.mean(NN_PA[NHNW][very_off_NHNW], PDW_NHNW[very_off_NHNW])
    Results[39]<- weighted.mean(NN_PA[poverty][very_off_pov], PDW_pov[very_off_pov])
    
  }else{
    # Sys.time()
    Results[1]<- mean(eps) 
    Results[2]<- sqrt(mean((eps)^2))
    
    Results[3]<- mean(eps[NHNW])
    Results[4]<- sqrt(mean((eps[NHNW])^2))
    
    Results[5]<- mean(eps[poverty])
    Results[6]<- sqrt(mean((eps[poverty])^2))
    
    # Results[7]<- mean(eps[SD])
    # Results[8]<- sqrt(mean((eps[SD])^2))
    # Sys.time() # 11s
    
    ## AQI class:
    # Sys.time()
    Results[7]<- mean(Real_class > Shown_class)
    Results[8]<- mean(Real_class < Shown_class)
    Results[13]<- mean(very_off)
    
    Results[9]<- mean(Real_class[NHNW] > Shown_class[NHNW])
    Results[10]<- mean(Real_class[NHNW] < Shown_class[NHNW])
    Results[14]<- mean(very_off_NHNW)
    
    Results[11]<- mean(Real_class[poverty] > Shown_class[poverty])
    Results[12]<- mean(Real_class[poverty] < Shown_class[poverty])
    Results[15]<- mean(very_off_pov)
    
    # Results[15]<- mean(Real_class[SD] > Shown_class[SD])
    # Results[16]<- mean(Real_class[SD] < Shown_class[SD])
    # Results[20]<- mean(abs(Real_class[SD]-Shown_class[SD])>1)
    # Sys.time() # 65s
    
    ## Largest errors:
    # Sys.time()
    Results[16]<- quantile(eps, 0.95)
    Results[17]<- mean(eps>10)
    
    Results[18]<- quantile(eps[NHNW], 0.95)
    Results[19]<- mean(eps[NHNW]>10)
    
    Results[20]<- quantile(eps[poverty], 0.95)
    Results[21]<- mean(eps[poverty]>10)
    
    # Results[27]<- quantile(eps[SD], 0.95)
    # Results[28]<- sum(eps[SD]>10)/length(eps[SD])
    # Sys.time() # 35s
    
    Results[22]<- mean(dists)
    Results[23]<- mean(dists[NHNW_pos])
    Results[24]<- mean(dists[poverty_pos])
    Results[25]<- mean(Dists[very_off])
    Results[26]<- mean(Dists[NHNW][very_off_NHNW])
    Results[27]<- mean(Dists[poverty][very_off_pov])
    
    Results[28]<- median(dists)
    Results[29]<- median(dists[NHNW_pos])
    Results[30]<- median(dists[poverty_pos])
    Results[31]<- median(Dists[very_off])
    Results[32]<- median(Dists[NHNW][very_off_NHNW])
    Results[33]<- median(Dists[poverty][very_off_pov])
    
    Results[34]<- mean(NN_pa)
    Results[35]<- mean(NN_pa[NHNW_pos])
    Results[36]<- mean(NN_pa[poverty_pos])
    Results[37]<- mean(NN_PA[very_off])
    Results[38]<- mean(NN_PA[NHNW][very_off_NHNW])
    Results[39]<- mean(NN_PA[poverty][very_off_pov])
    
  }
  
  return(Results)
}

## Run all trials for one experiment:
run_sim<- function(seed_num, no_err_set, err_set, frac = NULL, num = 100, 
                   road_weights = NULL, err=NULL, weighted=FALSE){
  # road_weights can also be CES score weights!
  
  set.seed(7*seed_num)
  if(is.null(road_weights)){
    if(is.null(frac)){
      these<- sample(err_set, num, replace=FALSE)
    }else{
      these<- sample(err_set, frac*length(err_set), replace=FALSE)
    }
  }else{
    if(is.null(frac)){
      these<- sample(err_set, num, replace=FALSE, prob = road_weights)
    }else{
      these<- sample(err_set, frac*length(err_set), replace=FALSE, prob = road_weights)
    }
  }
  
  # print(seed_num)
  if(is.null(err)){
    return(results(CA_clean, unique(c(no_err_set,these)), error_pos=these, w = weighted))
  }else{
    return(results(CA_clean, unique(c(no_err_set,these)), error_pos=these, err=err, w = weighted))
  }
}

## Testing:
                 
roads<- read.csv("LCS_data/Hwy_lengths.csv")[!my_nas0,]
lengths<- roads$Roads_500 + 0.1
rWeights<- lengths/sum(lengths)

sink("Timing_one_sim_366_road-weighting.txt")  
                 
s<- Sys.time()
# res<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), num=1000)
                 
res<- run_sim(303, which(CA_clean$AQS_site==1), 1:dim(CA_clean)[1], 
                    num=1000, road_weights = rWeights)
e<- Sys.time()
print(paste("Unweighted:", e-s)) 
# 2.8 mins unweighted, 366 days --> would take 140 mins to run 50 trials
# 54 secs unweighted, 183 days (every other) --> would take 45 mins to run 50 trials
# 3.1 secs unweighted, 24 days --> would take 2.5 mins to run 50 trials
# 1.7 secs unweighted, 12 days --> would take 1.4 mins to run 50 trials

# plot(c(12, 24, 183, 366), c(1.7, 3.1, 54, 2.8*60), 
#      xlab = "Days", ylab = "Seconds / Trial")
# abline(0,0.5)


s<- Sys.time()
# res<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), num=1000,
#               weighted=TRUE)
                 
res<- run_sim(303, which(CA_clean$AQS_site==1), 1:dim(CA_clean)[1], 
                    num=1000, road_weights = rWeights, weighted = TRUE)
e<- Sys.time()
print(paste("Weighted:", e-s)) 
                 
print(gc())
                 
sink()
                 
# 2.9 mins weighted, 366 days --> would take  mins to run 50 trials
# 58 secs weighted, 183 days (every other) --> would take  mins to run 50 trials
# 4.4 secs weighted, 24 days --> would take  mins to run 50 trials
# 1.5 secs weighted, 12 days --> would take  mins to run 50 trials

# plot(c(12, 24, 183, 366), c(1.5, 4.4, 58, 2.9*60)*50/60,
#      xlab = "Days of Data", ylab = "Minutes / 50 Trials", col = "red", 
#      main = "PD-weighted = red, unweighted = blue") 
# points(c(12, 24, 183, 366), c(1.7, 3.1, 54, 2.8*60)*50/60, col="blue") # basically the same
# x<- c(12, 24, 183, 366)
# x2<- x^2
# y<- c(1.5, 4.4, 58, 2.9*60)*50/60
# test<- lm(y ~ x2)
# lines(x, test$coefficients[1] + test$coefficients[2]*x2)


### Density plot of errors vs real obs

# # Set up:
# DF<- CA_clean
# no_err_set<- which(DF$AQS_site == 1)
# err_set<- which(DF$PA == 1)
# num<- 1000
# seed_num<- 303
# 
# pos<- unique(c(no_err_set,these))
# error_pos<- these

# Errors<- Shown - Real
# # Randomly sample 1000 observations at which to assess NN impact
# set.seed(7*303)
# pick<- sample(1:length(Errors), 1000, replace = FALSE)
# xy<- data.frame(Real, Errors)[pick,] #[error_pos,]

# scatter<- qplot(Real, Errors, data=xy) +
#   scale_x_continuous(limits=c(min(xy$Real),max(xy$Real))) + 
#   scale_y_continuous(limits=c(min(xy$Errors),max(xy$Errors))) + 
#   geom_rug(col=rgb(.5,0,0,alpha=.2))
# scatter

# # Histograms for different levels of real PM:
# windows()
# par(mfrow=c(2,2))
# hist(Errors[which(Real <= 12)],
#      main = "Errors from Good AQ", breaks = seq(-70, 80, 5))
# hist(Errors[which(Real <= 35.5 & Real > 12)], 
#      main = "Errors from Moderate AQ", breaks = seq(-70, 80, 5))
# hist(Errors[which(Real <= 55.5 & Real > 35.5)],  
#      main = "Errors from AQ Unhealthy for Sensitive Groups", breaks = seq(-70, 80, 5))
# hist(Errors[which(Real > 55.5)], 
#      main = "Errors from Unhealthy AQ", breaks = seq(-70, 80, 5))

# ## Heat map:
# err_seq<- seq(min(Errors[error_pos]), max(Errors[error_pos]), length.out = 100) # Errors<- Shown - Real
# real_seq<- seq(min(Real[error_pos]), max(Real[error_pos]), length.out = 100)
# res<- matrix(0,nrow=100, ncol=100)
# for(i in 1:length(error_pos)){
#   x<- 1
#   y<- 1
#   while(Real[error_pos][i] > real_seq[x]){ x<- x+1 }
#   while(Errors[error_pos][i] > err_seq[y]){ y<- y+1 }
#   res[x,y]<- res[x,y] + 1
# }

# res_df<- data.frame(Real=rep(real_seq, 100), Errors=sort(rep(err_seq, 100)),
#                     Density = as.vector(res)/sum(res))

# ggplot(res_df, aes(Real, Errors)) + geom_raster(aes(fill=Density))


