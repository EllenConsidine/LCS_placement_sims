# Preliminary code adapated from Sim_functions.R
# Still use same functions

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

##################### Pollution Score

CA_clean<- readRDS("CA_with_SDI2_CES.rds") 
# sum(is.na(CA_clean$CES_score)) # 4847/475772 = 1%
# sum(is.na(CA_clean$Pollution_score)) # 5
CES<- CA_clean[which(!is.na(CA_clean$Pollution_score)),]
n_obs<- dim(CES)[1]
my_nas0<- readRDS("CA_NA_pos.rds")
my_nas0[which(my_nas0 == FALSE)][which(is.na(CA_clean$Pollution_score))]<- TRUE
rm("CA_clean")

days<- c(15, 46, 75, 106,
         136, 167, 197, 228,
         259, 289, 320, 350) # 15th of each month

# days<- c(1, 15, 32, 46, 61, 75, 92, 106,
#          122, 136, 153, 167, 183, 197, 214, 228,
#          245, 259, 275, 289, 306, 320, 336, 350) # 1st and 15th of each month

# days<- seq(2,366,by=2) # every other day
n_days<- length(days)

Real<- readRDS("Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]

# load("Analysis/Simulate_PA_ME.RData")
source("Intermediate data/Calibrate_PA.R") # includes Deciles for Real
Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
source("Analysis/AQI_equation.R") # includes Real_class
Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]


cWeights<- CES$Pollution_score/sum(CES$Pollution_score)
CA_clean<- CES

## Weighted
for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CES$AQS_site==1), 1:dim(CES)[1], 
                    num=n, road_weights = cWeights, weighted = TRUE)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CES$AQS_site==1), 
                                     1:dim(CES)[1], 
                                     num=n, road_weights = cWeights, 
                                     weighted = TRUE))
    print(i)
  }
  write.csv(Results, paste0("Analysis/Results/D1-CES_pollution_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-CES_pollution_N_",n,"_avg.csv"),
            row.names = FALSE)
}

## Unweighted
for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CES$AQS_site==1), 1:dim(CES)[1], 
                    num=n, road_weights = cWeights)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CES$AQS_site==1), 
                                     1:dim(CES)[1], 
                                     num=n, road_weights = cWeights))
    print(i)
  }
  write.csv(Results, paste0("Analysis/Results/D1-CES_pollution_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-CES_pollution_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}


##################### CES Score

CA_clean<- readRDS("CA_with_SDI2_CES.rds") 
# sum(is.na(CA_clean$CES_score)) # 4847/475772 = 1%
# sum(is.na(CA_clean$Pollution_score)) # 5
CES<- CA_clean[which(!is.na(CA_clean$CES_score)),]
n_obs<- dim(CES)[1]
my_nas0<- readRDS("CA_NA_pos.rds")
my_nas0[which(my_nas0 == FALSE)][which(is.na(CA_clean$CES_score))]<- TRUE
rm("CA_clean")

days<- c(1, 15, 32, 46, 61, 75, 92, 106,
         122, 136, 153, 167, 183, 197, 214, 228,
         245, 259, 275, 289, 306, 320, 336, 350) # 1st and 15th of each month
# days<- seq(2,366,by=2) # every other day
n_days<- length(days)

Real<- readRDS("Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]

# load("Analysis/Simulate_PA_ME.RData")
source("Intermediate data/Calibrate_PA.R") # includes Deciles for Real
Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
source("Analysis/AQI_equation.R") # includes Real_class
Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]


cWeights<- CES$CES_score/sum(CES$CES_score)
CA_clean<- CES

## Weighted
for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CES$AQS_site==1), 1:dim(CES)[1], 
                    num=n, road_weights = cWeights, weighted = TRUE)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CES$AQS_site==1), 
                                     1:dim(CES)[1], 
                                     num=n, road_weights = cWeights, 
                                     weighted = TRUE))
    print(i)
  }
  write.csv(Results, paste0("Analysis/Results/D1-CES_score_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-CES_score_N_",n,"_avg.csv"),
            row.names = FALSE)
}

## Unweighted
for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CES$AQS_site==1), 1:dim(CES)[1], 
                    num=n, road_weights = cWeights)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CES$AQS_site==1), 
                                     1:dim(CES)[1], 
                                     num=n, road_weights = cWeights))
    print(i)
  }
  write.csv(Results, paste0("Analysis/Results/D1-CES_score_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-CES_score_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}



