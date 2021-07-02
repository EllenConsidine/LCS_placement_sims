## Using functions from Sim_functions.R
source("LCS_placement_sims/Analysis/Sim_functions.R")

#### Using numbers of sensors instead of percents:

Results<- results(CA_clean, which(CA_clean$AQS_site == 1), w = TRUE)
write.csv(Results, paste0("LCS_results/D366-AQS_sites.csv"),
          row.names = FALSE)

## Purple Air sites

for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
                    num=n, weighted = TRUE)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     which(CA_clean$PA_site==1), 
                                     num=n, weighted = TRUE))
    print(i)
  }
  write.csv(Results, paste0("LCS_results/D366-PA_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-PA_N_",n,"_avg.csv"),
            row.names = FALSE)
}

## School sites

for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$School==1), 
                    num=n, weighted = TRUE)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     which(CA_clean$School==1), 
                                     num=n, weighted = TRUE))
    print(i)
  }
  write.csv(Results, paste0("LCS_results/D366-School_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-School_N_",n,"_avg.csv"),
            row.names = FALSE)
}


### Unweighted by population density

Results<- results(CA_clean, which(CA_clean$AQS_site == 1), w = FALSE)
write.csv(Results, paste0("LCS_results/D366-AQS_sites_unweighted.csv"),
          row.names = FALSE)

## Purple Air sites

for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
                    num=n)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     which(CA_clean$PA_site==1), 
                                     num=n))
    print(i)
  }
  write.csv(Results, paste0("LCS_results/D366-PA_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-PA_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}

## School sites

for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$School==1), 
                    num=n)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     which(CA_clean$School==1), 
                                     num=n))
    print(i)
  }
  write.csv(Results, paste0("LCS_results/D366-School_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-School_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}

#### Road weighting:
roads<- read.csv("LCS_data/Hwy_lengths.csv")[!my_nas0,]
# sum(roads$Roads_500 > 0.5) # 21121

# Add 0.1 to each location so it has a chance of getting picked
lengths<- roads$Roads_500 + 0.1
rWeights<- lengths/sum(lengths)

## Weighted
for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), 1:dim(CA_clean)[1], 
                    num=n, road_weights = rWeights, weighted = TRUE)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     1:dim(CA_clean)[1], 
                                     num=n, road_weights = rWeights, 
                                     weighted = TRUE))
    print(i)
  }
  write.csv(Results, paste0("LCS_results/D366-Roads_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-Roads_N_",n,"_avg.csv"),
            row.names = FALSE)
}

## Unweighted
for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), 1:dim(CA_clean)[1], 
                    num=n, road_weights = rWeights)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     1:dim(CA_clean)[1], 
                                     num=n, road_weights = rWeights))
    print(i)
  }
  write.csv(Results, paste0("LCS_results/D366-Roads_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-Roads_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}

###################################################

##### Using CalEnviroScreen indices:

##################### Pollution Score

CA_clean<- readRDS("LCS_data/CA_with_SDI2_CES.rds") 
# sum(is.na(CA_clean$CES_score)) # 4847/475772 = 1%
# sum(is.na(CA_clean$Pollution_score)) # 5
CES<- CA_clean[which(!is.na(CA_clean$Pollution_score)),]
n_obs<- dim(CES)[1]
# my_nas0<- readRDS("LCS_data/CA_NA_pos.rds")
my_nas0[which(my_nas0 == FALSE)][which(is.na(CA_clean$Pollution_score))]<- TRUE
rm("CA_clean")

Real<- readRDS("LCS_data/Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]

# load("Analysis/Simulate_PA_ME.RData")
source("LCS_placement_sims/Analysis/Calibrate_PA.R") # includes Deciles for Real
Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
source("LCS_placement_sims/Analysis/AQI_equation.R") # includes Real_class
Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]

                                         
pWeights<- CES$Pollution_score/sum(CES$Pollution_score)
CA_clean<- CES

## Weighted
for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CES$AQS_site==1), 1:dim(CES)[1], 
                    num=n, road_weights = pWeights, weighted = TRUE)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CES$AQS_site==1), 
                                     1:dim(CES)[1], 
                                     num=n, road_weights = pWeights, 
                                     weighted = TRUE))
    print(i)
  }
  write.csv(Results, paste0("LCS_results/D366-CES_pollution_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-CES_pollution_N_",n,"_avg.csv"),
            row.names = FALSE)
}

## Unweighted
for(n in c(50, 100, 250, 500, 1000)){
  Results<- run_sim(303, which(CES$AQS_site==1), 1:dim(CES)[1], 
                    num=n, road_weights = pWeights)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CES$AQS_site==1), 
                                     1:dim(CES)[1], 
                                     num=n, road_weights = pWeights))
    print(i)
  }
  write.csv(Results, paste0("LCS_results/D366-CES_pollution_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-CES_pollution_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}

                                         
##################### CES Score

CA_clean<- readRDS("LCS_data/CA_with_SDI2_CES.rds") 
# sum(is.na(CA_clean$CES_score)) # 4847/475772 = 1%
# sum(is.na(CA_clean$Pollution_score)) # 5
CES<- CA_clean[which(!is.na(CA_clean$CES_score)),]
n_obs<- dim(CES)[1]
my_nas0<- readRDS("LCS_data/CA_NA_pos.rds")
my_nas0[which(my_nas0 == FALSE)][which(is.na(CA_clean$CES_score))]<- TRUE
rm("CA_clean")

Real<- readRDS("LCS_data/Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]

# load("Analysis/Simulate_PA_ME.RData")
source("LCS_placement_sims/Analysis/Calibrate_PA.R") # includes Deciles for Real
Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
source("LCS_placement_sims/Analysis/AQI_equation.R") # includes Real_class
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
  write.csv(Results, paste0("LCS_results/D366-CES_score_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-CES_score_N_",n,"_avg.csv"),
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
  write.csv(Results, paste0("LCS_results/D366-CES_score_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("LCS_results/D366-CES_score_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}

