setwd("/n/home13/econsidine")
folder<- "Revised_LCS_results/"

## Using functions from Sim_functions.R
source("LCS_placement_sims/Analysis/Sim_functions.R")

args<- commandArgs(trailing = TRUE) # name of experiment (ME type+amount), number of lcs, placement strategy,  

name<- args[1] 
num_lcs<- args[2]
strategy<- args[3]

# print(args)

## Prepare data:

weight_samp<- FALSE

if(strategy=="PurpleAir"){
  locs<- which(CA_clean$PA_site==1)
}else if(strategy=="Schools"){
  locs<- which(CA_clean$School==1)
}else if(strategy=="Roads"){
  weight_samp<- TRUE
  
  roads<- read.csv("LCS_data/Hwy_lengths.csv")[!my_nas0,]
  lengths<- roads$Roads_500 + 0.1
  rWeights<- lengths/sum(lengths)
}else if(strategy == "Pollution"){
  weight_samp<- TRUE
  
  CA_clean<- readRDS("LCS_data/CA_with_CES_projected.rds")
  CES<- CA_clean[which(!is.na(CA_clean$Pollution_score)),]
  n_obs<- dim(CES)[1]
  my_nas0[which(my_nas0 == FALSE)][which(is.na(CA_clean$Pollution_score))]<- TRUE
  CA_clean<- CES
  
  Real<- readRDS("LCS_data/Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]
  source("LCS_placement_sims/Analysis/Calibrate_PA.R") # includes Deciles for Real
  Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
  source("LCS_placement_sims/Analysis/AQI_equation.R") # includes Real_class
  Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
  Real_class1<- Real_class > 2
  
  rWeights<- CES$Pollution_score/sum(CES$Pollution_score)
}else if(strategy == "CES"){ 
  weight_samp<- TRUE
  
  CA_clean<- readRDS("LCS_data/CA_with_CES_projected.rds") 
  CES<- CA_clean[which(!is.na(CA_clean$CES_score)),]
  n_obs<- dim(CES)[1]
  my_nas0<- readRDS("LCS_data/CA_NA_pos.rds")
  my_nas0[which(my_nas0 == FALSE)][which(is.na(CA_clean$CES_score))]<- TRUE
  CA_clean<- CES

  Real<- readRDS("LCS_data/Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]
  source("LCS_placement_sims/Analysis/Calibrate_PA.R") # includes Deciles for Real
  Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
  source("LCS_placement_sims/Analysis/AQI_equation.R") # includes Real_class
  Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
  Real_class1<- Real_class > 2
  
  rWeights<- CES$CES_score/sum(CES$CES_score)
}

## Run sims:

if(weight_samp == FALSE){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), locs, 
                    num=num_lcs, weighted = TRUE, name=name)
  W_Results<- Results[[1]]
  UNW_Results<- Results[[2]]
  
  for(i in 2:100){
    Results<- run_sim(303*i, which(CA_clean$AQS_site==1), locs, 
                                     num=num_lcs, weighted = TRUE, name=name)
    W_Results<- rbind(W_Results, Results[[1]])
    UNW_Results<- rbind(UNW_Results, Results[[2]])
    print(i)
  }
}else{
  Results<- run_sim(303, which(CA_clean$AQS_site==1), 1:dim(CA_clean)[1], 
                    num=num_lcs, weighted = TRUE, name=name, road_weights=rWeights)
  W_Results<- Results[[1]]
  UNW_Results<- Results[[2]]
  
  for(i in 2:100){
    Results<- run_sim(303*i, which(CA_clean$AQS_site==1), 1:dim(CA_clean)[1], 
                      num=num_lcs, weighted = TRUE, name=name, road_weights=rWeights)
    W_Results<- rbind(W_Results, Results[[1]])
    UNW_Results<- rbind(UNW_Results, Results[[2]])
    print(i)
  }
}


## Save results:
write.csv(W_Results, paste0(folder, name, "_", strategy, "_", num_lcs, ".csv"), 
          row.names = FALSE)
W_avg_res<- apply(W_Results, MARGIN = 2, mean)
write.csv(W_avg_res, paste0(folder, name, "_", strategy, "_", num_lcs,"_avg.csv"),
          row.names = FALSE)

write.csv(UNW_Results, paste0(folder, name, "_", strategy, "_", num_lcs,"_unweighted.csv"), 
          row.names = FALSE)
UNW_avg_res<- apply(UNW_Results, MARGIN = 2, mean)
write.csv(UNW_avg_res, paste0(folder, name, "_", strategy, "_", num_lcs,"_unweighted_avg.csv"),
          row.names = FALSE)

