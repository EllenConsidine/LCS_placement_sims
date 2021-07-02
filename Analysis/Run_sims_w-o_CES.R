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




