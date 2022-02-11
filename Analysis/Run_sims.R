setwd("/n/home13/econsidine")

## Using functions from Sim_functions.R
source("LCS_placement_sims/Analysis/Sim_functions.R")

# ## AQS monitors only (just have to do this once, then can comment out for the rest of the simulations):
# Results<- results(CA_clean, which(CA_clean$AQS_site == 1), w = TRUE)
# write.csv(Results[[1]], paste0("New_LCS_results/D366-AQS_sites.csv"),
#           row.names = FALSE)
# write.csv(Results[[2]], paste0("New_LCS_results/D366-AQS_sites_unweighted.csv"),
#           row.names = FALSE)

# print("Finished with AQS")

#### For each different type & amount of sensor measurement error:
name<- "Diff-025" # Change this name
folder<- "LCS_results/"

## All PurpleAir sensor locations:
# Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
#                   num=sum(CA_clean$PA_site), weighted = TRUE)
# write.csv(Results[[1]], paste0(folder, name, "-All_PA_sites.csv"),
#           row.names = FALSE)
# write.csv(Results[[2]], paste0(folder, name, "-All_PA_sites_unweighted.csv"),
#           row.names = FALSE)
# print("Finished with All-PurpleAir")

n<- sum(CA_clean$PA_site==1)
Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
                    num=n, weighted = TRUE)
W_Results<- Results[[1]]
UNW_Results<- Results[[2]]

for(i in 2:50){
  Results<- run_sim(303*i, which(CA_clean$AQS_site==1), 
                                   which(CA_clean$PA_site==1), 
                                   num=n, weighted = TRUE)
  W_Results<- rbind(W_Results, Results[[1]])
  UNW_Results<- rbind(UNW_Results, Results[[2]])
  print(i)
}
write.csv(W_Results, paste0(folder, name, "-PA_N_",n,".csv"), 
          row.names = FALSE)
W_avg_res<- apply(W_Results, MARGIN = 2, mean)
write.csv(W_avg_res, paste0(folder, name, "-PA_N_",n,"_avg.csv"),
          row.names = FALSE)

write.csv(UNW_Results, paste0(folder, name, "-PA_N_",n,"_unweighted.csv"), 
          row.names = FALSE)
UNW_avg_res<- apply(UNW_Results, MARGIN = 2, mean)
write.csv(UNW_avg_res, paste0(folder, name, "-PA_N_",n,"_unweighted_avg.csv"),
          row.names = FALSE)
print(paste("Finished with All-PurpleAir", name))

# ## Purple Air sites:

# for(n in c(50, 100, 250, 500, 1000)){
#   Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
#                     num=n, weighted = TRUE)
#   W_Results<- Results[[1]]
#   UNW_Results<- Results[[2]]
  
#   for(i in 2:50){
#     Results<- run_sim(303*i, which(CA_clean$AQS_site==1), 
#                                      which(CA_clean$PA_site==1), 
#                                      num=n, weighted = TRUE)
#     W_Results<- rbind(W_Results, Results[[1]])
#     UNW_Results<- rbind(UNW_Results, Results[[2]])
#     print(i)
#   }
#   write.csv(W_Results, paste0(folder, name, "-PA_N_",n,".csv"), 
#             row.names = FALSE)
#   W_avg_res<- apply(W_Results, MARGIN = 2, mean)
#   write.csv(W_avg_res, paste0(folder, name, "-PA_N_",n,"_avg.csv"),
#             row.names = FALSE)
          
#   write.csv(UNW_Results, paste0(folder, name, "-PA_N_",n,"_unweighted.csv"), 
#             row.names = FALSE)
#   UNW_avg_res<- apply(UNW_Results, MARGIN = 2, mean)
#   write.csv(UNW_avg_res, paste0(folder, name, "-PA_N_",n,"_unweighted_avg.csv"),
#             row.names = FALSE)
# }

# print("Finished with PA")

# ## School sites:

# for(n in c(50, 100, 250, 500, 1000)){ 
#   Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$School==1), 
#                     num=n, weighted = TRUE)
#   W_Results<- Results[[1]]
#   UNW_Results<- Results[[2]]
          
#   for(i in 2:50){
#     Results<- run_sim(303*i, which(CA_clean$AQS_site==1), 
#                                      which(CA_clean$School==1), 
#                                      num=n, weighted = TRUE)
#     W_Results<- rbind(W_Results, Results[[1]])
#     UNW_Results<- rbind(UNW_Results, Results[[2]])
#     print(i)
#   }
#   write.csv(W_Results, paste0(folder, name, "-School_N_",n,".csv"), 
#             row.names = FALSE)
#   W_avg_res<- apply(W_Results, MARGIN = 2, mean)
#   write.csv(W_avg_res, paste0(folder, name, "-School_N_",n,"_avg.csv"),
#             row.names = FALSE)
          
#   write.csv(UNW_Results, paste0(folder, name, "-School_N_",n,"_unweighted.csv"), 
#             row.names = FALSE)
#   UNW_avg_res<- apply(UNW_Results, MARGIN = 2, mean)
#   write.csv(UNW_avg_res, paste0(folder, name, "-School_N_",n,"_unweighted_avg.csv"),
#             row.names = FALSE)
# }

# print("Finished with Schools")


# ## Road weighting:
# roads<- read.csv("LCS_data/Hwy_lengths.csv")[!my_nas0,]
# # sum(roads$Roads_500 > 0.5) # 21121

# # Add 0.1 to each location so it has a non-zero chance of getting picked
# lengths<- roads$Roads_500 + 0.1
# rWeights<- lengths/sum(lengths)

# for(n in c(50, 100, 250, 500, 1000)){
#   Results<- run_sim(303, which(CA_clean$AQS_site==1), 1:dim(CA_clean)[1], 
#                     num=n, road_weights = rWeights, weighted = TRUE)
#   W_Results<- Results[[1]]
#   UNW_Results<- Results[[2]]
#   for(i in 2:50){
#     Results<- run_sim(303*i, which(CA_clean$AQS_site==1), 
#                                      1:dim(CA_clean)[1], 
#                                      num=n, road_weights = rWeights, 
#                                      weighted = TRUE)
#     W_Results<- rbind(W_Results, Results[[1]])
#     UNW_Results<- rbind(UNW_Results, Results[[2]])
#     print(i)
#   }
#   write.csv(W_Results, paste0(folder, name, "-Roads_N_",n,".csv"), 
#             row.names = FALSE)
#   W_avg_res<- apply(W_Results, MARGIN = 2, mean)
#   write.csv(W_avg_res, paste0(folder, name, "-Roads_N_",n,"_avg.csv"),
#             row.names = FALSE)
          
#   write.csv(UNW_Results, paste0(folder, name, "-Roads_N_",n,"_unweighted.csv"), 
#             row.names = FALSE)
#   UNW_avg_res<- apply(UNW_Results, MARGIN = 2, mean)
#   write.csv(UNW_avg_res, paste0(folder, name, "-Roads_N_",n,"_unweighted_avg.csv"),
#             row.names = FALSE)
# }

# print("Finished with Roads")

# ###################################################

# ##### Using CalEnviroScreen indices: have to re-load the data because there are more NAs to remove

# ### Pollution Score:

# CA_clean<- readRDS("LCS_data/CA_with_CES_projected.rds") 
# # sum(is.na(CA_clean$CES_score)) # 4847/475772 = 1%
# # sum(is.na(CA_clean$Pollution_score)) # 5
# CES<- CA_clean[which(!is.na(CA_clean$Pollution_score)),]
# n_obs<- dim(CES)[1]
# # my_nas0<- readRDS("LCS_data/CA_NA_pos.rds")
# my_nas0[which(my_nas0 == FALSE)][which(is.na(CA_clean$Pollution_score))]<- TRUE
# CA_clean<- CES

# Real<- readRDS("LCS_data/Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]

# # load("Analysis/Simulate_PA_ME.RData")
# source("LCS_placement_sims/Analysis/Calibrate_PA.R") # includes Deciles for Real
# Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
# source("LCS_placement_sims/Analysis/AQI_equation.R") # includes Real_class
# Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
                                         
# Real_class1<- Real_class > 2
                                    
# pWeights<- CES$Pollution_score/sum(CES$Pollution_score)

# for(n in c(50, 100, 250, 500, 1000)){
#   Results<- run_sim(303, which(CES$AQS_site==1), 1:dim(CES)[1], 
#                     num=n, road_weights = pWeights, weighted = TRUE)
#   W_Results<- Results[[1]]
#   UNW_Results<- Results[[2]]
#   for(i in 2:50){
#     Results<- run_sim(303*i, which(CES$AQS_site==1), 
#                                      1:dim(CES)[1], 
#                                      num=n, road_weights = pWeights, 
#                                      weighted = TRUE)
#     W_Results<- rbind(W_Results, Results[[1]])
#     UNW_Results<- rbind(UNW_Results, Results[[2]])
#     print(i)
#   }
#   write.csv(W_Results, paste0(folder, name, "-CES_pollution_N_",n,".csv"), 
#             row.names = FALSE)
#   W_avg_res<- apply(W_Results, MARGIN = 2, mean)
#   write.csv(W_avg_res, paste0(folder, name, "-CES_pollution_N_",n,"_avg.csv"),
#             row.names = FALSE)
          
#   write.csv(UNW_Results, paste0(folder, name, "-CES_pollution_N_",n,"_unweighted.csv"), 
#             row.names = FALSE)
#   UNW_avg_res<- apply(UNW_Results, MARGIN = 2, mean)
#   write.csv(UNW_avg_res, paste0(folder, name, "-CES_pollution_N_",n,"_unweighted_avg.csv"),
#             row.names = FALSE)
# }

# print("Finished with Pollution score")
                                         
# ##################### CES Score

# CA_clean<- readRDS("LCS_data/CA_with_CES_projected.rds") 
# # sum(is.na(CA_clean$CES_score)) # 4847/475772 = 1%
# # sum(is.na(CA_clean$Pollution_score)) # 5
# CES<- CA_clean[which(!is.na(CA_clean$CES_score)),]
# n_obs<- dim(CES)[1]
# my_nas0<- readRDS("LCS_data/CA_NA_pos.rds")
# my_nas0[which(my_nas0 == FALSE)][which(is.na(CA_clean$CES_score))]<- TRUE
# CA_clean<- CES

# Real<- readRDS("LCS_data/Daily_PM25_CA.rds")[as.vector(sapply(days, function(x) (x-1)*length(my_nas0)+(1:length(my_nas0))))][rep(!my_nas0,n_days)]

# # load("Analysis/Simulate_PA_ME.RData")
# source("LCS_placement_sims/Analysis/Calibrate_PA.R") # includes Deciles for Real
# Deciles<- Deciles[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
# source("LCS_placement_sims/Analysis/AQI_equation.R") # includes Real_class
# Real_class<- Real_class[as.vector(sapply(days, function(x) (x-1)*n_obs+(1:n_obs)))]
                                         
# Real_class1<- Real_class > 2

# cWeights<- CES$CES_score/sum(CES$CES_score)


# for(n in c(50, 100, 250, 500, 1000)){
#   Results<- run_sim(303, which(CES$AQS_site==1), 1:dim(CES)[1], 
#                     num=n, road_weights = cWeights, weighted = TRUE)
#   W_Results<- Results[[1]]
#   UNW_Results<- Results[[2]]
#   for(i in 2:50){
#     Results<- run_sim(303*i, which(CES$AQS_site==1), 
#                                      1:dim(CES)[1], 
#                                      num=n, road_weights = cWeights, 
#                                      weighted = TRUE)
#     W_Results<- rbind(W_Results, Results[[1]])
#     UNW_Results<- rbind(UNW_Results, Results[[2]])
#     print(i)
#   }
#   write.csv(W_Results, paste0(folder, name, "-CES_score_N_",n,".csv"), 
#             row.names = FALSE)
#   W_avg_res<- apply(W_Results, MARGIN = 2, mean)
#   write.csv(W_avg_res, paste0(folder, name, "-CES_score_N_",n,"_avg.csv"),
#             row.names = FALSE)
          
#   write.csv(UNW_Results, paste0(folder, name, "-CES_score_N_",n,"_unweighted.csv"), 
#             row.names = FALSE)
#   UNW_avg_res<- apply(UNW_Results, MARGIN = 2, mean)
#   write.csv(UNW_avg_res, paste0(folder, name, "-CES_score_N_",n,"_unweighted_avg.csv"),
#             row.names = FALSE)
# }

# print("Finished with CES score")

