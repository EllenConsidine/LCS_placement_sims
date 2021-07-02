## Using functions from Sim_functions.R

#### Using numbers of sensors instead of percents:

Results<- results(CA_clean, which(CA_clean$AQS_site == 1), w = TRUE)
write.csv(Results, paste0("Analysis/Results/D1-AQS_sites.csv"),
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
  write.csv(Results, paste0("Analysis/Results/D1-PA_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-PA_N_",n,"_avg.csv"),
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
  write.csv(Results, paste0("Analysis/Results/D1-School_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-School_N_",n,"_avg.csv"),
            row.names = FALSE)
}


### Unweighted by population density

Results<- results(CA_clean, which(CA_clean$AQS_site == 1), w = FALSE)
write.csv(Results, paste0("Analysis/Results/D1-AQS_sites_unweighted.csv"),
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
  write.csv(Results, paste0("Analysis/Results/D1-PA_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-PA_N_",n,"_unweighted_avg.csv"),
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
  write.csv(Results, paste0("Analysis/Results/D1-School_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-School_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}

#### Road weighting:
roads<- read.csv("Intermediate data/Hwy_lengths.csv")[!my_nas0,]
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
  write.csv(Results, paste0("Analysis/Results/D1-Roads_N_",n,".csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-Roads_N_",n,"_avg.csv"),
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
  write.csv(Results, paste0("Analysis/Results/D1-Roads_N_",n,"_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/D1-Roads_N_",n,"_unweighted_avg.csv"),
            row.names = FALSE)
}



####################################### Fractions of sites...

### Population density weighted

## Purple Air sites

Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
        frac=1, weighted = TRUE)
write.csv(Results, paste0("Analysis/Results/All_PA_sites_24_days.csv"), 
          row.names = FALSE)

for(f in c(0.1, 0.25, 0.5)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
                    frac=f, weighted = TRUE)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     which(CA_clean$PA_site==1), 
                                     frac=f, weighted = TRUE))
    print(i)
  }
  write.csv(Results, paste0("Analysis/Results/PA_",f*100,"_percent.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/PA_",f*100,"_percent_avg.csv"),
            row.names = FALSE)
}

## School sites

Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$School==1), 
                  frac=1, weighted = TRUE)
write.csv(Results, paste0("Analysis/Results/All_Schools_24_days.csv"), 
          row.names = FALSE)

for(f in c(0.1, 0.25, 0.5)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$School==1), 
                    frac=f, weighted = TRUE)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     which(CA_clean$School==1), 
                                     frac=f, weighted = TRUE))
    print(i)
  }
  write.csv(Results, paste0("Analysis/Results/School_",f*100,"_percent.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/School_",f*100,"_percent_avg.csv"),
            row.names = FALSE)
}


### Unweighted by population density

## Purple Air sites

Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
                  frac=1)
write.csv(Results, paste0("Analysis/Results/All_PA_sites_24_days_unweighted.csv"), 
          row.names = FALSE)

for(f in c(0.1, 0.25, 0.5)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$PA_site==1), 
                    frac=f)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     which(CA_clean$PA_site==1), 
                                     frac=f))
    print(i)
  }
  write.csv(Results, paste0("Analysis/Results/PA_",f*100,"_percent_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/PA_",f*100,"_percent_unweighted_avg.csv"),
            row.names = FALSE)
}

## School sites

Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$School==1), 
                  frac=1)
write.csv(Results, paste0("Analysis/Results/All_Schools_24_days_unweighted.csv"), 
          row.names = FALSE)

for(f in c(0.1, 0.25, 0.5)){
  Results<- run_sim(303, which(CA_clean$AQS_site==1), which(CA_clean$School==1), 
                    frac=f)
  for(i in 2:50){
    Results<- rbind(Results, run_sim(303*i, which(CA_clean$AQS_site==1), 
                                     which(CA_clean$School==1), 
                                     frac=f))
    print(i)
  }
  write.csv(Results, paste0("Analysis/Results/School_",f*100,"_percent_unweighted.csv"), 
            row.names = FALSE)
  avg_res<- apply(Results, MARGIN = 2, mean)
  write.csv(avg_res, paste0("Analysis/Results/School_",f*100,"_percent_unweighted_avg.csv"),
            row.names = FALSE)
}
