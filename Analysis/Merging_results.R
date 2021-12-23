library(stringr)

setwd("/n/home13/econsidine")

Metrics<- c(rep(c("MAE", "RMSE"),3),
            rep(c("Real class > shown class","Shown class > real class"),3),
            rep("AQI class off by >1",3),
            rep(c("95th percentile of errors",
                  "% of the |errors| > 10"),3),
            rep(c("Mean distance to NN monitor/sensor"),3),
            rep(c("Mean distance to NN monitor/sensor, among misclassifications > 1"),3),
            rep(c("Median distance to NN monitor/sensor"),3),
            rep(c("Median distance to NN monitor/sensor, among misclassifications > 1"),3),
            rep(c("Mean % of NNs that are LCSs"),3),
            rep(c("Mean % of NNs that are LCSs, among misclassifications > 1"),3),
            rep("Mean distance to NN monitor/sensor, among all misclassifications",3),
            rep("Median distance to NN monitor/sensor, among all misclassifications",3),
            rep("Mean % of NNs that are LCSs, among all misclassifications",3),
            rep("% Unhealthy and Showing Healthy",3))
prefix<- "SA-010-" # "D366-"
suffix<- "_avg.csv"

directory<- "New_LCS_results"

pre_files<- list.files(directory,
           prefix)
suff_files<- list.files(directory,
                        suffix)
files<- intersect(pre_files, suff_files)

unweighted<- str_detect(files, "unw")

# Get unweighted

UNW_names<- sapply(files[unweighted], function(x) str_split(x, "-")[[1]][2])
UNW_Names<- sapply(UNW_names, function(x) paste0(str_split(x, "0_")[[1]][1], "0"))

UNW<- matrix(0, nrow=51, ncol=25)

for(i in 1:25){
  results<- read.csv(paste0(directory, "/", files[unweighted][i]))
  UNW[,i]<- t(results)
}

colnames(UNW)<- UNW_Names
row.names(UNW)<- Metrics

write.csv(UNW, paste0(directory, "/", prefix, "Results_366_days_unweighted.csv"))


# Get weighted

weighted<- !unweighted

W_names<- sapply(files[weighted], function(x) str_split(x, "-")[[1]][2])
W_Names<- sapply(W_names, function(x) paste0(str_split(x, "0_")[[1]][1], "0"))


W<- matrix(0, nrow=51, ncol=25)

for(i in 1:25){
  results<- read.csv(paste0(directory, "/", files[weighted][i]))
  W[,i]<- t(results)
}

colnames(W)<- W_Names
row.names(W)<- Metrics

write.csv(W, paste0(directory,"/", prefix, "Results_366_days_weighted.csv"))

#####################################################################
############ Putting everything together to prepare for Shiny:

pdw<- read.csv(paste0(directory, "/", prefix, "Results_366_days_weighted.csv"))
unw<- read.csv(paste0(directory, "/", prefix, "Results_366_days_unweighted.csv"))

names(pdw)[1]<- "X"
names(unw)[1]<- "X"

Metrics<- c(rep(c("MAE", "RMSE"),3),
            rep(c("Real class > shown class","Shown class > real class"),3),
            rep("AQI class off by >1",3),
            rep(c("95th percentile of errors",
                  "% of the |errors| > 10"),3),
            rep(c("Mean distance to NN monitor/sensor"),3),
            rep(c("Mean distance to NN monitor/sensor, among misclassifications > 1"),3),
            rep(c("Median distance to NN monitor/sensor"),3),
            rep(c("Median distance to NN monitor/sensor, among misclassifications > 1"),3),
            rep(c("Mean % of NNs that are LCSs"),3),
            rep(c("Mean % of NNs that are LCSs, among misclassifications > 1"),3),
            rep("Mean distance to NN monitor/sensor, among all misclassifications",3),
            rep("Median distance to NN monitor/sensor, among all misclassifications",3),
            rep("Mean % of NNs that are LCSs, among all misclassifications",3),
            rep("% Unhealthy and Showing Healthy",3))

pdw[,1]<- Metrics
unw[,1]<- Metrics

## Add in AQS-only results

aqs_w<- read.csv(paste0(directory, "/D366-AQS_sites.csv"))
aqs_unw<- read.csv(paste0(directory, "/D366-AQS_sites_unweighted.csv"))

pdw$AQS<- as.numeric(unlist(aqs_w))
unw$AQS<- as.numeric(unlist(aqs_unw))

pdw$AQS[c(34:39,46:48)]<- 0 # no LCSs for AQS only -- just showing collocations
unw$AQS[c(34:39,46:48)]<- 0 # no LCSs for AQS only -- just showing collocations

## Calculate % Unhealthy Showing Healthy / % Unhealthy

unhealthy_w<- c(0.001863175, 0.00246099, 0.004137348)
unhealthy_unw<- c(0.000612631, 0.00166163, 0.001190286)

Names<- names(pdw)
names(pdw)<- NULL
names(unw)<- NULL

pdw<- rbind(pdw, 0,0,0)
pdw[52:54,1]<- "% Showing Healthy, out of Unhealthy"
pdw[52,2:27]<- as.numeric(pdw[49,2:27])/unhealthy_w[1]
pdw[53,2:27]<- as.numeric(pdw[50,2:27])/unhealthy_w[2]
pdw[54,2:27]<- as.numeric(pdw[51,2:27])/unhealthy_w[3]

unw<- rbind(unw, 0,0,0)
unw[52:54,1]<- "% Showing Healthy, out of Unhealthy"
unw[52,2:27]<- as.numeric(unw[49,2:27])/unhealthy_unw[1]
unw[53,2:27]<- as.numeric(unw[50,2:27])/unhealthy_unw[2]
unw[54,2:27]<- as.numeric(unw[51,2:27])/unhealthy_unw[3]

names(pdw)<- Names
names(unw)<- Names

write.csv(pdw, paste0(directory, "/", prefix, "LCS_final_results_12-23-21_weighted.csv"), row.names=FALSE)
write.csv(unw, paste0(directory, "/", prefix, "LCS_final_results_12-23-21_unweighted.csv"), row.names=FALSE)
                 
#################################

### All PA sites:
pdw<- read.csv(paste0(directory, "/", prefix, "All_PA_sites.csv"))
unw<- read.csv(paste0(directory, "/", prefix, "All_PA_sites_unweighted.csv"))

DF<- data.frame(Metrics, PDW=pdw, UNW=unw)
write.csv(DF, paste0(directory, "/", prefix, "All_PA_sites_results_12-23-21.csv"), row.names=FALSE)
