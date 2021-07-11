library(stringr)

setwd("/n/home13/econsidine")

Metrics<- c(rep(c("MAE", "RMSE"),3),
            rep(c("Real class > shown class","Shown class > real class",
                  "AQI class off by >1"),3),
            rep(c("95th percentile of errors",
                  "% of the |errors| > 10"),3),
           rep(c("Mean distance to NN monitor/sensor"),3),
            rep(c("Mean distance to NN monitor/sensor, among misclassifications > 1"),3),
            rep(c("Median distance to NN monitor/sensor"),3),
            rep(c("Median distance to NN monitor/sensor, among misclassifications > 1"),3),
            rep(c("Mean % of NNs that are LCSs"),3),
            rep(c("Mean % of NNs that are LCSs, among misclassifications > 1"),3))
prefix<- "D366-"
suffix<- "_avg.csv"

pre_files<- list.files("LCS_results",
           prefix)
suff_files<- list.files("LCS_results",
                        suffix)
files<- intersect(pre_files, suff_files)

unweighted<- str_detect(files, "unw")

# Get unweighted

UNW_names<- sapply(files[unweighted], function(x) str_split(x, "-")[[1]][2])
UNW_Names<- sapply(UNW_names, function(x) paste0(str_split(x, "0_")[[1]][1], "0"))

UNW<- matrix(0, nrow=39, ncol=25)

for(i in 1:25){
  results<- read.csv(files[unweighted][i])
  UNW[,i]<- t(results)
}

colnames(UNW)<- UNW_Names
row.names(UNW)<- Metrics

write.csv(UNW, "LCS_results/Results_366_days_unweighted.csv")


# Get weighted

weighted<- !unweighted

W_names<- sapply(files[weighted], function(x) str_split(x, "-")[[1]][2])
W_Names<- sapply(W_names, function(x) paste0(str_split(x, "0_")[[1]][1], "0"))


W<- matrix(0, nrow=39, ncol=25)

for(i in 1:25){
  results<- read.csv(files[weighted][i])
  W[,i]<- t(results)
}

colnames(W)<- W_Names
row.names(W)<- Metrics

write.csv(W, "LCS_results/Results_366_days_weighted.csv")

