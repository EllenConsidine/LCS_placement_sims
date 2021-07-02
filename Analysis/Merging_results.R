library(stringr)

setwd("/n/home13/econsidine")

Metrics<- c(rep(c("MAE", "RMSE"),3),
            rep(c("Real class > shown class","Shown class > real class",
                  "AQI class off by >1"),3),
            rep(c("95th percentile of errors",
                  "% of the |errors| > 10"),3) )
prefix<- "D366-"
suffix<- "_avg.csv"

pre_files<- list.files("LCS_results",
           prefix)
suff_files<- list.files("LCS_results",
                        suffix)
files<- intersect(pre_files, suff_files)

Names<- sapply(files, function(x) str_split(x, "-")[[1]][2])
Names<- sapply(names, function(x) str_split(x, "0_")[[1]][1])
Names<- sapply(Names, function(x) paste0(x,"0"))

# Get unweighted

unweighted<- str_detect(files, "unw")

UNW<- matrix(0, nrow=21, ncol=25)

for(i in 1:25){
  results<- read.csv(files[unweighted][i])
  UNW[,i]<- t(results)
}

colnames(UNW)<- Names[unweighted]
row.names(UNW)<- Metrics

write.csv(UNW, "LCS_results/Results_366_days_unweighted.csv")


# Get weighted

weighted<- !unweighted

W<- matrix(0, nrow=21, ncol=25)

for(i in 1:25){
  results<- read.csv(files[weighted][i])
  W[,i]<- t(results)
}

colnames(W)<- Names[weighted]
row.names(W)<- Metrics

write.csv(W, "LCS_results/Results_366_days_weighted.csv")


