
setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi/Analysis/Results")

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

unhealthy_w<- c(0.001863175, 0.00246099, 0.004137348)
unhealthy_unw<- c(0.000612631, 0.00166163, 0.001190286)

make_table<- function(file_list, weighted=TRUE, name="All-PA_tab1.csv"){
  ## Column ordering: MAE, 95% errors, Underclass (R>S), Overclass(S>R), UHM
  ## Row ordering: [aqs, no-me, ndf-10, ndf-25, df-10, df-25, epa] for overall, then NW, then Pov
  
  overall_metrics<- c(1, 16, 7, 8, 49)
  NW_metrics<- c(3, 18, 9, 10, 50)
  Pov_metrics<- c(5, 20, 11, 12, 51)
  
  Overall<- matrix(0, nrow=7, ncol=5)
  NW<- matrix(0, nrow=7, ncol=5)
  Pov<- matrix(0, nrow=7, ncol=5)

  for(i in 1:6){
    df<- read.csv(file_list[i])
    Overall[i,]<- df[c(1, 16, 7, 8, 49),1]
    NW[i,]<- df[c(3, 18, 9, 10, 50),1]
    Pov[i,]<- df[c(5, 20, 11, 12, 51),1]
  }
  
  if(!weighted){
    Overall[,5]<- Overall[,5]/unhealthy_unw[1]
    NW[,5]<- NW[,5]/unhealthy_unw[2]
    Pov[,5]<- Pov[,5]/unhealthy_unw[3]
  }else{
    Overall[,5]<- Overall[,5]/unhealthy_w[1]
    NW[,5]<- NW[,5]/unhealthy_w[2]
    Pov[,5]<- Pov[,5]/unhealthy_w[3]
  }
  
  DF<- rbind(Overall, NW, Pov)
  DF[,3:5]<- 100*DF[,3:5]
  
  write.csv(DF, name, row.names=FALSE)
  
}

## All PurpleAir:

make_table(c("Revisions/No-ME_PurpleAir_4343_avg.csv",
             "Revisions/Ndf-10_PurpleAir_4343_avg.csv",
             "Revisions/Ndf-25_PurpleAir_4343_avg.csv",
             "Revisions/Df-10_PurpleAir_4343_avg.csv",
             "Revisions/DF-25_PurpleAir_4343_avg.csv",
             "Revisions/EPA-resids_PurpleAir_4343_avg.csv"), 
           name = "Revisions/All-PA_tab1.csv")

make_table(c("Revisions/No-ME_PurpleAir_4343_unweighted_avg.csv",
             "Revisions/Ndf-10_PurpleAir_4343_unweighted_avg.csv",
             "Revisions/Ndf-25_PurpleAir_4343_unweighted_avg.csv",
             "Revisions/Df-10_PurpleAir_4343_unweighted_avg.csv",
             "Revisions/DF-25_PurpleAir_4343_unweighted_avg.csv",
             "Revisions/EPA-resids_PurpleAir_4343_unweighted_avg.csv"), 
           name = "Revisions/All-PA_tab1_unweighted.csv", weighted=FALSE)


## Testing std deviation vs magnitude:

res<- read.csv("Revisions/EPA-resids_PurpleAir_4343.csv")
SDs<- apply(res, MARGIN=2, sd)
avgs<- apply(res, MARGIN=2, mean)
summary(SDs/avgs)

## All schools:

make_table(c("Revisions/No-ME_Schools_7548_avg.csv",
             "Revisions/Ndf-10_Schools_7548_avg.csv",
             "Revisions/Ndf-25_Schools_7548_avg.csv",
             "Revisions/Df-10_Schools_7548_avg.csv",
             "Revisions/DF-25_Schools_7548_avg.csv",
             "Revisions/EPA-resids_Schools_7548_avg.csv"), 
           name = "Revisions/All-schools_tab1.csv")

make_table(c("Revisions/No-ME_Schools_7548_unweighted_avg.csv",
             "Revisions/Ndf-10_Schools_7548_unweighted_avg.csv",
             "Revisions/Ndf-25_Schools_7548_unweighted_avg.csv",
             "Revisions/Df-10_Schools_7548_unweighted_avg.csv",
             "Revisions/DF-25_Schools_7548_unweighted_avg.csv",
             "Revisions/EPA-resids_Schools_7548_unweighted_avg.csv"), 
           name = "Revisions/All-schools_tab1_unweighted.csv", weighted=FALSE)


