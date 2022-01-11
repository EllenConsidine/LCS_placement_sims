### Preparing the PurpleAir (PA) data and comparing it with nearby reference measurements, to inform the simulation of LCS measurement error

library(dplyr)

setwd("/n/home13/econsidine")

# df<- read.csv("Getting data/NN_PA_AQS.csv")
# aqs<- read.csv("Intermediate data/CA_AQS_2020.csv") # EPA summary file
# pa<- read.csv("Getting data/PA_2020_NNs.csv")
# names(pa)[which(names(pa) == "PM2.5..CF.1..ug.m3")]<- "PA_PM25"
# names(pa)[which(names(pa) == "Temperature_F")]<- "Temp"
# names(pa)[which(names(pa) == "Humidity_.")]<- "RH"
# 
# pa$Date<- sapply(pa$created_at, function(x) strsplit(x, " ")[[1]][1])
# pa$Hour<- sapply(pa$created_at, function(x) strsplit(strsplit(x, " ")[[1]][2],":")[[1]][1])
# 
# length(unique(pa$ID))
# 
# pa_hourly<- aggregate(. ~ Date + Hour + ID, pa[,c("PA_PM25", "Temp", "RH",
#                                                   "Date", "Hour", "ID")], mean)
# pa_daily<- aggregate(. ~ Date + ID, pa[,c("PA_PM25", "Temp", "RH",
#                                                       "Date", "ID")], mean)
# 
# PA_HD<- inner_join(pa_daily, pa_hourly, by = c("Date", "ID"), suffix = c(".d", ".h"))
# write.csv(PA_HD, "Intermediate data/PA_daily_vs_hourly.csv", row.names=FALSE)
# 
# AQS<- inner_join(aqs, df[,c("Lon", "Lat", "PA.ID", "Dist")], by=c("Lon", "Lat"))
# Daily<- inner_join(AQS, pa_daily, by = c("Date", "PA.ID" = "ID"))
# 
# write.csv(Daily, "Intermediate data/Daily_AQS_PA_2020.csv", row.names=FALSE)


# ## Analyzing daily vs hourly data:
# 
# PA_HD<- read.csv("Intermediate data/PA_daily_vs_hourly.csv")
# hi<- which(PA_HD$PA_PM25.d > 500 | PA_HD$PA_PM25.h > 500)
# PA_HD<- PA_HD[-hi,]
# 
# summary(PA_HD$PA_PM25.d)
# summary(PA_HD$PA_PM25.h)
# 
# SDs<- aggregate(PA_PM25.h ~ Date, PA_HD, sd)
# summary(SDs$PA_PM25.h)


## Calibration / error analysis part:
Daily<- read.csv("LCS_data/Daily_AQS_PA_2020.csv")
outdoor<- read.csv("LCS_data/PA_outside.csv")$id # only use PA sensors that are located outdoors

Daily<- Daily[which(Daily$PA.ID %in% outdoor),]
rm(outdoor)

## Do some QA/QC first:
high<- which(Daily$PA_PM25 > 500) # Five observations above 500
Daily<- Daily[-high,]


## EPA calibration:
Daily$EPA_PA<- 0.52*Daily$PA_PM25 - 0.085*Daily$RH + 5.71 # National calibration equation from EPA report
epa_errors<- Daily$EPA_PA - Daily$PM2.5 # R^2 = 0.897


# # EDA:
# hist(epa_errors)
# plot(Daily$PM2.5, Daily$EPA_PA)
# abline(0,1)
# plot(log(Daily$PM2.5), epa_errors)
# abline(0,0)
# # plot(Daily$PM2.5, epa_errors)
# # abline(0,0)
# plot(Daily$RH, epa_errors)
# abline(0,0)
# plot(Daily$Temp, epa_errors)
# abline(0,0)

## Empirical drawing of residuals, by decile of true PM2.5:
pm_q<- quantile(Daily$PM2.5, seq(0.1, 0.9, 0.1))

PM_q<- function(x){ # there's probably a better way to do this in R...
  if(x > pm_q[5]){
    if(x > pm_q[7]){
      if(x > pm_q[9]){
        return(10)
      }else if(x > pm_q[8]){
        return(9)
      }else{
        return(8)
      }
    }else if(x > pm_q[6]){
      return(7)
    }else{
      return(6)
    }
  }else if(x > pm_q[2]){
    if(x > pm_q[4]){
      return(5)
    }else if (x > pm_q[3]){
      return(4)
    }else{
      return(3)
    }
  }else{
    if(x > pm_q[1]){
      return(2)
    }else{
      return(1)
    }
  }
}

Daily$Decile<- sapply(Daily$PM2.5, PM_q)
Q_resids<- vector(mode = "list", length = 10)
for(i in 1:10){
  Q_resids[[i]]<- epa_errors[which(Daily$Decile == i)]
}

# # rm("high")
# # rm("i")
# # save.image("Analysis/Simulate_PA_ME.RData")
# 
# my_nas0<- readRDS("CA_NA_pos.rds")
# Real<- readRDS("Daily_PM25_CA.rds")[rep(!my_nas0,366)]
# 
# # Parallelize to calculate deciles for all PM2.5 observations in the study
# num<- 10000
# my_seq<- round(seq(1,length(Real), length.out=num))
# my_list<- c()
# for(i in 1:(num-1)){
#   my_list<- append(my_list, list(Real[my_seq[i]:(my_seq[i+1]-1)]))
# }
# 
# my_list[[num-1]]<- append(my_list[[num-1]], Real[length(Real)])
# 
# write_decile<- function(j){
#   if(!file.exists(paste0("Analysis/Deciles/Real_deciles_",j,".rds"))){
#     deciles<- sapply(my_list[[j]], PM_q)
#     saveRDS(deciles, paste0("Analysis/Deciles/Real_deciles_",j,".rds"))
#   }
# }
# 
# library(parallel)
# library(future.apply)
# 
# options(future.globals.maxSize= 1500*1024^2)
# plan(multiprocess, workers = 8)
# 
# future_lapply(1:length(my_list), function(j){write_decile(j)})
# 
# plan(sequential)
# 
# # deciles<- sapply(Real, PM_q)
# # rm(list=setdiff(ls(), "deciles"))
# # save.image("Analysis/Real_deciles.RData")
# # load("Analysis/Real_deciles.RData")

# Deciles<- readRDS("Analysis/Deciles/Real_deciles_1.rds")
# for(i in 2:9999){
#   d<- readRDS(paste0("Analysis/Deciles/Real_deciles_",i,".rds"))
#   Deciles<- append(Deciles, d)
#   print(i)
# }
# saveRDS(Deciles, "Analysis/Real_deciles.rds") # file path is local, below is where this file is stored on the cluster


Deciles<- readRDS("LCS_data/Real_deciles.rds")


