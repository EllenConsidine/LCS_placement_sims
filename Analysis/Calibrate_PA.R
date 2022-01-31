### Preparing the PurpleAir (PA) data and comparing it with nearby reference measurements, to inform the simulation of LCS measurement error

library(dplyr)

setwd("/n/home13/econsidine") # FASRC directory

# ## Read in the two different PA channels (broken into two parts by idiosyncracy of the download process) 
#   ## and join them into one dataframe each:
# 
# A1<- read.csv("Getting data/PA_2020_NNs_channelA_1.csv") # "parent" channel
# A2<- read.csv("Getting data/PA_2020_NNs_channelA_2.csv") # "parent" channel
# B1<- read.csv("Getting data/PA_2020_NNs_channelB_1.csv") # "child" channel
# B2<- read.csv("Getting data/PA_2020_NNs_channelB_2.csv") # "child" channel
# 
# A<- rbind(A1, A2)
# rm("A1", "A2")
# B<- rbind(B1[,names(B2)], B2)
# rm("B1", "B2")
# 
# ## Obtain date from the timestamp:
# 
# A$Date<- sapply(A$created_at, function(x) strsplit(x, " ")[[1]][1])
# B$Date<- sapply(B$created_at, function(x) strsplit(x, " ")[[1]][1])
# 
# saveRDS(A, "Getting data/Channel-A_in-process.rds")
# saveRDS(B, "Getting data/Channel-B_in-process.rds")
# 
# ## Simplify the names of the fields we need:
# 
# names(A)[which(names(A) == "PM2.5..CF.1..ug.m3")]<- "PA_PM25"
# names(A)[which(names(A) == "Temperature_F")]<- "Temp"
# names(A)[which(names(A) == "Humidity_.")]<- "RH"
# 
# names(B)[which(names(B) == "PM2.5..CF.1..ug.m3")]<- "PA_PM25"
# # names(B)[which(names(B) == "Temperature_F")]<- "Temp"
# # names(B)[which(names(B) == "Humidity_.")]<- "RH"
# 
# ## Aggregate into daily measurements, keeping track of how many observations were available:
# 
# A$n_obs<- 1
# B$n_obs<- 1
# 
# A_count<- aggregate(n_obs ~ Date + ID, A[,c("n_obs", "Date", "ID")], sum)
# B_count<- aggregate(n_obs ~ Date + ID, B[,c("n_obs", "Date", "ID")], sum)
# 
# A_daily<- aggregate(. ~ Date + ID, A[,c("PA_PM25", "Temp", "RH",
#                                           "Date", "ID")], mean)
# B_daily<- aggregate(. ~ Date + ID, B[,c("PA_PM25",
#                                           "Date", "ID")], mean)
# 
# ## Remove the daily observations with less than 90% available:
# 
# A_joined<- inner_join(A_daily, A_count, by=c("Date", "ID"))
# A_90<- A_joined[which(A_joined$n_obs >= floor(0.9*720)),] # removed 372 obs
# 
# B_joined<- inner_join(B_daily, B_count, by=c("Date", "ID"))
# B_90<- B_joined[which(B_joined$n_obs >= floor(0.9*720)),] # removed 317 obs
# 
# ## Join A and B channels:
# 
# both<- inner_join(A_90, B_90, by=c("Date", "ID"), suffix = c(".A", ".B"))
# 
# PA_daily<- both[which(both$Date >= "2020-01-01"),] # removed 219 obs
# # PA_daily<- distinct(PA_daily)
# saveRDS(PA_daily, "PA_process_both_channels.rds")
# 
# ## QA/QC on PurpleAir:
#   ## keep if they differ by less than 5ug/m3 or 61%
#   ## average channels A & B
# 
# PA_daily<- readRDS("PA_process_both_channels.rds")
# 
# abs_diff<- abs(PA_daily$PA_PM25.A - PA_daily$PA_PM25.B)
# 
# perc_diff<- 2*abs_diff/(PA_daily$PA_PM25.A + PA_daily$PA_PM25.B)
# 
# small_diff<- which((abs_diff < 5)|(perc_diff < 0.61))
# # head(PA_daily[-small_diff,])
# 
# PA_daily<- PA_daily[small_diff,]
# PA_daily$PA_PM25<- rowMeans(PA_daily[,c("PA_PM25.A", "PA_PM25.B")])
# 
# 
# #### Join with AQS data:
# 
# df<- read.csv("Getting data/NN_PA_AQS_50m-outdoor.csv")
# aqs<- read.csv("Intermediate data/CA_AQS_2020_75pct.csv")
# 
# AQS<- inner_join(aqs, df[,c("Lon", "Lat", "PA.ID", "Dist")], by=c("Lon", "Lat")) # 13473 obs
# Daily<- inner_join(AQS, PA_daily, by = c("Date", "PA.ID" = "ID")) # 9061 obs
# Daily<- distinct(Daily) # 7016 obs
# 
# write.csv(Daily, "Intermediate data/Daily_AQS_PA_2020_both-channels.csv", row.names=FALSE) # done locally, so path is different than FASRC


###### Calibration / error analysis part:

Daily<- read.csv("LCS_data/Daily_AQS_PA_2020_both-channels.csv")
outdoor<- read.csv("LCS_data/PA_outside.csv")$id # only use PA sensors that are located outdoors

Daily<- Daily[which(Daily$PA.ID %in% outdoor),] # 5759 obs
# length(unique(Daily$PA.ID)) # 22 unique sensor-monitor pairs
rm(outdoor)

high_pos<- which(Daily$PM2.5 > 112) # 48 obs
# summary(Daily[high_pos,"PM2.5"])

Daily<- Daily[-high_pos,] # 5711 obs

#### EPA calibration:
Daily$EPA_PA<- 0.524*Daily$PA_PM25 - 0.0862*Daily$RH + 5.75 # From Barkjohn, et al. 2021
epa_errors<- Daily$EPA_PA - Daily$PM2.5 

# ## EDA:
# 
# summary(epa_errors)
# # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# # -80.3569  -2.2502  -0.1092  -0.3086   1.5947  66.3495
# 
# cor(Daily$EPA_PA, Daily$PM2.5)^2 # R^2 = 0.809
# sqrt(mean(epa_errors^2)) # RMSE = 5.326
# # Daily$PM2.5[which(Daily$PM2.5 == 0)]<- 0.05
# frac<- epa_errors/Daily$PM2.5
# summary(frac[which(Daily$PM2.5 != 0)]) # 99% of obs. 
# 
# hist(epa_errors)
# plot(Daily$PM2.5, Daily$EPA_PA)
# abline(0,1)
# plot(log(Daily$PM2.5), epa_errors, xlab = "log(AQS PM2.5)",
#      ylab = "Residuals", main = "Calibrated PurpleAir - AQS Measurements")
# abline(0,0)
# # plot(Daily$PM2.5, epa_errors)
# # abline(0,0)
# plot(Daily$RH, epa_errors)
# abline(0,0)
# plot(Daily$Temp, epa_errors)
# abline(0,0)
# 
# ## My test model:
# my_cal<- lm(PM2.5 ~ PA_PM25 + RH, Daily)
# summary(my_cal)
# 
# summary(my_cal$residuals)
# # Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# # -49.9754  -2.2827  -0.6461   0.0000   1.7518  82.9241 
# 
# plot(log(Daily$PM2.5), my_cal$residuals, xlab = "log(AQS PM2.5)",
#      ylab = "Residuals", main = "CA-only calibration - AQS Measurements")
# abline(0,0)


####### Simulating wrt PM2.5:

### Empirical drawing:
pm_q<- quantile(Daily$PM2.5, seq(0.1, 0.9, 0.1))

PM_q<- function(x){
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

# rm("high_pos")
# rm("i")
# 
# my_nas0<- readRDS("CA_NA_pos.rds")
# Real<- readRDS("Daily_PM25_CA.rds")[rep(!my_nas0,366)]
# 
# # Parallelize:
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
# plan(multisession, workers = 8)
# 
# future_lapply(1:length(my_list), function(j){write_decile(j)})
# 
# plan(sequential)
# 
# Deciles<- readRDS("Analysis/Deciles/Real_deciles_1.rds")
# for(i in 2:9999){
#   d<- readRDS(paste0("Analysis/Deciles/Real_deciles_",i,".rds"))
#   Deciles<- append(Deciles, d)
#   print(i)
# }
# saveRDS(Deciles, "Analysis/Real_deciles_updated_1-24-2022.rds") # done locally, so path is different than FASRC


Deciles<- readRDS("LCS_data/Real_deciles_updated_1-24-2022.rds")

## Used to generate SI Table A.1:
# for(i in 1:10){
#   print(i)
#   print(summary(Real[which(Deciles == i)]))
# }





