## Recreate PM2.5 table
  ## https://forum.airnowtech.org/t/the-aqi-equation/169

Class<- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
          "Unhealthy", "Very Unhealthy", "Hazardous")
Conc_low<- c(0, 12.1, 35.5, 55.5, 150.5, 250.5)
Conc_high<- c(12, 35.4, 55.4, 150.4, 250.4, 500.4)
AQI_low<- c(0, 51, 101, 151, 201, 301)
AQI_high<- c(50, 100, 150, 200, 300, 500)

AQI_table<- data.frame(Class=1:6, Conc_low, Conc_high, AQI_low, AQI_high)

## Function
AQI_x<- function(x){ #takes in concentration of interest
  n_r<- which((AQI_table$Conc_low <= x)&(AQI_table$Conc_high > x))
  aqi<-(AQI_table$AQI_high[n_r]-AQI_table$AQI_low[n_r])/(AQI_table$Conc_high[n_r]-AQI_table$Conc_low[n_r])*(x-AQI_table$Conc_low[n_r])+AQI_table$AQI_low[n_r]
  return(round(aqi))
  # return(c(round(aqi), AQI_table$Class[n_r]))
}

AQI_class<- function(x){ #takes in concentration of interest
  n_r<- which((AQI_table$Conc_low <= x)&(AQI_table$Conc_high > x))
  aqi<-(AQI_table$AQI_high[n_r]-AQI_table$AQI_low[n_r])/(AQI_table$Conc_high[n_r]-AQI_table$Conc_low[n_r])*(x-AQI_table$Conc_low[n_r])+AQI_table$AQI_low[n_r]
  return(AQI_table$Class[n_r])
}

PM<- 0:500
AQI<- unlist(sapply(PM, AQI_x))

AQI_ref<- data.frame(PM=0:500, AQI=c(AQI, 500)) # gives one less row for some reason
AQI_ref$Class<- c(unlist(sapply(PM, AQI_class)), 6)

## Calculate AQI + classes for Daily_PM:

# Real_match<- match(round(Daily_PM), AQI_ref$PM)
# Real_aqi<- AQI_ref$AQI[Real_match]
# Real_class<- AQI_ref$Class[Real_match]
# saveRDS(Real_aqi, "Daily_PM_AQI.rds")
# saveRDS(Real_class, "Daily_PM_AQI-class.rds")

# Real_aqi<- readRDS("Daily_PM_AQI.rds")
Real_class<- readRDS("LCS_data/Daily_PM_AQI-class.rds")

# these<- seq(10, 200, 10)
# these_aqis<- sapply(these, AQI)
# 
# plot(these, these_aqis)
