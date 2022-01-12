library(dplyr)

## Getting locations of monitors:

aqs.1<- read.csv("Getting data/daily_88101_2020/daily_88101_2020.csv") # obtained final version on 1/11/22
aqs.2<- read.csv("Getting data/daily_88502_2020/daily_88502_2020.csv")

aqs_vars<- c("Longitude", "Latitude", "Date.Local", "Site.Num", "Parameter.Code", "Arithmetic.Mean", "AQI")
CA_aqs.1<- aqs.1[which(aqs.1$State.Code == 6), aqs_vars]
CA_aqs.2<- aqs.2[which(aqs.2$State.Code == 6), aqs_vars]

CA_aqs<- rbind(CA_aqs.1, CA_aqs.2)
names(CA_aqs)<- c("Lon", "Lat", "Date", "Site.ID", "Parameter", "PM2.5", "AQI")
CA_aqs$PM2.5[which(CA_aqs$PM2.5 < 0)]<- 0
# summary(CA_aqs$PM2.5)

CA_aqs.locs<- distinct(CA_aqs[,c("Lon", "Lat", "Site.ID", "Parameter")])
plot(CA_aqs.locs[,1], CA_aqs.locs[,2], col = CA_aqs.locs$Parameter, pch = 2, cex=0.5)

CA_avgs<- aggregate(PM2.5 ~ Lon + Lat + Date, CA_aqs, mean)

write.csv(CA_avgs, "Intermediate data/CA_AQS_2020.csv", row.names=FALSE)
write.csv(CA_aqs.locs, "Intermediate data/CA_AQS-locs_2020.csv", row.names=FALSE)
