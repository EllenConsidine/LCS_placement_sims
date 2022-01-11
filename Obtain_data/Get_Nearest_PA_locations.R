## Code adapted from Priyanka deSouza

## Updated: https://docs.google.com/document/d/15ijz94dXJ-YAZLi9iZ_RaBwrZ4KtYeCy08goGBwnbCU/edit#

library(RCurl)
library(jsonlite)
library(raster)

json_data <- jsonlite::fromJSON(getURL("https://www.purpleair.com/data.json")) #"https://api.purpleair.com/v1/sensors" -- need API key
purpleair<-as.data.frame(json_data$data)
names(purpleair)<- json_data$fields

coord<-purpleair[,c("ID", "Lat", "Lon")]

pa<-subset(coord, !is.na(coord$Lon))

#Keeping one of the 2 channels:
pa1<-pa[order(pa$Lat, pa$Lon),]
PA<-pa1[!duplicated(pa1$Lat, pa1$Lon),]
PA<- as.data.frame(apply(PA, MARGIN=2, as.numeric))
plot(PA[,"Lon"], PA[,"Lat"])
write.csv(PA, "PA_world2.csv")

# Subset to US:
PA_USA<- PA[which((PA$Lon <= -67)&(PA$Lon >= -125)&(PA$Lat <= 50)&(PA$Lat >= 25)),]
plot(PA_USA[,"Lon"], PA_USA[,"Lat"])
write.csv(PA_USA, "PA_USA2.csv")

## Subset to CA:
PA<- read.csv("PA_USA2.csv")

CA<- shapefile("Getting data/CA_boundary.shp")
coordinates(PA)<- c("Lon", "Lat")
projection(PA)<- projection(CA)
coords<- coordinates(PA)

CA_locs<- over(PA, CA)
CA_pos<- which(!is.na(CA_locs))
plot(coords[CA_pos,"Lon"], coords[CA_pos,"Lat"])

CA_info<- data.frame(PA[CA_pos,])[,2:4]
write.csv(CA_info, "Intermediate data/PA_CA_locations_with_IDs.csv", row.names=FALSE)

## Pick out only the outdoor ones:
CA<- read.csv("Intermediate data/PA_CA_locations_with_IDs.csv")
outdoor<- read.csv("Getting data/PA_outside.csv")$id # obtained in PA_historical_data.ipynb

CA_outdoor<- CA[which(CA$ID %in% outdoor),]
write.csv(CA_outdoor, "Intermediate data/PA_CA_locations_outdoors_with_IDs.csv", row.names=FALSE)

## Figure out which ones are closest to the AQS monitors: 
setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")
# CA_clean<- readRDS("CA_with_SDI_CES.rds")

library(FNN)

AQS<- read.csv("Intermediate data/CA_AQS-locs_2020.csv")
PA<- read.csv("Intermediate data/PA_CA_locations_with_IDs.csv")

knn<- get.knnx(PA[,c("Lon", "Lat")], AQS[,c("Lon", "Lat")], k=1)

library(fields)
Dist<- rdist.earth.vec(AQS[,c("Lon", "Lat")], PA[knn$nn.index,c("Lon", "Lat")], 
                       miles=FALSE)
sum(Dist < 1)/length(Dist) # 45% under 1km away

DF<- data.frame(AQS, PA.ID = PA$ID[knn$nn.index], Dist)

# df<- DF[which(DF$Dist <= 0.5),] # 66 obs
# df<- DF[which(DF$Dist <= 0.1),] # 50 obs
df<- DF[which(DF$Dist <= 0.05),] # 46 obs within 50 meters
summary(df$Dist)

write.csv(df, "Getting data/NN_PA_AQS_50m.csv", row.names=FALSE)

## Check that these ^^^ are all outdoor: 
NN<- read.csv("Getting data/NN_PA_AQS_50m.csv")

indoor<- which(! NN$PA.ID %in% outdoor)

NN<- NN[-indoor,]

write.csv(NN, "Getting data/NN_PA_AQS_50m-outdoor.csv", row.names=FALSE)
