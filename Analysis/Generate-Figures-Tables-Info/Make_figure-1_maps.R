library(ggplot2)
library(maps)
library(raster)
library(gridExtra)

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

CA_clean<- readRDS("CA_clean_projected.rds")
DF<- CA_clean

CES<- readRDS("CA_with_CES_projected.rds")

roads<- shapefile("Getting data/CA_hwys.shp")
roads_df<- fortify(roads)

NHNW_pos<- which((1-DF$nonHisp.white) > quantile(1-DF$nonHisp.white, 0.80)) # top quintile
poverty_pos<- which(DF$poverty > quantile(DF$poverty, 0.80)) # top quintile

CA<- map_data("state", "california")

## Use colorblind-friendly palette:
CBF<- palette.colors()
# PA = 8 (purple)
# Schools = 3 (skyblue)
# Roads = 6 (blue) -- actually use 9 (gray) for the road map
# CES score = 7 (vermillion)
# Pollution score = 2 (orange)
# AQS = 1 (black)

p1<- ggplot() + geom_polygon(data=CA, aes(x=long, y=lat), 
                                  color="darkgray", fill = "white") + 
  geom_point(data = DF[which(DF$PA_site == 1),], aes(x=Lon, y=Lat),
             color = CBF[8], size = 0.75) +
  geom_point(data = DF[which(DF$AQS_site == 1),], 
             aes(x=Lon, y=Lat), color = CBF[1], shape = 17, size = 1.5) + 
  ggtitle("AQS Monitors (triangles) and PurpleAir LCS (dots)") + theme_void()

p2<- ggplot() + geom_polygon(data=CA, aes(x=long, y=lat), 
                             color="darkgray", fill = "white") + 
  geom_path(data=roads_df, aes(x=long, y=lat, group=group), color = "navy") +
  geom_point(data = DF[which(DF$School == 1),], aes(x=Lon, y=Lat),
             color = CBF[3], size = 0.75) +
  ggtitle("Public Schools and Highways") + theme_void()

p3<- ggplot(data=CES, aes(x=Lon, y=Lat, color=CES_score)) + 
  geom_point(size = 0.5) + scale_color_gradient(low="white", high=CBF[7]) +
  ggtitle("CES Score") + labs(color = "CES Score") + theme_void()

p4<- ggplot(data=CES, aes(x=Lon, y=Lat, color=Pollution_score)) + 
  geom_point(size = 0.5) + scale_color_gradient(low="white", high=CBF[2]) + 
  ggtitle("Pollution Score") + labs(color = "Pollution Score") + theme_void()

png("Writing/Maps_CBF.png", width = 800, height = 800)
grid.arrange(p1, p2, p4, p3, nrow=2)
dev.off()

