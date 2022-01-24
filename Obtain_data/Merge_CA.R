###### Merging static information from different sources, to inform the simulations

library(FNN) # has get.kknx function
library(raster)
library(rgdal)
library(rgeos)
library(sp)

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

## PM25 grid locations:
CA_Locs<- readRDS("Getting data/CA_locations.rds")

## AQS locations: find nearest neighbor grid cell
AQS<- read.csv("Intermediate data/CA_AQS-locs_2016.csv")
knn.aqs<- get.knnx(CA_Locs[,c("Lon", "Lat")], AQS[,c("Lon", "Lat")], k=1)$nn.index
CA_Locs$AQS_site<- 0
CA_Locs$AQS_site[knn.aqs]<- 1

## Purple Air locations: find nearest neighbor grid cell
PA<- read.csv("Intermediate data/PA_CA_locations_outdoors_with_IDs.csv")
knn.pa<- get.knnx(CA_Locs[,c("Lon", "Lat")], PA[,c("Lon", "Lat")], k=1)$nn.index
CA_Locs$PA_site<- 0
CA_Locs$PA_site[knn.pa]<- 1

## School locations: find nearest neighbor grid cell
Schools<- shapefile("Intermediate data/CA_school_locs.shp")
school_coords<- data.frame(Schools[,c("LON1516", "LAT1516")])
knn.sch<- get.knnx(CA_Locs[,c("Lon", "Lat")], school_coords[,1:2], k=1)$nn.index
CA_Locs$School<- 0
CA_Locs$School[knn.sch]<- 1

## Census shapefile: overlay with grid points to extract variables
Census<- shapefile("Intermediate data/Census_vars.shp")

grid_sites<- CA_Locs[,c("Lon", "Lat")]
coordinates(grid_sites)<- c("Lon", "Lat")
projection(grid_sites)<- projection(Census)
coords<- coordinates(grid_sites)

grid_census<- over(grid_sites, Census)

## Check missingness:
my_nas<- apply(grid_census, MARGIN=1, function(x) sum(is.na(x))>3) #15538 = 0.032%
my_nas0<- apply(grid_census, MARGIN=1, function(x) sum(is.na(x))>0) #17784 = 0.036%

plot(CA_Locs$Lon[my_nas0==1], CA_Locs$Lat[my_nas0==1], cex=0.5) # missing mostly along coast --> underwater tracts / unpopulated areas

CA_stats<- cbind(CA_Locs, grid_census)
saveRDS(CA_stats, "Intermediate data/CA_info_without_hwys.rds")

## Highways shapefile: sum lengths within buffer(s) of each grid cell
CA_stats<- readRDS("Intermediate data/CA_info_without_hwys.rds")
hwys<- read.csv("Intermediate data/Hwy_lengths.csv") # calculated in Road_lengths.R

CA_all<- cbind(CA_stats, hwys)
saveRDS(CA_all, "CA_data.rds")

## Visualize:

library(ggplot2)

ggplot(CA_all[which(CA_all$Roads_50>0),], aes(x = Lon, y = Lat, colour = Roads_50)) +
  geom_point()

ggplot(CA_all[which(CA_all$Roads_100>0),], aes(x = Lon, y = Lat, colour = Roads_100)) +
  geom_point()

ggplot(CA_all[which(CA_all$Roads_250>0),], aes(x = Lon, y = Lat, colour = Roads_250)) +
  geom_point()

ggplot(CA_all[which(CA_all$Roads_500>0),], aes(x = Lon, y = Lat, colour = Roads_500)) +
  geom_point()


## Transform Lat-Lon to get distances in meters:

library(rgdal)
library(raster)

grid<- CA_clean[,c("Lon", "Lat")]
coordinates(grid)<- c("Lon", "Lat")
projection(grid)<- "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"

grid_df<- SpatialPointsDataFrame(grid, CA_clean[,c("Lon", "Lat")])
Grid<- spTransform(grid_df, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

CA_clean$Easting<- coordinates(Grid)[,1]
CA_clean$Northing<- coordinates(Grid)[,2]

saveRDS(CA_clean, "CA_clean_projected.rds")

## Adding in Cal Enviro Screen:

ces<- read.csv("Getting data/CES3_results.csv")
# library(ggplot2)
# windows()
# ggplot(ces, aes(x = Longitude, y = Latitude, colour = Pollution.Burden.Score)) +
#   geom_point(cex=1)
# ggplot(ces[order(ces$SB.535.Disadvantaged.Community),], aes(x = Longitude, y = Latitude, colour = SB.535.Disadvantaged.Community)) +
#   geom_point(cex=1)
# ggplot(ces, aes(x = Longitude, y = Latitude, colour = CES.3.0.Percentile)) +
#   geom_point(cex=1)

CES<- data.frame(GEOID_t=ces[,1], CES_score=ces$CES.3.0.Score,
                 CES_pct=ces$CES.3.0.Percentile, 
                 CES_disadvantaged=ces$SB.535.Disadvantaged.Community,
                 Pollution_score=ces$Pollution.Burden.Score,
                 Pollution_pct = ces$Pollution.Burden.Pctl)

dim(CES)[1] # 8035
length(which(!is.na(CES$CES_score))) # 7929
length(which(!is.na(CES$CES_disadvantaged))) # 8035
length(which(!is.na(CES$Pollution_score))) # 8035
length(which(!is.na(ces$Housing.Burden))) # 7878
length(which(!is.na(ces$Linguistic.Isolation))) # 7793

length(unique(CA_static$GEOID_t)) # 7284
length(unique(CA_clean$GEOID_t)) # 7191

missing_t<- which(!CES$GEOID_t %in% unique(CA_static$GEOID_t)) # 775

library(maps)
map("state", "CA")
points(ces[missing_t, "Longitude"], ces[missing_t, "Latitude"],
       pch=2, cex=0.5)

CES$GEOID_t<- as.character(CES$GEOID_t)
CA_ces<- left_join(CA_clean, CES, by = c("GEOID_t"))

ggplot(CA_ces, aes(x = Lon, y = Lat, colour = CES_pct)) +
  geom_point(cex=0.5)
ggplot(CA_ces, aes(x = Lon, y = Lat, colour = Pollution_score)) +
  geom_point(cex=0.5)
ggplot(CA_ces, aes(x = Lon, y = Lat, colour = CES_disadvantaged)) +
  geom_point(cex=0.5)

CA_clean<- CA_ces
saveRDS(CA_clean, "CA_with_CES_projected.rds") # Still has some NAs from the CES data


