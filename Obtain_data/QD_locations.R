setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi/Getting data")

library(sf)
library(sp)
# library(rgdal)
library(raster)
# # library(tigris)
# # state_shp<- states(year=2016) # 9.1 MB
# # CA<- state_shp[which(state_shp$STUSPS == "CA"),"geometry"]
# # write_sf(CA, "CA_boundary.shp")
# CA<- shapefile("CA_boundary.shp") # for plotting: just use "geometry"
# st_crs(CA) # NAD83
# 
# 
# grid_sites<- readRDS("USPredSite.rds")
# coordinates(grid_sites)<- c("Lon", "Lat")
# projection(grid_sites)<- projection(CA)
# coords<- coordinates(grid_sites)
# 
# CA_locs<- over(grid_sites, CA)
# CA_pos<- which(!is.na(CA_locs))
# plot(coords[CA_pos,"Lon"], coords[CA_pos,"Lat"])
# 
# CA_info<- data.frame(ID=CA_pos, Lon=coords[CA_pos,"Lon"], Lat=coords[CA_pos,"Lat"])
# saveRDS(CA_info, "CA_locations.rds")
CA_Locs<- readRDS("CA_locations.rds")

## In each file: the first 11196911 records are 1 km grid cells, the rest are monitoring sites
test<- readRDS("PredictionStep2_PM25_USGrid_20120820_20120820.rds")

CA<- data.frame(CA_Locs, PM25=test[CA_Locs$ID])

# library(ggplot2)
# 
# ggplot(CA, aes(x = Lon, y = Lat, colour = PM25)) +
#   geom_point()

