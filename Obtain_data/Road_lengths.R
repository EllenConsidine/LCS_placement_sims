library(tigris)
library(dplyr)
library(sf)
library(rgdal)

# Main roads in CA: 
roads<- readOGR(dsn="D:\\CU Research\\ML PM25 project\\Data sets", layer="NHPNLine")

CA_roads<- roads[which(roads$STATE_CODE == 6),c("MILES", "KM", "FCLASS")]
writeOGR(CA_roads, dsn="C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi/Getting data",
         layer="CA_roads", driver="ESRI Shapefile")

# See FCLASS descriptions here: http://gisdata.lib.ncsu.edu/fedgov/bts/ntad01/polyline/nhpn/nhpn.htm
CA_hwys<- CA_roads[CA_roads$FCLASS %in% c(2, 6, 12, 14, 16),]
writeOGR(CA_hwys, dsn="C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi/Getting data",
         layer="CA_hwys", driver="ESRI Shapefile")

#### EXTRACT:

hwys<- readOGR(dsn="Getting data", layer="CA_hwys")
# windows()
# plot(hwys)
# dev.off()
Hwys<- spTransform(hwys, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
saveRDS(Hwys, "Getting data/Hwys_projected.rds")

CA_stats<- readRDS("Intermediate data/CA_info_without_hwys.rds")
grid<- CA_stats[,c("Lon", "Lat")]
coordinates(grid)<- c("Lon", "Lat")
projection(grid)<- projection(hwys)

grid_df<- SpatialPointsDataFrame(grid, CA_stats[,c("Lon", "Lat")])
Grid<- spTransform(grid_df, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#Make buffers 
#Note: this projection is in meters
Buffers<- c(50, 100, 250, 500)
cuts<- round(seq(1,dim(CA_stats)[1], length.out=50))

for(c in 2:50){
  
  for(b in 1:length(Buffers)){
    gbuffer<- gBuffer(Grid[cuts[c-1]:cuts[c],], width = Buffers[b], byid= TRUE)
    
    #Sum road lengths which intersect
    my_sum<- over(gbuffer, Hwys[,c("KM")], fn= sum)
    
    # road_df<- cbind(Grid, my_sum)
    # names(road_df)[3]<- paste0("Hwys_", Buffers[b], "m")
    file<- paste0("Intermediate data/Highway_files/Hwys_", Buffers[b],"_", c, ".csv")
    write.csv(my_sum, file)
    
    print(paste0("Buffer = ", Buffers[b], " : ", c, sep = ""))
  }
  
}

## Combine outputs:
Road_df<- matrix(0, nrow=dim(CA_stats)[1], ncol=length(Buffers))

for(b in 1:length(Buffers)){
  Roads<- c()
  for(c in 2:50){
    data<- read.csv(paste0("Intermediate data/Highway_files/Hwys_", Buffers[b],"_", c, ".csv"))
    if(c>2){
      data<- data[-dim(data)[1],]
    }
    Roads<- append(Roads, data[,"KM"])
  }
  # print(length(Roads))
  Road_df[,b]<- Roads
}
Road_df[is.na(Road_df)]<- 0
Road_DF<- data.frame(Road_df)
names(Road_DF)<- c(paste0("Roads_", Buffers))
apply(Road_DF, MARGIN=2, function(x) sum(x>0))

write.csv(Road_DF, "Intermediate data/Hwy_lengths.csv", row.names = FALSE)

