library(raster)

schools<- shapefile("C:\\Users\\ellen\\OneDrive\\MyDocs\\Graduate Research\\Low-cost AQ sensor epi\\EDGE_GEOCODE_PUBLICSCH_1516\\EDGE_GEOCODE_PUBLICSCH_1516.shp")

CA<- schools[which(as.numeric(schools$STFIP15) == 6), c("CNTY15", "LZIP", "LCITY",
                                                        "LAT1516", "LON1516")]

plot(CA)

shapefile(CA, "C:\\Users\\ellen\\OneDrive\\MyDocs\\Graduate Research\\Low-cost AQ sensor epi\\CA_school_locs.shp")


