CA_Locs<- readRDS("Getting data/CA_locations.rds")

files<- list.files("D:/Grad Research/")

for(f in files){
  date<- strsplit(f, "_")[[1]][4]
  data<- readRDS(paste("D:/Grad Research", f, sep="/"))
  CA_PM<- data[CA_Locs$ID]
  saveRDS(CA_PM, paste0("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi/QD_CA/CA_", date, ".rds"))
  print(date)
}
