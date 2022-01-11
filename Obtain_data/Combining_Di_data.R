setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

folder<- "C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi/"
files<- list.files(paste0(folder, "QD_CA/"))

PM25<- c()

for(f in files){
  data<- readRDS(paste0(folder, "QD_CA/", f))
  PM25<- append(PM25, data)
  print(f)
}

saveRDS(PM25, "Daily_PM25_CA.rds", compress = TRUE)

s<- Sys.time()
PM25<- readRDS("Daily_PM25_CA.rds")
e<- Sys.time()

e-s #Only takes 12 s to read in
