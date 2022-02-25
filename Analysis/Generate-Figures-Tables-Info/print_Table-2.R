library(stringr)

setwd("")

my_print<- function(f, wtd = FALSE){ 
  res<- unlist(read.csv(f))
  
  MAE<- c(res[1], res[3], res[5])
  RS<- c(res[7], res[9], res[11])*100
  SR<- c(res[8], res[10], res[12])*100
  uh<- c(res[49], res[50], res[51])*100
  perc.95<- c(res[16], res[18], res[20])
  
  if(wtd){
    UH<- uh/c(0.001863175, 0.00246099, 0.004137348)
  }else{
    UH<- uh/c(0.000612631, 0.00166163, 0.001190286)
  }
  
  df<- round(data.frame(MAE, RS, SR, UH, perc.95),2)
  return(df)
}

## Apply function:

my_print("clsad-010-PA_N_4343_avg.csv", wtd = TRUE)
my_print("clsad-025-PA_N_4343_avg.csv", wtd = TRUE)
my_print("Diff-010-PA_N_4343_avg.csv", wtd = TRUE)
my_print("Diff-025-PA_N_4343_avg.csv", wtd = TRUE)
my_print("EPA-resids-PA_N_4343_avg.csv", wtd = TRUE)

my_print("clsad-010-PA_N_4343_unweighted_avg.csv")
my_print("clsad-025-PA_N_4343_unweighted_avg.csv")
my_print("Diff-010-PA_N_4343_unweighted_avg.csv")
my_print("Diff-025-PA_N_4343_unweighted_avg.csv")
my_print("EPA-resids-PA_N_4343_unweighted_avg.csv")
