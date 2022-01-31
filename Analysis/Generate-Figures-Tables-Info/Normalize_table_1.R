## Originally, Table 1 in the manuscript was not normalized...

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi/Analysis")

df<- read.csv("Summary_table_orig.csv")


cell_func<- function(x){
  m<- strsplit(x, " ")[[1]][1]
  s<- strsplit(x, "\\(")[[1]][2]
  s<- strsplit(s, "\\)")[[1]][1]
  return(as.numeric(c(m, s)))
}

col_func<- function(clm){
  test<- sapply(clm, cell_func)
  test[1,]<- test[1,] - test[1,1]
  test<- test / test[2,1]
  
  return(as.vector(apply(round(test,2), MARGIN=2, function(x) paste0(x[1], " (", x[2], ")"))))
}

norm.DF<- apply(df[,2:6], MARGIN=2, col_func)

Norm.DF<- data.frame(Locations=df[,1], norm.DF)
write.csv(Norm.DF, "Summary_table_normalized.csv")

