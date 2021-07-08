library(shiny)
library(dplyr)
library(data.table)

setwd("/n/home13/econsidine")

pdw<- read.csv("LCS_results/Results_366_days_weighted.csv")
unw<- read.csv("LCS_results/Results_366_days_unweighted.csv")

W_aqs<- read.csv("LCS_results/D366-AQS_sites.csv")
UNW_aqs<- read.csv("LCS_results/D366-AQS_sites_unweighted.csv")
pdw$AQS<- sapply(W_aqs, as.numeric)
unw$AQS<- sapply(UNW_aqs, as.numeric)

# pdw[,2:27]<- apply(pdw[,2:27], MARGIN=2, as.numeric)
# unw[,2:27]<- apply(unw[,2:27], MARGIN=2, as.numeric)

ui <- fluidPage(
  
  selectInput("plotMetric",
              label = "Choose metric to plot: ",
              choices = c("MAE", "RMSE", "Real class > shown class",
                           "Shown class > real class", "AQI class off by >1",
                           "95th percentile of errors", "% of the |errors| > 10",
                         "Mean distance to NN monitor/sensor",
                         "Mean distance to NN monitor/sensor, among misclassifications > 1",
                         "Median distance to NN monitor/sensor",
                         "Median distance to NN monitor/sensor, among misclassifications > 1",
                         "Mean % of NNs that are LCSs",
                         "Mean % of NNs that are LCSs, among misclassifications > 1")),
  
  plotOutput("resultsWeighted"),
  plotOutput("resultsUnweighted"),
  
  selectInput("tabMetric",
              label = "Choose metric to show in tables: ",
              choices = c("MAE", "RMSE", "Real class > shown class",
                          "Shown class > real class", "AQI class off by >1",
                          "95th percentile of errors", "% of the |errors| > 10",
                         "Mean distance to NN monitor/sensor",
                         "Mean distance to NN monitor/sensor, among misclassifications > 1",
                         "Median distance to NN monitor/sensor",
                         "Median distance to NN monitor/sensor, among misclassifications > 1",
                         "Mean % of NNs that are LCSs",
                         "Mean % of NNs that are LCSs, among misclassifications > 1")),
  p("Weighted Results:"),
  tableOutput("tableWeighted"),
  p("Unweighted Results:"),
  tableOutput("tableUnweighted")
)

server <- function(input, output, session) {
  
  output$resultsWeighted<- renderPlot({
    pdw<- pdw[order(pdw$X),]
    k<- which(pdw$X == input$plotMetric)[1]
    plot(c(0, 50, 100, 250, 500, 1000), pdw[k,c(27, 5,2,4,6,3)],
         type = "l", col = "red", lty = 1, lwd = 2,
         ylim = c(min(pdw[k:(k+2),2:27]), 
                  max(pdw[k:(k+2),2:27])),
         xlab = "Number of low-cost sensors", ylab = input$plotMetric,
         main = "Weighted by Population Density")
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+1, c(27, 5,2,4,6,3)],
          col = "red", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+2, c(27, 5,2,4,6,3)],
          col = "red", lty = 3, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k, c(27, 10,7,9,11,8)],
          col = "blue", lty = 1, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+1, c(27, 10,7,9,11,8)],
          col = "blue", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+2, c(27, 10,7,9,11,8)],
          col = "blue", lty = 3, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k, c(27, 15,12,14,16,13)],
          col = "purple", lty = 1, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+1, c(27, 15,12,14,16,13)],
          col = "purple", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+2, c(27, 15,12,14,16,13)],
          col = "purple", lty = 3, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k, c(27, 20,17,19,21,18)],
          col = "green", lty = 1, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+1, c(27, 20,17,19,21,18)],
          col = "green", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+2, c(27, 20,17,19,21,18)],
          col = "green", lty = 3, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k, c(27, 25,22,24,26,23)],
          col = "orange", lty = 1, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+1, c(27, 25,22,24,26,23)],
          col = "orange", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), pdw[k+2, c(27, 25,22,24,26,23)],
          col = "orange", lty = 3, lwd = 2)


    legend("topright", legend=c("Pollution", "Pollution, nonwhite",
                                "Pollution, poverty", 
                                # "Pollution, SD",
                                "CES score", "CES score, nonwhite", 
                                "CES score, poverty",
                                # , "CES score, SD"
                                "Purple Air", "Purple Air, nonwhite", 
                                "Purple Air, poverty", 
                                # "Purple Air, SD",
                                "Roads", "Roads, nonwhite", "Roads, poverty",
                                # "Roads, SD",
                                "Schools", "Schools, nonwhite", 
                                "Schools, poverty" 
                                #, "Schools, SD"
                                ), 
           col=c(rep("red",3), rep("blue",3), rep("purple", 3), 
                 rep("green",3), rep("orange", 3)), 
           lty=rep(1:3,5), lwd = rep(2, 3*5))
  })
  
  output$resultsUnweighted<- renderPlot({
    unw<- unw[order(unw$X),]
    k<- which(unw$X == input$plotMetric)[1]
    plot(c(0, 50, 100, 250, 500, 1000), unw[k,c(27, 5,2,4,6,3)], 
         type = "l", col = "red", lty = 1, lwd = 2,
         ylim = c(min(unw[k:(k+2),2:27]), 
                  max(unw[k:(k+2),2:27])),
         xlab = "Number of low-cost sensors", ylab = input$plotMetric,
         main = "Unweighted by Population Density")
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+1, c(27, 5,2,4,6,3)],
          col = "red", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+2, c(27, 5,2,4,6,3)],
          col = "red", lty = 3, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k, c(27, 10,7,9,11,8)],
          col = "blue", lty = 1, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+1, c(27, 10,7,9,11,8)],
          col = "blue", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+2, c(27, 10,7,9,11,8)],
          col = "blue", lty = 3, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k, c(27, 15,12,14,16,13)],
          col = "purple", lty = 1, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+1, c(27, 15,12,14,16,13)],
          col = "purple", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+2, c(27, 15,12,14,16,13)],
          col = "purple", lty = 3, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k, c(27, 20,17,19,21,18)],
          col = "green", lty = 1, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+1, c(27, 20,17,19,21,18)],
          col = "green", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+2, c(27, 20,17,19,21,18)],
          col = "green", lty = 3, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k, c(27, 25,22,24,26,23)],
          col = "orange", lty = 1, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+1, c(27, 25,22,24,26,23)],
          col = "orange", lty = 2, lwd = 2)
    lines(c(0, 50, 100, 250, 500, 1000), unw[k+2, c(27, 25,22,24,26,23)],
          col = "orange", lty = 3, lwd = 2)


    legend("topright", legend=c("Pollution", "Pollution, nonwhite",
                                "Pollution, poverty", 
                                # "Pollution, SD",
                                "CES score", "CES score, nonwhite", 
                                "CES score, poverty",
                                # , "CES score, SD"
                                "Purple Air", "Purple Air, nonwhite", 
                                "Purple Air, poverty", 
                                # "Purple Air, SD",
                                "Roads", "Roads, nonwhite", "Roads, poverty",
                                # "Roads, SD",
                                "Schools", "Schools, nonwhite", 
                                "Schools, poverty" 
                                #, "Schools, SD"
    ), 
    col=c(rep("red",3), rep("blue",3), rep("purple", 3), 
          rep("green",3), rep("orange", 3)), 
    lty=rep(1:3,5), lwd = rep(2, 3*5))
  })
  
  output$tableWeighted<- renderTable({
    pdw<- pdw[order(pdw$X),]
    k<- which(pdw$X == input$tabMetric)
    pdw[c(k:(k+2)),]
    }, digits = 4)
  output$tableUnweighted<- renderTable({
    unw<- unw[order(unw$X),]
    k<- which(unw$X == input$tabMetric)
    unw[c(k:(k+2)),]
    }, digits = 4)
}

shinyApp(ui, server)
