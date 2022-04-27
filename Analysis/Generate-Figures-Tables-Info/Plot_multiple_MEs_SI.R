library(ggplot2)
library(cowplot)

setwd("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi")

CBF<- palette.colors()

my_plot<- function(df, j, t, units = "", ylab = NULL, Ylim = NULL){
  df<- df[order(df$X),]
  k<- 3*(j-1)+1
  
  Ylab<- paste(df[k,1],units)
  if(!is.null(ylab)){
    Ylab<- ylab
  }
  
  DF<- data.frame(Num=c(0, 50, 100, 250, 500, 1000),
                  t(df[k,c(27, 5,2,4,6,3)]),
                  t(df[k+1, c(27, 5,2,4,6,3)]),
                  t(df[k+2, c(27, 5,2,4,6,3)]),
                  t(df[k, c(27, 10,7,9,11,8)]),
                  t(df[k+1, c(27, 10,7,9,11,8)]),
                  t(df[k+2, c(27, 10,7,9,11,8)]),
                  t(df[k, c(27, 15,12,14,16,13)]),
                  t(df[k+1, c(27, 15,12,14,16,13)]),
                  t(df[k+2, c(27, 15,12,14,16,13)]),
                  t(df[k, c(27, 20,17,19,21,18)]),
                  t(df[k+1, c(27, 20,17,19,21,18)]),
                  t(df[k+2, c(27, 20,17,19,21,18)]),
                  t(df[k, c(27, 25,22,24,26,23)]),
                  t(df[k+1, c(27, 25,22,24,26,23)]),
                  t(df[k+2, c(27, 25,22,24,26,23)]) )
  row.names(DF)<- NULL
  colnames(DF)<- c("Num", paste0("Y", 1:15))
  
  if(is.null(Ylim)){
    low<- min(df[k:(k+2),2:27])
    high<- max(df[k:(k+2),2:27])
  }else{
    low<- Ylim[1]
    high<- Ylim[2]
  }
  
  ggplot(DF, aes(Num, Y1)) + xlab("Number of LCS") + ylab(Ylab) +
    theme_bw() + ylim(c(low, high)) + 
    geom_line(aes(color = "Pollution Score  ", linetype = "Overall  "), lwd = 1) +
    geom_line(aes(Num, Y2, color = "Pollution Score  ", linetype = "High % Nonwhite  "), lwd = 1) +
    geom_line(aes(Num, Y3, color = "Pollution Score  ", linetype = "High % Poverty  "), lwd = 1) +
    geom_line(aes(Num, Y4, color = "CES Score  ", linetype = "Overall  "), lwd = 1) +
    geom_line(aes(Num, Y5, color = "CES Score  ", linetype = "High % Nonwhite  "), lwd = 1) +
    geom_line(aes(Num, Y6, color = "CES Score  ", linetype = "High % Poverty  "), lwd = 1) +
    geom_line(aes(Num, Y7, color = "PurpleAir  ", linetype = "Overall  "), lwd = 1) +
    geom_line(aes(Num, Y8, color = "PurpleAir  ", linetype = "High % Nonwhite  "), lwd = 1) +
    geom_line(aes(Num, Y9, color = "PurpleAir  ", linetype = "High % Poverty  "), lwd = 1) +
    geom_line(aes(Num, Y10, color = "Roads  ", linetype = "Overall  "), lwd = 1) +
    geom_line(aes(Num, Y11, color = "Roads  ", linetype = "High % Nonwhite  "), lwd = 1) +
    geom_line(aes(Num, Y12, color = "Roads  ", linetype = "High % Poverty  "), lwd = 1) +
    geom_line(aes(Num, Y13, color = "Schools  ", linetype = "Overall  "), lwd = 1) +
    geom_line(aes(Num, Y14, color = "Schools  ", linetype = "High % Nonwhite  "), lwd = 1) +
    geom_line(aes(Num, Y15, color = "Schools  ", linetype = "High % Poverty  "), lwd = 1) +
    ggtitle(t) + theme(legend.position = "none") +
    scale_color_manual(name="Placement Strategy:",
                       breaks=c("Pollution Score  ", "CES Score  ", "PurpleAir  ",
                                "Roads  ", "Schools  "),
                       values=c("Pollution Score  " = as.vector(CBF[2]), 
                                "CES Score  " = as.vector(CBF[7]), 
                                "PurpleAir  " = as.vector(CBF[8]),
                                "Roads  " = as.vector(CBF[6]), 
                                "Schools  " = as.vector(CBF[3])) ) +
    scale_linetype_manual(name="Neighborhoods:", breaks=c("Overall  ", "High % Nonwhite  ",
                                                          "High % Poverty  "), 
                          values=c("Overall  " = 1,
                                   "High % Nonwhite  " = 2, 
                                   "High % Poverty  " = 3))
  
}


### Weighted

## % R > S

rs1<- my_plot(df = read.csv("Analysis/Results/Sensitivity Analysis/LCS_final_results_SA_weighted.csv"),
              j = 16, t = "(a)    No Sensor ME: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.10))
rs2<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-010-LCS_final_results_12-23-21_weighted.csv"),
              j = 16, t = "(b)    10% Non-differential ME: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.10))
rs3<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-025-LCS_final_results_12-23-21_weighted.csv"),
              j = 16, t = "(c)    25% Non-differential ME: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.10))
rs4<- my_plot(df = read.csv("Analysis/Results/New results/SA-010-LCS_final_results_12-23-21_weighted.csv"),
              j = 16, t = "(d)    10% Differential ME: Real Class > Shown Class",
              ylab = "Fraction of Days", Ylim = c(0.02, 0.10))
rs5<- my_plot(df = read.csv("Analysis/Results/New results/SA-025-LCS_final_results_12-10-21_weighted.csv"),
              j = 16, t = "(e)    25% Differential ME: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.10))
rs6<- my_plot(df = read.csv("Analysis/Results/Final EPA results/EPA-resids-LCS_final_results_weighted.csv"),
              j = 16, t = "(f)    EPA Calibration Residuals: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.10))

RSrow<- plot_grid(rs1 + theme(legend.position="none"),
                  rs2 + theme(legend.position="none"),
                  rs3 + theme(legend.position="none"),
                  rs4 + theme(legend.position="none"),
                  rs5 + theme(legend.position="none"),
                  rs6 + theme(legend.position="none"),
                  nrow = 3)

png("Final Plots/Real-greater-than-Shown_weighted.png", width = 795, height = 645)

legend<- get_legend(rs1 + theme(legend.position = "bottom", legend.box = "vertical",
                                legend.key.width = unit(2, "line"),
                                legend.spacing.y = unit(0.1, "cm")))

plot_grid(RSrow, legend, ncol = 1, rel_heights = c(4, 0.5))

dev.off()


## % S > R

sr1<- my_plot(df = read.csv("Analysis/Results/Sensitivity Analysis/LCS_final_results_SA_weighted.csv"),
              j = 18, t = "(a)    No Sensor ME: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.28))
sr2<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-010-LCS_final_results_12-23-21_weighted.csv"),
              j = 18, t = "(b)    10% Non-differential ME: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.28))
sr3<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-025-LCS_final_results_12-23-21_weighted.csv"),
              j = 18, t = "(c)    25% Non-differential ME: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.28))
sr4<- my_plot(df = read.csv("Analysis/Results/New results/SA-010-LCS_final_results_12-23-21_weighted.csv"),
              j = 18, t = "(d)    10% Differential ME: Shown Class > Real Class",
              ylab = "Fraction of Days", Ylim = c(0.02, 0.28))
sr5<- my_plot(df = read.csv("Analysis/Results/New results/SA-025-LCS_final_results_12-10-21_weighted.csv"),
              j = 18, t = "(e)    25% Differential ME: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.28))
sr6<- my_plot(df = read.csv("Analysis/Results/Final EPA results/EPA-resids-LCS_final_results_weighted.csv"),
              j = 18, t = "(f)    EPA Calibration Residuals: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0.02, 0.28))

SRrow<- plot_grid(sr1 + theme(legend.position="none"),
                  sr2 + theme(legend.position="none"),
                  sr3 + theme(legend.position="none"),
                  sr4 + theme(legend.position="none"),
                  sr5 + theme(legend.position="none"),
                  sr6 + theme(legend.position="none"),
                  nrow = 3)

png("Final Plots/Shown-greater-than-Real_weighted.png", width = 795, height = 645)

legend<- get_legend(sr1 + theme(legend.position = "bottom", legend.box = "vertical",
                                legend.key.width = unit(2, "line"),
                                legend.spacing.y = unit(0.1, "cm")))

plot_grid(SRrow, legend, ncol = 1, rel_heights = c(4, 0.5))

dev.off()


################################## UNWEIGHTED by pop. density

### MAE and distance: five ME variations and distance to NN -- 3x2, equal sized
d<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-025-LCS_final_results_12-23-21_unweighted.csv"),
            j = 10, t = "(a)    Distance to Nearest Monitor or Sensor", ylab = "Avg. Distance (m)")
m1<- my_plot(df = read.csv("Analysis/Results/Sensitivity Analysis/LCS_final_results_SA_unweighted.csv"),
             j = 6, t = "(b)    No Sensor Measurement Error: MAE", 
             ylab = as.expression(bquote(~"MAE (" * mu * "g/"*m^3*")")))
m2<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-025-LCS_final_results_12-23-21_unweighted.csv"),
             j = 6, t = "(c)    25% Non-differential Measurement Error: MAE", 
             ylab = as.expression(bquote(~"MAE (" * mu * "g/"*m^3*")")))
m3<- my_plot(df = read.csv("Analysis/Results/New results/SA-010-LCS_final_results_12-23-21_unweighted.csv"),
             j = 6, t = "(d)    10% Differential Measurement Error: MAE", 
             ylab = as.expression(bquote(~"MAE (" * mu * "g/"*m^3*")")))
m4<- my_plot(df = read.csv("Analysis/Results/New results/SA-025-LCS_final_results_12-10-21_unweighted.csv"),
             j = 6, t = "(e)    25% Differential Measurement Error: MAE", 
             ylab = as.expression(bquote(~"MAE (" * mu * "g/"*m^3*")")))
m5<- my_plot(df = read.csv("Analysis/Results/Final EPA results/EPA-resids-LCS_final_results_unweighted.csv"),
             j = 6, t = "(f)    EPA Calibration Residual Draws: MAE", 
             ylab = as.expression(bquote(~"MAE (" * mu * "g/"*m^3*")")))

Mrow<- plot_grid(d + theme(legend.position="none"),
                 m1 + theme(legend.position="none"),
                 m2 + theme(legend.position="none"),
                 m3 + theme(legend.position="none"),
                 m4 + theme(legend.position="none"),
                 m5 + theme(legend.position="none"),
                 nrow = 3)

png("Final Plots/MAE-distance_unweighted.png", width = 795, height = 645)

legend<- get_legend(m1 + theme(legend.position = "bottom", legend.box = "vertical",
                               legend.key.width = unit(2, "line"),
                               legend.spacing.y = unit(0.1, "cm")))

plot_grid(Mrow, legend, ncol = 1, rel_heights = c(4, 0.5))

dev.off()

### UH Misclassifications: five ME variations -- no ME big across top, then 2x2 for rest

uh1<- my_plot(df = read.csv("Analysis/Results/Sensitivity Analysis/LCS_final_results_SA_unweighted.csv"),
              j = 2, t = "(a)    No Sensor Measurement Error: UHM", 
              ylab = "UH Misclassifications (%)")
uh2<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-010-LCS_final_results_12-23-21_unweighted.csv"),
              j = 2, t = "(b)    10% Non-differential Measurement Error: UHM", ylab = "UH Misclassifications (%)")
uh3<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-025-LCS_final_results_12-23-21_unweighted.csv"),
              j = 2, t = "(c)    25% Non-differential Measurement Error: UHM", ylab = "UH Misclassifications (%)")
uh4<- my_plot(df = read.csv("Analysis/Results/New results/SA-010-LCS_final_results_12-23-21_unweighted.csv"),
              j = 2, t = "(d)    10% Differential Measurement Error: UHM", ylab = "UH Misclassifications (%)")
uh5<- my_plot(df = read.csv("Analysis/Results/New results/SA-025-LCS_final_results_12-10-21_unweighted.csv"),
              j = 2, t = "(e)    25% Differential Measurement Error: UHM", ylab = "UH Misclassifications (%)")
uh6<- my_plot(df = read.csv("Analysis/Results/Final EPA results/EPA-resids-LCS_final_results_unweighted.csv"),
              j = 2, t = "(f)    EPA Calibration Residual Draws: UHM", ylab = "UH Misclassifications (%)")

Hrow<- plot_grid(uh1 + theme(legend.position="none"),
                 uh2 + theme(legend.position="none"),
                 uh3 + theme(legend.position="none"),
                 uh4 + theme(legend.position="none"),
                 uh5 + theme(legend.position="none"),
                 uh6 + theme(legend.position="none"),
                 nrow = 3)

png("Final Plots/UHM-all-6_unweighted.png", width = 795, height = 645)

legend<- get_legend(uh1 + theme(legend.position = "bottom", legend.box = "vertical",
                                legend.key.width = unit(2, "line"),
                                legend.spacing.y = unit(0.1, "cm")))

plot_grid(Hrow, legend, ncol = 1, rel_heights = c(4, 0.5))

dev.off()


## Distance to NN among misclass. > 1

dm1<- my_plot(df = read.csv("Analysis/Results/Sensitivity Analysis/LCS_final_results_SA_unweighted.csv"),
              j = 12, t = "(a)    No Sensor ME: Dist. Among Misclass. > 1",
              ylab = "Avg. Distance (m)")
dm2<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-010-LCS_final_results_12-23-21_unweighted.csv"),
              j = 12, t = "(b)    10% Non-differential ME: Dist. Among Misclass. > 1", 
              ylab = "Avg. Distance (m)")
dm3<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-025-LCS_final_results_12-23-21_unweighted.csv"),
              j = 12, t = "(c)    25% Non-differential ME: Dist. Among Misclass. > 1", 
              ylab = "Avg. Distance (m)")
dm4<- my_plot(df = read.csv("Analysis/Results/New results/SA-010-LCS_final_results_12-23-21_unweighted.csv"),
              j = 12, t = "(d)    10% Differential ME: Dist. Among Misclass. > 1", 
              ylab = "Avg. Distance (m)")
dm5<- my_plot(df = read.csv("Analysis/Results/New results/SA-025-LCS_final_results_12-10-21_unweighted.csv"),
              j = 12, t = "(e)    25% Differential ME: Dist. Among Misclass. > 1", 
              ylab = "Avg. Distance (m)")
dm6<- my_plot(df = read.csv("Analysis/Results/Final EPA results/EPA-resids-LCS_final_results_unweighted.csv"),
              j = 12, t = "(f)    EPA Calibration Residuals: Dist. Among Misclass. > 1", 
              ylab = "Avg. Distance (m)")

DMrow<- plot_grid(dm1 + theme(legend.position="none"),
                  dm2 + theme(legend.position="none"),
                  dm3 + theme(legend.position="none"),
                  dm4 + theme(legend.position="none"),
                  dm5 + theme(legend.position="none"),
                  dm6 + theme(legend.position="none"),
                  nrow = 3)

png("Final Plots/Dist-Large-Misclass_unweighted.png", width = 795, height = 645)

legend<- get_legend(dm1 + theme(legend.position = "bottom", legend.box = "vertical",
                                legend.key.width = unit(2, "line"),
                                legend.spacing.y = unit(0.1, "cm")))

plot_grid(DMrow, legend, ncol = 1, rel_heights = c(4, 0.5))

dev.off()


## % R > S

rs1<- my_plot(df = read.csv("Analysis/Results/Sensitivity Analysis/LCS_final_results_SA_unweighted.csv"),
              j = 16, t = "(a)    No Sensor ME: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.01, 0.06))
rs2<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-010-LCS_final_results_12-23-21_unweighted.csv"),
              j = 16, t = "(b)    10% Non-differential ME: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.01, 0.06))
rs3<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-025-LCS_final_results_12-23-21_unweighted.csv"),
              j = 16, t = "(c)    25% Non-differential ME: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.01, 0.06))
rs4<- my_plot(df = read.csv("Analysis/Results/New results/SA-010-LCS_final_results_12-23-21_unweighted.csv"),
              j = 16, t = "(d)    10% Differential ME: Real Class > Shown Class",
              ylab = "Fraction of Days", Ylim = c(0.01, 0.06))
rs5<- my_plot(df = read.csv("Analysis/Results/New results/SA-025-LCS_final_results_12-10-21_unweighted.csv"),
              j = 16, t = "(e)    25% Differential ME: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.01, 0.06))
rs6<- my_plot(df = read.csv("Analysis/Results/Final EPA results/EPA-resids-LCS_final_results_unweighted.csv"),
              j = 16, t = "(f)    EPA Calibration Residuals: Real Class > Shown Class", 
              ylab = "Fraction of Days", Ylim = c(0.01, 0.06))

RSrow<- plot_grid(rs1 + theme(legend.position="none"),
                  rs2 + theme(legend.position="none"),
                  rs3 + theme(legend.position="none"),
                  rs4 + theme(legend.position="none"),
                  rs5 + theme(legend.position="none"),
                  rs6 + theme(legend.position="none"),
                  nrow = 3)

png("Final Plots/Real-greater-than-Shown_unweighted.png", width = 795, height = 645)

legend<- get_legend(rs1 + theme(legend.position = "bottom", legend.box = "vertical",
                                legend.key.width = unit(2, "line"),
                                legend.spacing.y = unit(0.1, "cm")))

plot_grid(RSrow, legend, ncol = 1, rel_heights = c(4, 0.5))

dev.off()

## % S > R

sr1<- my_plot(df = read.csv("Analysis/Results/Sensitivity Analysis/LCS_final_results_SA_unweighted.csv"),
              j = 18, t = "(a)    No Sensor ME: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0, 0.15))
sr2<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-010-LCS_final_results_12-23-21_unweighted.csv"),
              j = 18, t = "(b)    10% Non-differential ME: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0, 0.15))
sr3<- my_plot(df = read.csv("Analysis/Results/Clsad results/SA-clsad-025-LCS_final_results_12-23-21_unweighted.csv"),
              j = 18, t = "(c)    25% Non-differential ME: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0, 0.15))
sr4<- my_plot(df = read.csv("Analysis/Results/New results/SA-010-LCS_final_results_12-23-21_unweighted.csv"),
              j = 18, t = "(d)    10% Differential ME: Shown Class > Real Class",
              ylab = "Fraction of Days", Ylim = c(0, 0.15))
sr5<- my_plot(df = read.csv("Analysis/Results/New results/SA-025-LCS_final_results_12-10-21_unweighted.csv"),
              j = 18, t = "(e)    25% Differential ME: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0, 0.15))
sr6<- my_plot(df = read.csv("Analysis/Results/Final EPA results/EPA-resids-LCS_final_results_unweighted.csv"),
              j = 18, t = "(f)    EPA Calibration Residuals: Shown Class > Real Class", 
              ylab = "Fraction of Days", Ylim = c(0, 0.15))

SRrow<- plot_grid(sr1 + theme(legend.position="none"),
                  sr2 + theme(legend.position="none"),
                  sr3 + theme(legend.position="none"),
                  sr4 + theme(legend.position="none"),
                  sr5 + theme(legend.position="none"),
                  sr6 + theme(legend.position="none"),
                  nrow = 3)

png("Final Plots/Shown-greater-than-Real_unweighted.png", width = 795, height = 645)

legend<- get_legend(sr1 + theme(legend.position = "bottom", legend.box = "vertical",
                                legend.key.width = unit(2, "line"),
                                legend.spacing.y = unit(0.1, "cm")))

plot_grid(SRrow, legend, ncol = 1, rel_heights = c(4, 0.5))

dev.off()
