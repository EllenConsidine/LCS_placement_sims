#### Used to generate SI Figure A.3

Daily<- read.csv("Intermediate data/Daily_AQS_PA_2020_both-channels.csv")
outdoor<- read.csv("Getting data/PA_outside.csv")$id

Daily<- Daily[which(Daily$PA.ID %in% outdoor),] # 5759 obs
# length(unique(Daily$PA.ID)) # 22 unique sensor-monitor pairs
rm(outdoor)

high_pos<- which(Daily$PM2.5 > 112) # 48 obs
# summary(Daily[high_pos,"PM2.5"])

Daily<- Daily[-high_pos,] # 5711 obs

# EPA calibration:
Daily$EPA_PA<- 0.524*Daily$PA_PM25 - 0.0862*Daily$RH + 5.75 # From Barkjohn et al. (2021)
epa_errors<- Daily$EPA_PA - Daily$PM2.5
# hist(epa_errors, main = "Distribution of Residuals from EPA Calibration")

## Sims:
set.seed(321)

MEDF<- data.frame(lPM=log(Daily$PM2.5), EPA=epa_errors, 
                  S01=sapply(Daily$PM2.5, function(x) rnorm(1,mean=0,sd=0.1*x)),
                  S025=sapply(Daily$PM2.5, function(x) rnorm(1,mean=0,sd=0.25*x)),
                  S05=sapply(Daily$PM2.5, function(x) rnorm(1,mean=0,sd=0.5*x)))


p1<- ggplot(MEDF, aes(lPM, EPA)) + xlab("log(PM2.5)") + ylab("Measurement Error") +
  ggtitle("Residuals from EPA Calibration") + theme_bw() + geom_point() + ylim(c(-55, 55)) +
  geom_hline(yintercept=0, linetype=2, color="gray")
p2<- ggplot(MEDF, aes(lPM, S01)) + xlab("log(PM2.5)") + ylab("Measurement Error") +
  ggtitle("Simulated 10% Differential ME") + theme_bw() + geom_point() + ylim(c(-55, 55)) +
  geom_hline(yintercept=0, linetype=2, color="gray")
p3<- ggplot(MEDF, aes(lPM, S025)) + xlab("log(PM2.5)") + ylab("Measurement Error") +
  ggtitle("Simulated 25% Differential ME") + theme_bw() + geom_point() + ylim(c(-55, 55)) +
  geom_hline(yintercept=0, linetype=2, color="gray")
p4<- ggplot(MEDF, aes(lPM, S05)) + xlab("log(PM2.5)") + ylab("Measurement Error") +
  ggtitle("Simulated 50% Differential ME") + theme_bw() + geom_point() + ylim(c(-55, 55)) +
  geom_hline(yintercept=0, linetype=2, color="gray")

png("Final Plots/ME_examples.png", width = 750, height = 350)

plot_grid(p1 + theme(legend.position="none"), 
          p2 + theme(legend.position="none"),
          p3 + theme(legend.position="none"),
          p4 + theme(legend.position="none"),
          ncol=2)

dev.off()


