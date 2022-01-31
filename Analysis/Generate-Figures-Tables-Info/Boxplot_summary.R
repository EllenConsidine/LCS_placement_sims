#### Used to generate SI Figure A.2

library(ggplot2)
library(stringr)

box_data<- read.csv("C:/Users/ellen/OneDrive/MyDocs/Graduate Research/Low-cost AQ sensor epi/Plots/Table-1_summary-stats.csv")
names(box_data)[1]<- "Name"
names(box_data)[8]<- "Unhealthy"

CBF<- palette.colors()

p<- ggplot(box_data, aes(x = factor(Name, levels = Name))) + 
  geom_boxplot(aes(lower = Q1, upper = Q3, middle = Median,
                   ymin = Min., ymax = Max.), stat = "identity",
               fill = c(CBF[6], CBF[8], CBF[5], CBF[6], CBF[8], CBF[5]),
               alpha = c(0.6, 0.6, 0.6, 0.2, 0.2, 0.2)) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  xlab("") + ylab(as.expression(bquote(~PM[2.5] ~ "(" * mu * "g/"*m^3*")"))) + 
  labs(title = as.expression( bquote(~"Annual" ~ PM[2.5] ~ "Summaries and % Unhealthy Days")) ) +
  geom_text(data = box_data, aes(label = paste0(Unhealthy, "% unh."), y = Min. - 1), 
            position = position_dodge(width = .75), 
            show.legend = FALSE )
 

png("Final Plots/Boxplot_summaries.png", width = 550, height = 300)

p

dev.off()
