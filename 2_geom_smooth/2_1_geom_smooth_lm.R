### by xileioo

library(ggplot2)
library(scales)
#setwd("D:/study/R/R_PLOT_demo/R_PLOT_demo/2_geom_smooth")
mydata <- read.table("2_1_geom_smooth_lm_input.txt",sep = "\t",header = 1)
pdata <- mydata[,c("Time","Group","Percentage")]
pdata$Group <- factor(pdata$Group, levels = c("neg","pos"))
ggplot(pdata, 
       aes(x = Time, y = Percentage,color = Group)) +
  geom_point(aes(size = Percentage), alpha = 0.8) +
  scale_size(range = c(2, 4)) +
  scale_color_manual(values = c( "#034e7b","#e7298a")) +
  #geom_point(alpha = 0.65, aes(size = mpg)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
               se = F, size = 0.5, linetype = 2) +
  theme_bw() +
  scale_x_continuous(trans = log10_trans(),limits = c(1,10000000),
                     breaks = pdata$Time[1:8],
                     labels= pdata$Time[1:8]) +
  theme(panel.grid = element_blank(),
        #legend.position = c(0.8,0.3),
        legend.title = element_blank(),
        axis.text=element_text(size=8, colour = "black"),
        axis.title=element_text(size=8),
        plot.title = element_text(size=8,hjust = 0.5),
        legend.text = element_text(size=8))  +
  labs(x = "Time", y = "Percentage",title = "")
ggsave("2_1_geom_smooth_lm_plot.png",device = "png",width = 6,height = 3.5)
