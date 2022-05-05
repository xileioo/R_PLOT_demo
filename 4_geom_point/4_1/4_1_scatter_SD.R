library(ggplot2)
library(plyr)
library(RColorBrewer)
library(ggrepel)
library(scales)


setwd("D:/study/R/R_PLOT_demo/R_PLOT_demo/4_geom_point/4_1")
mydata <- read.csv("4_1_input.csv",header = T)

# error bar calculate
errorbar_df_raw <- aggregate(mydata[,1:2], 
                             list(mydata$sample,mydata$group, mydata$day), 
                             function(x) c(mean = mean(x), sd = sd(x), sem = sd(x)/sqrt(length(x))))
errorbar_df <- data.frame(errorbar_df_raw[,1:3],
                          errorbar_df_raw$X,
                          errorbar_df_raw$Y)
colnames(errorbar_df) <- c("ID","sample","day",
                           "X_mean","X_sd","X_SEM",
                           "Y_mean","Y_sd","Y_SEM")

X_error = "X_sd"
Y_error = "Y_sd"

# PLOT
pdata <- errorbar_df

# set color and shape
mycolors = rep(brewer.pal(4,"Set1"),each=3)
myshape = rep(c(16,15,17),4)

# ggplot
ggplot() + 
  geom_errorbar(data = pdata, 
                aes(ymin=Y_mean-get(Y_error), ymax=Y_mean+get(Y_error), 
                    x=X_mean), 
                position=position_dodge(.5)) + 
  geom_errorbar(data = pdata,
                aes(xmin = X_mean-get(X_error),xmax = X_mean+get(X_error),
                    y = Y_mean),
                position=position_dodge(.5)) +
  geom_point(data = pdata, aes(X_mean, Y_mean,color = ID, shape=ID),
             size = 4) +
  scale_color_manual(values = mycolors) +
  scale_shape_manual(values = myshape) +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  scale_y_continuous(trans = log10_trans(),
                     #breaks = trans_breaks("log10", function(x) 10^x),
                     #labels = trans_format("log10", math_format(10^.x)),
                     limits = c(0.01,100),
                     breaks = c(0.01, 0.1, 1, 10,100),
                     labels= c(0.01, 0.1, 1, 10,100)) +
  scale_x_continuous(trans = log10_trans(),
                     #breaks = trans_breaks("log10", function(x) 10^x),
                     #labels = trans_format("log10", math_format(10^.x)),
                     limits = c(0.01,100),
                     breaks = c(0.01, 0.1, 1, 10,100),
                     labels= c(0.01, 0.1, 1, 10,100)) +
  #geom_text_repel(data = errorbar_df,aes(label = ID, x=X_mean, y=Y_mean)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8,0.3),
        legend.title = element_blank(),
        axis.text=element_text(size=15, colour = "black"),
        axis.title=element_text(size=15),
        plot.title = element_text(size=15,hjust = 0.5),
        legend.text = element_text(size=15))  +
  labs(x = "X", y = "Y",title = "")
  
ggsave(filename = "4_1_scatter_SD.pdf",height = 8,width = 8)
ggsave("4_1_scatter_SD.png",device = "png",height = 8,width = 8)

