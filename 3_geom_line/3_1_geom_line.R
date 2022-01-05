library(ggplot2)
library(RColorBrewer)
#setwd("D:/study/R/R_PLOT_demo/R_PLOT_demo/3_geom_line")

pdata <- read.csv("3_1_geom_line_input.csv",header = T)
pdata$Sample <- factor(pdata$Sample, levels = c("WT","Mut"))
pd = position_dodge(0)

ggplot(pdata,aes(x=Sample,y=value,color=Group,shape=Group,group=Group)) +
  geom_hline(yintercept=5.5, linetype="dashed", color = "black") +
  geom_hline(yintercept=9.5, linetype="dashed", color = "black") +
  #geom_jitter()
  geom_line(position = pd) +
  geom_point(position = pd,size = 3) +
  #scale_color_brewer(palette="Dark2") +
  scale_shape_manual(values = c(15,19,17,0,2,0,2,0,5)) +
  scale_color_manual(values = c("#e31a1c","#d95f02","#1f78b4",
                                "#33a02c","#6a3d9a","#666666","#ff7f00")) +
  theme_bw() +
  xlab("") + ylab("Value") +
  theme(axis.title.x = element_text(color="black", size=12),
        axis.title.y = element_text(color="black", size=12),
        axis.text = element_text(color="black", size=12),
        panel.grid = element_blank()) +
  scale_y_continuous(labels=c("5" = ">1e-05", "6" = 10^-6,
                              "7" = 10^-7, "8" = 10^-8, "9" = 10^-9,
                              "10" = "<1e-10"))

ggsave("3_1_geom_line_plot.png",device = "png")