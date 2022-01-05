# by xileioo

library(ggplot2)
library(ggpubr)
#setwd("D:/study/R/R_PLOT_demo/R_PLOT_demo/1_boxplot")
mydata <- read.table("1_1_boxplot_stat_input.txt",header = T,sep = "\t",stringsAsFactors = F)
mydata <- na.omit(mydata)
mydata$Group <- factor(mydata$Group,levels = c("Control","A","B","C"))

## PLOT  
my_comparisons <- list(c("Control", "A"), c("Control", "B"), 
                        c("Control", "C"),c("B","C"))
ggplot(mydata, aes(Group,Value)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) +
  geom_boxplot(width = 0.6,outlier.shape = NA) +
  geom_jitter(aes(color=Group),width = 0.25,size=2.5,shape=19) +
  stat_compare_means(comparisons = my_comparisons,method="t.test",label.y =  c(45,47,49,51)) +
  #stat_compare_means(method = "anova") +
  #scale_fill_manual(values = c("#7FCBD7","#857EBB","#CA9DD7","#FACBD3")) +
  scale_color_manual(values = c("#737373","#4EBBD5","#01A087","#E64B35")) + 
  theme_bw()+
  scale_y_continuous(limits = c(15,53)) +
  theme(panel.grid = element_blank(),
        #legend.position = c(0.8,0.3),
        legend.title = element_blank(),
        axis.text=element_text(size=13, colour = "black"),
        axis.title=element_text(size=13),
        plot.title = element_text(size=10,hjust = 0.5),
        legend.text = element_text(size=10))  +
  labs(x = "", y = "Value",title = "")
#ggsave("1_1_boxplot_stat_plot.pdf")
ggsave("1_1_boxplot_stat_plot.png",device = "png")