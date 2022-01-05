# by xileioo

library(rstatix)
setwd("D:/study/R/R_PLOT_demo/R_PLOT_demo/1_boxplot")

pdata <- read.csv("1_2_boxplot_stat_input.csv",header = T)

p <- ggboxplot(
  pdata, x = "cluster", y = "exp", color = "group", 
  palette = "jco", facet.by = "protein",scales="free")

stat.test <- pdata %>%
  group_by(cluster, protein) %>%
  t_test(exp ~ group,ref.group = "Control") 
stat.test

stat.test <- stat.test %>%
  add_xy_position(x = "cluster", dodge = 0.8)

stat.test[stat.test$group2 == "GroupB",]$y.position <- stat.test[stat.test$group2 == "GroupB",]$y.position - 1
p + 
  stat_pvalue_manual(
    stat.test, label = "p.adj.signif", tip.length = 0.01
  ) 

ggsave("1_2_boxplot_stat_plot.png",width = 12,height = 8,device = "png")
