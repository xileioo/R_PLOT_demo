# by xileioo
# 2023-04-24

library(rstatix)
library(tidyverse)
setwd("R_PLOT_demo/1_boxplot/1_3")

mydata <- read.csv("1_3_stripchart_stat_input.csv",header = T, row.names = 1)
pdata <- mydata %>%
  pivot_longer(!group, names_to = "genes", values_to = "Exp")

stat.test <- pdata %>%
  group_by(genes) %>%
  t_test(Exp ~ group)

stat.test <- stat.test %>%
  add_xy_position(x = "genes", dodge = 0.8)

ggplot(pdata, aes(x = genes, y = Exp)) +
  geom_jitter(
    aes(fill = group, color = group), shape = 1, size = 2.5,
    position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)) +
  stat_pvalue_manual(
    stat.test, 
    label = "p.adj.signif", 
    tip.length = 0.01,
    bracket.nudge.y = -2) + 
  stat_summary(
    aes(fill = group), fun.data="mean_sdl", fun.args = list(mult=1), 
    size = 0.4, position = position_dodge(0.8)) + 
  stat_summary(
    aes(fill = group), fun.y="mean", fun.args = list(mult=1), 
    geom = "point",
    size = 0.4, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a")) +
  guides(fill="none") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 13),
        panel.grid = element_blank(),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=12))

ggsave("1_3_stripchart_stat_plot.png",width = 9, height = 4.5, device = "png")
