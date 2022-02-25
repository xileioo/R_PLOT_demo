library(reshape2)
library(ggplot2)
mydata <- read.table("2_2_loess_input.txt",sep = "\t",header = T)
head(mydata)
mydata2 <- melt(mydata, id = c("type","Location"))
mydata3 <- dcast(mydata2, Location+variable~type)
mydata3$Location <- factor(mydata3$Location,levels = c("Global","High-income Asia Pacific",
                                                       "High-income North America","Western Europe",
                                                       "Australasia","Andean Latin America","Tropical Latin America",
                                                       "Central Latin America","Southern Latin America","Caribbean",
                                                       "Central Europe","Eastern Europe","Central Asia","North Africa and Middle East",
                                                       "South Asia","Southeast Asia","East Asia","Oceania","Western Sub-Saharan Africa",
                                                       "Eastern Sub-Saharan Africa","Central Sub-Saharan Africa","Southern Sub-Saharan Africa"))
head(mydata3)
fitd <- loess(mydata3$Values ~ mydata3$X,span = 0.75)
my.count <- seq(0, 1, length.out =588)
predd <- predict(fitd, my.count) 
plot(mydata3$Values ~ mydata3$X, pch=16)
lines(my.count, predd,col = "black")
#lines(predd$fit, lty="solid", col="darkred", lwd=3)
pdata <- data.frame(x = my.count, y = predd)

myshape <- c(0,0,15,5,6,0,15,5,6,2,0,15,5,0,0,0,15,5,0,15,5,6)
mycolor <- c("black",rep("#c42327",4),rep("#EB9124",5),rep("#60ab2d",3),rep("#2b563b",1),rep("#2a3267",1),rep("#59367d",3),rep("#6cbfcc",4))

ggplot(mydata3,aes(x=X, y=Values)) +
  geom_point(aes(color = Location, shape = Location),size = 3) +
  scale_shape_manual(values = myshape) +
  scale_color_manual(values = mycolor) +
  scale_x_continuous(breaks=seq(0,1,0.1),limits = c(0.3, 0.9)) +
  scale_y_continuous(breaks=seq(0,260,50)) +
  geom_line(data=pdata, aes(x=x, y=y, group=1),size=1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        #axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"),
        legend.key.size=unit(0.8,'cm'),
        #legend.key.width=unit(5,'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.position = "none",
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  guides(shape = guide_legend(nrow = 22),size = F)

ggsave("2_2_loess_plot.png",device = "png",
       height = 8, width = 12)
