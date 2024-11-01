library(ggplot2)
library(maps)
library(gridExtra)

#HMA (run the same for LMA)
d <- read.csv(file='C:/Users/Desktop/hma_Present_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d$x<-round(d$x, digits=3)
d$y<-round(d$y, digits=3)
world <- map_data('world')
d2<- d[which(d$z>=0), ]
d2$groups<-round(d2$z/.05)*.05
d2$groups<-as.factor(d2$groups)
HMA_Present <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="darkgrey", color=NA) +
  geom_tile(aes(x=x, y=y,fill=groups,width=0.088, height=0.088), data=d2)+
  coord_quickmap() + 
  xlim(-47.5, -43)+
  ylim(45.5,49.5)+
  labs(x = "Longitude [°]", y="Latitude [°]", title="HMA_Present") +
  scale_fill_manual(breaks = levels(sort(d2$groups)),values = c ("#062b7b","#124780","#166789","#1c868d","#219990","#1aa87c","#12bb5d","#0dca35","#00dc00","#3de200","#7aed01","#baf502","#fcff00","#fbe206","#f5c90c","#efb310","#eea212","#e0831c","#d56e2d","#cb5f34","#c3503c"),name="Relative likelihood of occurrence") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey", size = 1.5, linetype = "solid"),
        panel.background = element_rect(fill = "#ffffff",colour = NA),
        plot.background = element_rect(fill = '#ffffff'),
        plot.title = element_text(color="black", size=16, face="bold"),
        legend.text=element_text(size=12),
        legend.background = element_rect(linetype = 'dotted', size = 1, colour = 1),
        axis.title.x = element_text(face="bold", color="black",size=16, angle=0),
        axis.text.x = element_text(color="black", size=14, face="plain",angle = 0),  
        axis.title.y = element_text(color="black", size=16, face="bold"),  
        axis.text.y = element_text(face="plain", color="black",size=14, angle=0))
HMA_Present 
HMA_Present <-HMA_Present + theme(legend.position="none")
