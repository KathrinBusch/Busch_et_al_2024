library(ggplot2)
library(stringr)
library(data.table)
library(tidyr)
library(gridExtra)
setwd("C:/Users/Desktop")
d1 <- read.table(file='C:/Users/Desktop/modelling/inputs/Vazella_pourtalesii_Present_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d4 <- d1[ which(d1$z!='-3.4e+38'), ]
d4 <- d4[ which(d4$z!=0), ]
d4 <- d4[ which(d4$z>0.1), ] 
d5 <- read.table(file='C:/Users/Desktop/mendeley/Data2_SODA_and_BNAM.txt', header=T, sep = "")
d5<-d5[c("x","y","Present_BtmTmp", "Depth")]
d4$comb<-paste(round(d4$y,digits=3),round(d4$x, digits=3), sep="_")
d5$comb<-paste(round(d5$y,digits=3), round(d5$x, digits=3), sep="_")
d5$x<-NULL
d5$y<-NULL
d6<-merge(d4,d5, by="comb")
d6 <- d6[ which(d6$Present_BtmTmp>6.2), ]
d6 <- d6[ which(d6$Present_BtmTmp<8.2), ]
d8<-d6
d8 <- d8[ which(d8$x>(-70)), ]
d8 <- d8[ which(d8$x<(-58)), ]
d8 <- d8[ which(d8$y>(40)), ]
d8 <- d8[ which(d8$y<(50)), ]
d9<-d8[c("comb","x","y","Present_BtmTmp")]
write.table(d9, file='C:/Users/Desktop/Present_BtmTmp_subsetted.txt', row.names=FALSE)
rm(list=ls(all=TRUE)) 
d1 <- read.table(file='C:/Users/Desktop/Present_BtmTmp_subsetted.txt', header=T, sep = "")
d4 <- read.table(file='C:/Users/Desktop/plotting/spatial_predictions.txt', header=T, sep = "")
d5<-d4[c("x","y","clim","bcb979ea7d6db9f1e81929d5a490002c")]
d6<-d4[c("x","y","clim","X016474f092254f68cbfe8b808fbdebe8")]
d1$x<-NULL
d1$y<-NULL
d5$comb<-paste(round(d5$y,digits=3),round(d5$x, digits=3), sep="_")
d5$comb<-paste(round(d5$y,digits=3), round(d5$x, digits=3), sep="_")
d5fin<-merge(d5,d1, by="comb")
d5fin$test<-d5fin$clim-d5fin$Present_BtmTmp
d5fin$test2<-round(d5fin$test, digits=0)
unique(d5fin$test2)
d5fin$test<-NULL
d5fin$test2<-NULL
d5fin$clim<-NULL
write.table(d5fin, file='C:/Users/Desktop/bcb979ea7d6db9f1e81929d5a490002c_Present.txt', row.names=FALSE)
d1$x<-NULL
d1$y<-NULL
d6$comb<-paste(round(d6$y,digits=3),round(d6$x, digits=3), sep="_")
d6$comb<-paste(round(d6$y,digits=3), round(d6$x, digits=3), sep="_")
d6fin<-merge(d6,d1, by="comb")
d6fin$test<-d6fin$clim-d6fin$Present_BtmTmp
d6fin$test2<-round(d6fin$test, digits=0)
unique(d6fin$test2)
d6fin$test<-NULL
d6fin$test2<-NULL
d6fin$clim<-NULL
write.table(d6fin, file='C:/Users/Desktop/X016474f092254f68cbfe8b808fbdebe8_Present.txt', row.names=FALSE)
rm(list=ls(all=TRUE)) 
d1a <- read.table(file='C:/Users/Desktop/bcb979ea7d6db9f1e81929d5a490002c_Present.txt', header=T, sep = "")
d1b <- read.table(file='C:/Users/Desktop/X016474f092254f68cbfe8b808fbdebe8_Present.txt', header=T, sep = "")
aa<-c("bcb979ea7d6db9f1e81929d5a490002c") 
world <- map_data('world')
p1<-ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="darkgrey", color=NA) +
  geom_tile(aes(x=x, y=y,fill=bcb979ea7d6db9f1e81929d5a490002c ,width=0.2, height=0.2), data=d1a)+
  xlim(-70, -58)+
  ylim(40,50)+
  labs(x = "Longitude [°]", y="Latitude [°]", title="bcb979ea7d6db9f1e81929d5a490002c_Present") +
  scale_fill_gradient(low = "#003380", high = "#ccd6e5", name="Relative abundance [%]", limits = c(0, 37))+ 
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
p1
pdf(file="/Users/Desktop/map_ASV1_Present.pdf", height= 7, width =12)
p1
dev.off()
aa<-c("X016474f092254f68cbfe8b808fbdebe8") 
world <- map_data('world')
p2<-ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="darkgrey", color=NA) +
  geom_tile(aes(x=x, y=y,fill=X016474f092254f68cbfe8b808fbdebe8 ,width=0.2, height=0.2), data=d1b)+
  xlim(-70, -58)+
  ylim(40,50)+
  labs(x = "Longitude [°]", y="Latitude [°]", title="X016474f092254f68cbfe8b808fbdebe8_Present") +
  scale_fill_gradient(low = "#698b84", high = "#afe9dd", name="Relative abundance [%]",limits = c(0, 11))+ 
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
p2
pdf(file="/Users/Desktop/map_ASV2_Present.pdf", height= 7, width =12)
p2
dev.off()
rm(list = ls())
d1 <- read.table(file='C:/Users/Desktop/bcb979ea7d6db9f1e81929d5a490002c_Present.txt', header=T, sep = "")
d2 <- read.table(file='C:/Users/Desktop/X016474f092254f68cbfe8b808fbdebe8_Present.txt', header=T, sep = "")
d1$x<-NULL
d1$y<-NULL
d2$x<-NULL
d2$y<-NULL
d1$Present_BtmTmp<-NULL
d3<-merge(d1,d2)
d3$comb<-NULL
data_long <- gather(d3, condition, measurement, ((names(d3)[1]):(names(d3)[2])), factor_key=TRUE)
names(data_long)[1]<-"clim"
names(data_long)[2]<-"FeatureID"
names(data_long)[3]<-"relabund_goodASVs"
fin<-data_long
group1 <- fin[ which(fin$clim>=6.2 & fin$clim<6.3), ]
group2 <- fin[ which(fin$clim>=6.3 & fin$clim<6.4), ]
group3 <- fin[ which(fin$clim>=6.4 & fin$clim<6.5), ]
group4 <- fin[ which(fin$clim>=6.5 & fin$clim<6.6), ]
group5 <- fin[ which(fin$clim>=6.6 & fin$clim<6.7), ]
group6 <- fin[ which(fin$clim>=6.7 & fin$clim<6.8), ]
group7 <- fin[ which(fin$clim>=6.8 & fin$clim<6.9), ]
group8 <- fin[ which(fin$clim>=6.9 & fin$clim<7.0), ]
group9 <- fin[ which(fin$clim>=7.0 & fin$clim<7.1), ]
group10 <- fin[ which(fin$clim>=7.1 & fin$clim<7.2), ]
group11 <- fin[ which(fin$clim>=7.2 & fin$clim<7.3), ]
group12 <- fin[ which(fin$clim>=7.3 & fin$clim<7.4), ]
group13 <- fin[ which(fin$clim>=7.4 & fin$clim<7.5), ]
group14 <- fin[ which(fin$clim>=7.5 & fin$clim<7.6), ]
group15 <- fin[ which(fin$clim>=7.6 & fin$clim<7.7), ]
group16 <- fin[ which(fin$clim>=7.7 & fin$clim<7.8), ]
group17 <- fin[ which(fin$clim>=7.8 & fin$clim<7.9), ]
group18 <- fin[ which(fin$clim>=7.9 & fin$clim<8.0), ]
group19 <- fin[ which(fin$clim>=8.0 & fin$clim<8.1), ]
group20 <- fin[ which(fin$clim>=8.1 & fin$clim<8.2), ]
group1$group<-rep("group1", length(group1$clim))
group2$group<-rep("group2", length(group2$clim))
group3$group<-rep("group3", length(group3$clim))
group4$group<-rep("group4", length(group4$clim))
group5$group<-rep("group5", length(group5$clim))
group6$group<-rep("group6", length(group6$clim))
group7$group<-rep("group7", length(group7$clim))
group8$group<-rep("group8", length(group8$clim))
group9$group<-rep("group9", length(group9$clim))
group10$group<-rep("group10", length(group10$clim))
group11$group<-rep("group11", length(group11$clim))
group12$group<-rep("group12", length(group12$clim))
group13$group<-rep("group13", length(group13$clim))
group14$group<-rep("group14", length(group14$clim))
group15$group<-rep("group15", length(group15$clim))
group16$group<-rep("group16", length(group16$clim))
group17$group<-rep("group17", length(group17$clim))
group18$group<-rep("group18", length(group18$clim))
group19$group<-rep("group19", length(group19$clim))
group20$group<-rep("group20", length(group20$clim))
aa<-rbind(group1, group2)
aa<-rbind(aa, group3)
aa<-rbind(aa, group4)
aa<-rbind(aa, group5)
aa<-rbind(aa, group6)
aa<-rbind(aa, group7)
aa<-rbind(aa, group8)
aa<-rbind(aa, group9)
aa<-rbind(aa, group10)
aa<-rbind(aa, group11)
aa<-rbind(aa, group12)
aa<-rbind(aa, group13)
aa<-rbind(aa, group14)
aa<-rbind(aa, group15)
aa<-rbind(aa, group16)
aa<-rbind(aa, group17)
aa<-rbind(aa, group18)
aa<-rbind(aa, group19)
aa<-rbind(aa, group20)
aa$clim<-NULL
aa$comb<-NULL
aa$combo <- paste(aa$group, aa$FeatureID, sep="_")
aa$combo <- as.factor(aa$combo)
group_mean <- aggregate(aa$relabund_goodASVs, list(aa$combo), median)
names(group_mean)[1]<-"combo"
names(group_mean)[2]<-"meanrelabund"
datnew<-str_split_fixed(group_mean$combo, "_", 2)
d=as.data.frame(datnew)
nn<-cbind(d, group_mean)
names(nn)[1]<-"Category"
names(nn)[4]<-"Frequency"
nn$combo<-NULL
names(nn)[1]<-"clim"
names(nn)[2]<-"taxon"
names(nn)[3]<-"meanrelabund"
nn$clim<-gsub("\\bgroup1\\b", "6.2", nn$clim)
nn$clim<-gsub("\\bgroup2\\b", "6.3", nn$clim)
nn$clim<-gsub("\\bgroup3\\b", "6.4", nn$clim)
nn$clim<-gsub("\\bgroup4\\b", "6.5", nn$clim)
nn$clim<-gsub("\\bgroup5\\b", "6.6", nn$clim)
nn$clim<-gsub("\\bgroup6\\b", "6.7", nn$clim)
nn$clim<-gsub("\\bgroup7\\b", "6.8", nn$clim)
nn$clim<-gsub("\\bgroup8\\b", "6.9", nn$clim)
nn$clim<-gsub("\\bgroup9\\b", "7.0", nn$clim)
nn$clim<-gsub("\\bgroup10\\b", "7.1", nn$clim)
nn$clim<-gsub("\\bgroup11\\b", "7.2", nn$clim)
nn$clim<-gsub("\\bgroup12\\b", "7.3", nn$clim)
nn$clim<-gsub("\\bgroup13\\b", "7.4", nn$clim)
nn$clim<-gsub("\\bgroup14\\b", "7.5", nn$clim)
nn$clim<-gsub("\\bgroup15\\b", "7.6", nn$clim)
nn$clim<-gsub("\\bgroup16\\b", "7.7", nn$clim)
nn$clim<-gsub("\\bgroup17\\b", "7.8", nn$clim)
nn$clim<-gsub("\\bgroup18\\b", "7.9", nn$clim)
nn$clim<-gsub("\\bgroup19\\b", "8.0", nn$clim)
nn$clim<-gsub("\\bgroup20\\b", "8.1", nn$clim)
nn$clim<-as.numeric(nn$clim)
nn$taxon<-as.factor(nn$taxon)
nn2<-nn
nn2[is.na(nn2)] <- 0
names(nn2)[1]<-"clim_group"
names(nn2)[2]<-"FeatureID"
names(nn2)[3]<-"relabund_goodASVs"
nn2<- nn2[order(nn2$clim_group),] 
write.table(nn2, file='C:/Users/Desktop/subsetted_goodASVs_clim_group.txt', row.names=FALSE)
rm(list = ls())
d3 <- read.table(file='C:/Users/Desktop/subsetted_goodASVs_clim_group.txt', header=T, sep = "")
p3<-ggplot(data=d3, aes(x=clim_group, y=relabund_goodASVs, group=FeatureID, color=FeatureID)) +
  geom_point() +
  labs(x = "Temperature [°C]", y="Relative abundance [%]") +
  scale_color_manual(values=c("#003380","#afe9dd")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey", size = 1.5, linetype = "solid"),
        panel.background = element_rect(fill = "#ffffff",colour = NA),
        plot.background = element_rect(fill = '#ffffff'),
        axis.title.x = element_text(color="black", size=20, face="bold"),  
        axis.text.x = element_text(face="plain", color="black",size=18, angle=0),
        axis.title.y = element_text(color="black", size=20, face="bold"),  
        axis.text.y = element_text(face="plain", color="black",size=18, angle=0))
p3
pdf("C:/Users/Desktop/line_plot_two_taxa.pdf", height=5 , width =8)
p3
dev.off()
rm(list = ls())
