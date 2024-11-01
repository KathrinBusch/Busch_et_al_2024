library(stringr)
library(ggplot2)
library(maps)

species <- c('Desmophyllum_dianthus', 'Lophelia_pertusa', 'seawater', 'Stryphnus_fortis', 'Vazella_pourtalesii', 'Weberella_bursa') # Modify manually host species names
mean_observed_asvs <- c('428', '149', '583', '480', '336', '161') #  Add manually mean number of observed ASVs per host species
d1  <- data.frame(species,mean_observed_asvs)
d1$mean_observed_asvs<-as.numeric(d1$mean_observed_asvs)
write.table(d1, file='C:/Users/Desktop/coral_sponges_amplicon_richness.txt', row.names=FALSE)
rm(list=ls(all=TRUE)) 

### Vazella_pourtalesii
aa<-0.02 # Manually put MSS_threshold for host species
d <- read.csv(file='C:/Users/Desktop/Vazella_pourtalesii_Present_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Vazella_pourtalesii_Present_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Vazella_pourtalesii_RCP85_2046_2065_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Vazella_pourtalesii_RCP85_2046_2065_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Vazella_pourtalesii_RCP85_2066_2085_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Vazella_pourtalesii_RCP85_2066_2085_threshold.csv', row.names=FALSE)
rm(list=ls(all=TRUE)) 

### Weberella_bursa
aa<-0.03 # Manually put MSS_threshold for host species
d <- read.csv(file='C:/Users/Desktop/Weberella_bursa_Present_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Weberella_bursa_Present_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Weberella_bursa_RCP85_2046_2065_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Weberella_bursa_RCP85_2046_2065_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Weberella_bursa_RCP85_2066_2085_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Weberella_bursa_RCP85_2066_2085_threshold.csv', row.names=FALSE)
rm(list=ls(all=TRUE)) 

### Stryphnus_fortis
aa<-0.02 # Manually put MSS_threshold for host species
d <- read.csv(file='C:/Users/Desktop/Stryphnus_fortis_Present_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Stryphnus_fortis_Present_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Stryphnus_fortis_RCP85_2046_2065_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Stryphnus_fortis_RCP85_2046_2065_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Stryphnus_fortis_RCP85_2066_2085_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Stryphnus_fortis_RCP85_2066_2085_threshold.csv', row.names=FALSE)
rm(list=ls(all=TRUE)) 

### Lophelia_pertusa
aa<-0.02 # Manually put MSS_threshold for host species
d <- read.csv(file='C:/Users/Desktop/Lophelia_pertusa_Present_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Lophelia_pertusa_Present_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Lophelia_pertusa_RCP85_2046_2065_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Lophelia_pertusa_RCP85_2046_2065_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Lophelia_pertusa_RCP85_2066_2085_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Lophelia_pertusa_RCP85_2066_2085_threshold.csv', row.names=FALSE)
rm(list=ls(all=TRUE)) 

### Desmophyllum_dianthus
aa<-0.01 # Manually put MSS_threshold for host species
d <- read.csv(file='C:/Users/Desktop/Desmophyllum_dianthus_Present_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Desmophyllum_dianthus_Present_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Desmophyllum_dianthus_RCP85_2046_2065_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Desmophyllum_dianthus_RCP85_2046_2065_threshold.csv', row.names=FALSE)
d <- read.csv(file='C:/Users/Desktop/Desmophyllum_dianthus_RCP85_2066_2085_MeanPresProbRaster_RF.csv', header=T, sep = ",")
d2<- d[which(d$z>=0), ]
d2$z[d2$z < aa] <- 0
write.csv(d2, file='C:/Users/Desktop/Desmophyllum_dianthus_RCP85_2066_2085_threshold.csv', row.names=FALSE)
rm(list=ls(all=TRUE)) 

d1 <- read.table(file='C:/Users/Desktop/coral_sponges_amplicon_richness.txt', header=T, sep = "")
desmo_pres <- read.csv(file='C:/Users/Desktop/Desmophyllum_dianthus_Present_threshold.csv', header=T, sep = ",")
desmo_fut <- read.csv(file='C:/Users/Desktop/Desmophyllum_dianthus_RCP85_2046_2065_threshold.csv', header=T, sep = ",")
desmo_fut2 <- read.csv(file='C:/Users/Desktop/Desmophyllum_dianthus_RCP85_2066_2085_threshold.csv', header=T, sep = ",")
loph_pres <- read.csv(file='C:/Users/Desktop/Lophelia_pertusa_Present_threshold.csv', header=T, sep = ",")
loph_fut <- read.csv(file='C:/Users/Desktop/Lophelia_pertusa_RCP85_2046_2065_threshold.csv', header=T, sep = ",")
loph_fut2 <- read.csv(file='C:/Users/Desktop/Lophelia_pertusa_RCP85_2066_2085_threshold.csv', header=T, sep = ",")
vaz_pres<- read.csv(file='C:/Users/Desktop/Vazella_pourtalesii_Present_threshold.csv', header=T, sep = ",")
vaz_fut<- read.csv(file='C:/Users/Desktop/Vazella_pourtalesii_RCP85_2046_2065_threshold.csv', header=T, sep = ",")
vaz_fut2<- read.csv(file='C:/Users/Desktop/Vazella_pourtalesii_RCP85_2066_2085_threshold.csv', header=T, sep = ",")
web_pres <- read.csv(file='C:/Users/Desktop/Weberella_bursa_Present_threshold.csv', header=T, sep = ",")
web_fut <- read.csv(file='C:/Users/Desktop/Weberella_bursa_RCP85_2046_2065_threshold.csv', header=T, sep = ",")
web_fut2 <- read.csv(file='C:/Users/Desktop/Weberella_bursa_RCP85_2066_2085_threshold.csv', header=T, sep = ",")
stryph_pres <- read.csv(file='C:/Users/Desktop/Stryphnus_fortis_Present_threshold.csv', header=T, sep = ",")
stryph_fut <- read.csv(file='C:/Users/Desktop/Stryphnus_fortis_RCP85_2046_2065_threshold.csv', header=T, sep = ",")
stryph_fut2 <- read.csv(file='C:/Users/Desktop/Stryphnus_fortis_RCP85_2066_2085_threshold.csv', header=T, sep = ",")
desmo_pres$x<-round(desmo_pres$x, digits=3)
desmo_pres$y<-round(desmo_pres$y, digits=3)
desmo_pres$comb<- paste(desmo_pres$x,desmo_pres$y, sep="_")
desmo_pres$desmo_pres<-desmo_pres$z
desmo_pres[,1:3]<-NULL
desmo_pres[desmo_pres > 0] <- 1
desmo_pres[desmo_pres == 1] <- 428 ##### Microbial richness Desmophyllum_dianthus from d1
desmo_fut$x<-round(desmo_fut$x, digits=3)
desmo_fut$y<-round(desmo_fut$y, digits=3)
desmo_fut$comb<- paste(desmo_fut$x,desmo_fut$y, sep="_")
desmo_fut$desmo_fut<-desmo_fut$z
desmo_fut[,1:3]<-NULL
desmo_fut[desmo_fut > 0] <- 1
desmo_fut[desmo_fut == 1] <- 428 ##### Microbial richness Desmophyllum_dianthus from d1
desmo_fut2$x<-round(desmo_fut2$x, digits=3)
desmo_fut2$y<-round(desmo_fut2$y, digits=3)
desmo_fut2$comb<- paste(desmo_fut2$x,desmo_fut2$y, sep="_")
desmo_fut2$desmo_fut2<-desmo_fut2$z
desmo_fut2[,1:3]<-NULL
desmo_fut2[desmo_fut2 > 0] <- 1
desmo_fut2[desmo_fut2 == 1] <- 428 ##### Microbial richness Desmophyllum_dianthus from d1
loph_pres$x<-round(loph_pres$x, digits=3)
loph_pres$y<-round(loph_pres$y, digits=3)
loph_pres$comb<- paste(loph_pres$x,loph_pres$y, sep="_")
loph_pres$loph_pres<-loph_pres$z
loph_pres[,1:3]<-NULL
loph_pres[loph_pres > 0] <- 1
loph_pres[loph_pres == 1] <- 149 ##### Microbial richness Lophelia_pertusa from d1
loph_fut$x<-round(loph_fut$x, digits=3)
loph_fut$y<-round(loph_fut$y, digits=3)
loph_fut$comb<- paste(loph_fut$x,loph_fut$y, sep="_")
loph_fut$loph_fut<-loph_fut$z
loph_fut[,1:3]<-NULL
loph_fut[loph_fut > 0] <- 1
loph_fut[loph_fut == 1] <- 149 ##### Microbial richness Lophelia_pertusa from d1
loph_fut2$x<-round(loph_fut2$x, digits=3)
loph_fut2$y<-round(loph_fut2$y, digits=3)
loph_fut2$comb<- paste(loph_fut2$x,loph_fut2$y, sep="_")
loph_fut2$loph_fut2<-loph_fut2$z
loph_fut2[,1:3]<-NULL
loph_fut2[loph_fut2 > 0] <- 1
loph_fut2[loph_fut2 == 1] <- 149 ##### Microbial richness Lophelia_pertusa from d1
vaz_pres$x<-round(vaz_pres$x, digits=3)
vaz_pres$y<-round(vaz_pres$y, digits=3)
vaz_pres$comb<- paste(vaz_pres$x,vaz_pres$y, sep="_")
vaz_pres$vaz_pres<-vaz_pres$z
vaz_pres[,1:3]<-NULL
vaz_pres[vaz_pres > 0] <- 1
vaz_pres[vaz_pres == 1] <- 336 ##### Microbial richness Vazella_pourtalesii from d1
vaz_fut$x<-round(vaz_fut$x, digits=3)
vaz_fut$y<-round(vaz_fut$y, digits=3)
vaz_fut$comb<- paste(vaz_fut$x,vaz_fut$y, sep="_")
vaz_fut$vaz_fut<-vaz_fut$z
vaz_fut[,1:3]<-NULL
vaz_fut[vaz_fut > 0] <- 1
vaz_fut[vaz_fut == 1] <- 336 ##### Microbial richness Vazella_pourtalesii from d1
vaz_fut2$x<-round(vaz_fut2$x, digits=3)
vaz_fut2$y<-round(vaz_fut2$y, digits=3)
vaz_fut2$comb<- paste(vaz_fut2$x,vaz_fut2$y, sep="_")
vaz_fut2$vaz_fut2<-vaz_fut2$z
vaz_fut2[,1:3]<-NULL
vaz_fut2[vaz_fut2 > 0] <- 1
vaz_fut2[vaz_fut2 == 1] <- 336 ##### Microbial richness Vazella_pourtalesii from d1
web_pres$x<-round(web_pres$x, digits=3)
web_pres$y<-round(web_pres$y, digits=3)
web_pres$comb<- paste(web_pres$x,web_pres$y, sep="_")
web_pres$web_pres<-web_pres$z
web_pres[,1:3]<-NULL
web_pres[web_pres > 0] <- 1
web_pres[web_pres == 1] <- 161 ##### Microbial richness Weberella_bursa from d1
web_fut$x<-round(web_fut$x, digits=3)
web_fut$y<-round(web_fut$y, digits=3)
web_fut$comb<- paste(web_fut$x,web_fut$y, sep="_")
web_fut$web_fut<-web_fut$z
web_fut[,1:3]<-NULL
web_fut[web_fut > 0] <- 1
web_fut[web_fut == 1] <- 161 ##### Microbial richness Weberella_bursa from d1
web_fut2$x<-round(web_fut2$x, digits=3)
web_fut2$y<-round(web_fut2$y, digits=3)
web_fut2$comb<- paste(web_fut2$x,web_fut2$y, sep="_")
web_fut2$web_fut2<-web_fut2$z
web_fut2[,1:3]<-NULL
web_fut2[web_fut2 > 0] <- 1
web_fut2[web_fut2 == 1] <- 161 ##### Microbial richness Weberella_bursa from d1
stryph_pres$x<-round(stryph_pres$x, digits=3)
stryph_pres$y<-round(stryph_pres$y, digits=3)
stryph_pres$comb<- paste(stryph_pres$x,stryph_pres$y, sep="_")
stryph_pres$stryph_pres<-stryph_pres$z
stryph_pres[,1:3]<-NULL
stryph_pres[stryph_pres > 0] <- 1
stryph_pres[stryph_pres == 1] <- 480 ##### Microbial richness Stryphnus_fortis from d1
stryph_fut$x<-round(stryph_fut$x, digits=3)
stryph_fut$y<-round(stryph_fut$y, digits=3)
stryph_fut$comb<- paste(stryph_fut$x,stryph_fut$y, sep="_")
stryph_fut$stryph_fut<-stryph_fut$z
stryph_fut[,1:3]<-NULL
stryph_fut[stryph_fut > 0] <- 1
stryph_fut[stryph_fut == 1] <- 480 ##### Microbial richness Stryphnus_fortis from d1
stryph_fut2$x<-round(stryph_fut2$x, digits=3)
stryph_fut2$y<-round(stryph_fut2$y, digits=3)
stryph_fut2$comb<- paste(stryph_fut2$x,stryph_fut2$y, sep="_")
stryph_fut2$stryph_fut2<-stryph_fut2$z
stryph_fut2[,1:3]<-NULL
stryph_fut2[stryph_fut2 > 0] <- 1
stryph_fut2[stryph_fut2 == 1] <- 480 ##### Microbial richness Stryphnus_fortis from d1
data<-merge(desmo_pres, desmo_fut, by="comb")
data<-merge(data, desmo_fut2, by="comb")
data<-merge(data, loph_pres, by="comb")
data<-merge(data, loph_fut, by="comb")
data<-merge(data, loph_fut2, by="comb")
data<-merge(data, vaz_pres, by="comb", all=T)
data<-merge(data, vaz_fut, by="comb", all=T)
data<-merge(data, vaz_fut2, by="comb", all=T)
data<-merge(data, web_pres, by="comb", all=T)
data<-merge(data, web_fut, by="comb", all=T)
data<-merge(data, web_fut2, by="comb", all=T)
data<-merge(data, stryph_pres, by="comb", all=T)
data<-merge(data, stryph_fut, by="comb", all=T)
data<-merge(data, stryph_fut2, by="comb", all=T)
pres<-data[c(1,2,5,8,11,14)]
pres$sum_pres<-rowSums(pres[,2:6])
fut<-data[c(1,3,6,9,12,15)]
fut$sum_fut<-rowSums(fut[,2:6])
fut2<-data[c(1,4,7,10,13,16)]
fut2$sum_fut2<-rowSums(fut2[,2:6])
fin<-merge(pres,fut, by="comb")
fin<-merge(fin,fut2, by="comb")
fin2<-fin[,1:2]
datnew<-str_split_fixed(fin2$comb, "_", 2)
d=as.data.frame(datnew)
names(d)[1]<-"x"
names(d)[2]<-"y"
d$comb<-paste(d$x,d$y, sep="_")
fin3<-merge(d, fin, by="comb")
write.table(fin3, file='C:/Users/Desktop/coral_sponges_amplicon_threshold.txt', row.names=FALSE)
rm(list=ls())

fin3<-read.table(file='C:/Users/Desktop/coral_sponges_amplicon_threshold.txt', header=T, sep = "")
pres<- fin3[which(fin3$sum_pres>=0), ]
pres$y<-as.numeric(pres$y)
pres$x<-as.numeric(pres$x)
fut<- fin3[which(fin3$sum_fut>=0), ]
fut$y<-as.numeric(fut$y)
fut$x<-as.numeric(fut$x)
fut2<- fin3[which(fin3$sum_fut2>=0), ]
fut2$y<-as.numeric(fut2$y)
fut2$x<-as.numeric(fut2$x)
world <- map_data('world')
p1 <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill="darkgrey", color=NA) +
  geom_tile(aes(x=x, y=y,fill= sum_pres,width=0.088, height=0.088), data=pres)+
  coord_quickmap() + 
  xlim(-82, -40)+
  ylim(25,72)+
  labs(x = "Longitude [°]", y="Latitude [°]", title="Present") +
  scale_fill_continuous(type = "viridis", name="Cumulative microbial richness\nin corals and sponges", limits = c(0, 1560))+
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
pdf(file="C:/Users/Desktop/richness_Present_threshold.pdf", height= 7, width =12)
p1
dev.off()
