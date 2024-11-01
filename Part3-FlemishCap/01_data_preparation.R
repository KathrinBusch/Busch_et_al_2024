library(sp)
library(raster)
##########################################################################################
### 1. Prepare environmental input data
##########################################################################################
d0 <- read.table(file='C:/Users/Desktop/mendeley/Data6_FlemishCap_HMA_LMA.txt', header=T, sep = "")
d0$Nutrient_cycling<-NULL
d0$Habitat_provision<-NULL
d0$HMA_presence<-NULL
d0$HMA_sum_biomass<-NULL
d0$LMA_presence<-NULL
d0$LMA_sum_biomass<-NULL
d0$comb<- paste(d0$y,d0$x, sep="_")
d0$dummy<-rep(1, length(d0$comb))
d0$x<-NULL
d0$y<-NULL
d1 <- read.table(file='C:/Users/Desktop/mendeley/Data2_SODA_and_BNAM.txt', header=T, sep = "")
d1$comb<- paste(d1$y,d1$x, sep="_")
d2<-merge(d0,d1, by="comb")
d2$comb<-NULL
d2$dummy<-NULL
nex<-d2[c("x","y","Depth", "Slope", "Present_BtmStress", "Present_BtmCur" ,"Present_MLD" ,"Present_BtmSal", "Present_SSS" ,"Present_SST" ,"Present_BtmTmp")]
d1<-d2[c("x","y","Depth", "Slope", "Present_BtmStress", "Present_BtmCur" ,"Present_MLD" ,"Present_BtmSal", "Present_SSS" ,"Present_SST" ,"Present_BtmTmp")]
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
writeRaster(xx, filename="C:/Users/Desktop/Depth.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
d1<-nex
d1[3]<-NULL
nex<-d1
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
writeRaster(xx, filename="C:/Users/Desktop/Slope.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
d1<-nex
d1[3]<-NULL
nex<-d1
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
aa
writeRaster(xx, filename="C:/Users/Desktop/Present_BtmStress.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
d1<-nex
d1[3]<-NULL
nex<-d1
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
aa
writeRaster(xx, filename="C:/Users/Desktop/Present_BtmCur.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
d1<-nex
d1[3]<-NULL
nex<-d1
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
aa
writeRaster(xx, filename="C:/Users/Desktop/Present_MLD.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
d1<-nex
d1[3]<-NULL
nex<-d1
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
aa
writeRaster(xx, filename="C:/Users/Desktop/Present_BtmSal.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
d1<-nex
d1[3]<-NULL
nex<-d1
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
aa
writeRaster(xx, filename="C:/Users/Desktop/Present_SSS.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
d1<-nex
d1[3]<-NULL
nex<-d1
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
aa
writeRaster(xx, filename="C:/Users/Desktop/Present_SST.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
d1<-nex
d1[3]<-NULL
nex<-d1
d1<-d1[,1:3]
aa<-names(d1)[3]
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
aa
writeRaster(xx, filename="C:/Users/Desktop/Present_BtmTmp.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")

########## Create folder and put 9 environmental parameters inside folder:
# "/Present" 

##########################################################################################
### 2. Prepare host occurrence input data
##########################################################################################
d6 <- read.table(file='C:/Users/Desktop/mendeley/Data6_FlemishCap_HMA_LMA.txt', header=T, sep = "")
d6$Nutrient_cycling<-NULL
d6$Habitat_provision<-NULL
d6$HMA_sum_biomass<-NULL
d6$LMA_sum_biomass<-NULL
hma<-d6
lma<-d6
hma$LMA_presence<-NULL
lma$HMA_presence<-NULL
hma2<-subset(hma, (!is.na(hma[,3])))
lma2<-subset(lma, (!is.na(lma[,3])))
hma2$set<-rep(1:length(hma2$HMA_presence))
lma2$set<-rep(1:length(lma2$LMA_presence))
hma2<-hma2[,c(4,1,2,3)] 
lma2<-lma2[,c(4,1,2,3)] 
write.csv(hma2, file='C:/Users/Desktop/hma.csv', row.names=FALSE)
write.csv(lma2, file='C:/Users/Desktop/lma.csv', row.names=FALSE)

