library(sp)
library(raster)
##########################################################################################
### 1. Prepare environmental input data
##########################################################################################
d1 <- read.table(file='Data2_SODA_and_BNAM.txt', header=T, sep = "")
d1<-d1[,1:4]
aa<-names(d1)[4]
d1$group<-NULL
spg <- d1
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
xx <- raster(spg)
writeRaster(xx, filename="Depth.tif", overwrite=TRUE, format="GTiff", datatype="FLT4S")
rm(list=ls(all=TRUE)) 
########## Run this for all parameters and timeframes to generate one individual geotiff-file for each parameter and timeframe. Here shown exemplary for "Depth".
########## Output:
# Depth.tif
# Slope.tif

# Past1871_BtmStress.tif
# Past1871_BtmCur.tif
# Past1871_MLD.tif
# Past1871_BtmSal.tif
# Past1871_SSS.tif
# Past1871_SST.tif
# Past1871_BtmTmp.tif

# Past1901_BtmStress.tif
# Past1901_BtmCur.tif
# Past1901_MLD.tif
# Past1901_BtmSal.tif
# Past1901_SSS.tif
# Past1901_SST.tif
# Past1901_BtmTmp.tif

# Past1931_BtmStress.tif
# Past1931_BtmCur.tif
# Past1931_MLD.tif
# Past1931_BtmSal.tif
# Past1931_SSS.tif
# Past1931_SST.tif
# Past1931_BtmTmp.tif

# Past1961_BtmStress.tif
# Past1961_BtmCur.tif
# Past1961_MLD.tif
# Past1961_BtmSal.tif
# Past1961_SSS.tif
# Past1961_SST.tif
# Past1961_BtmTmp.tif

# Present_BtmStress.tif
# Present_BtmCur.tif
# Present_MLD.tif
# Present_BtmSal.tif
# Present_SSS.tif
# Present_SST.tif
# Present_BtmTmp.tif

# RCP85_2046_2065_BtmStress.tif
# RCP85_2046_2065_BtmCur.tif
# RCP85_2046_2065_MLD.tif
# RCP85_2046_2065_BtmSal.tif
# RCP85_2046_2065_SSS.tif
# RCP85_2046_2065_SST.tif
# RCP85_2046_2065_BtmTmp.tif

# RCP85_2066_2085_BtmStress.tif
# RCP85_2066_2085_BtmCur.tif
# RCP85_2066_2085_MLD.tif
# RCP85_2066_2085_BtmSal.tif
# RCP85_2066_2085_SSS.tif
# RCP85_2066_2085_SST.tif
# RCP85_2066_2085_BtmTmp.tif

########## Create seven folders (one for each timeframe) and put 9 environmental parameters inside folder. Folder names:
# "/x1871" 
# "/x1901" 
# "/x1931" 
# "/x1961" 
# "/Present" 
# "/RCP85_2046_2065" 
# "/RCP85_2066_2085" 

##########################################################################################
### 2. Prepare host occurrence input data
##########################################################################################
d6 <- read.table(file='C:/Users/kbusc/Desktop/mendeley/Data4_Animal_Presence_PseudoAbsences_for_RF.txt', header=T, sep = "")
d6$set<-rep(1:length(d6$group))
d6 <- d6[, c(9,1, 2, 3, 4, 5,6,7,8)]
d6$group<-NULL
names(d6)[2]<-"x"
names(d6)[3]<-"y"
d1<-d6[,1:4]
d2<-d6[, c(1, 2, 3, 5)]
d3<-d6[, c(1, 2, 3, 6)]
d4<-d6[, c(1, 2, 3, 7)]
d5<-d6[, c(1, 2, 3, 8)]
write.csv(d1, file='C:/Users/kbusc/Desktop/Desmophyllum_dianthus.csv', row.names=FALSE)
write.csv(d2, file='C:/Users/kbusc/Desktop/Lophelia_pertusa.csv', row.names=FALSE)
write.csv(d3, file='C:/Users/kbusc/Desktop/Stryphnus_fortis.csv', row.names=FALSE)
write.csv(d4, file='C:/Users/kbusc/Desktop/Vazella_pourtalesii.csv', row.names=FALSE)
write.csv(d5, file='C:/Users/kbusc/Desktop/Weberella_bursa.csv', row.names=FALSE)


