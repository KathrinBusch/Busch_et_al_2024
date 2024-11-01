library(raster)
library(blockCV)
library(rgdal)
library(randomForest)
library(plotROC)
library(data.table)
library(PresenceAbsence)
library(ggplot2)

############################################# 
######## HMA 
############################################# 

# Read environmental predictors (for simplicity here shown for Present and two future timeframes, past timeframes can be added):
wdir<-"C:/Users/Desktop"
setwd(wdir)
rasterdir0 = "/Present" 
Todaypredictorfiles = list.files(path = paste(wdir, rasterdir0, sep=""), pattern = "\\.tif$", full.names = F)
Todaypredictors = c()  
for(x in Todaypredictorfiles)  
{
  predname = paste(wdir,rasterdir0,"/",x,sep="") 
  Todaypredictors = stack(c(Todaypredictors,raster(predname)))  
}

# Give identical variable names to all raster stacks:    MODIFY MANUALLY
names(Todaypredictors)
names(Todaypredictors) <- c('BtmStress', 'BtmCur', 'MLD','BtmSal', 'SSS', 'SST', 'BtmTmp', 'Depth' , 'Slope')  

# Read host occurrence data:
responsefilename = "hma.csv" 
responsefile = read.csv(responsefilename)
dd<-names(responsefile)[4]
responsefile = setDT(responsefile , keep.rownames = TRUE)[]
responsefile$set<-NULL
names(responsefile)[1]<-"ROWID"
names(responsefile)[4]<-"PresenceAbsence"
responsedata = data.frame(PresenceAbsence=responsefile$PresenceAbsence,x=responsefile$x,y=responsefile$y)
response = responsedata[which(!is.na(responsedata[,"PresenceAbsence"])),] 
animalPA<- SpatialPointsDataFrame(response[,c("x", "y")], response, proj4string=crs(Todaypredictors))
Presences<-subset(animalPA, PresenceAbsence=="1")

# Spatial blocking by specified range & random assignment
sac <- spatialAutoRange(rasterLayer = Todaypredictors,sampleNumber = 5000, border = NULL, showPlots = FALSE, plotVariograms = FALSE, doParallel = FALSE) # if the command does not work use: sac <- cv_spatial_autocor(r=Todaypredictors,num_sample=5000,plot=FALSE)
folds=5
sp.blocks <- spatialBlock(speciesData = animalPA, species = "PresenceAbsence", rasterLayer = Todaypredictors, theRange = sac$range, k = folds, selection = "random", numLimit=0, iteration = 500, degMetre= 77329, biomod2Format = TRUE, xOffset = 0.35, yOffset = 0.1, showBlocks=FALSE)  # degMetre= centre of study area, 46N; # if the command does not work use: sp.blocks<-cv_spatial(x=animalPA,column="PresenceAbsence",r=Todaypredictors,k=folds, size=sac$range, selection="random", iteration=500,deg_to_metre=77329, biomod2=TRUE, offset=c(0.35,0.1),plot=FALSE)  
cv <- list()
for ( f in 1:folds ){
  foldname <- paste0("fold",f)
  cv[[foldname]][['train']] <- which(sp.blocks$foldID!=f)
  cv[[foldname]][['test']] <- which(sp.blocks$foldID==f)
}
foldblockinfo<-list(foldID=sp.blocks$foldID, blockpolys=sp.blocks$blocks, cv=cv)
Todayblocks<-foldblockinfo$blockpolys

# SDM modelling - Random Forest (RF)
mydata <- extract(Todaypredictors, animalPA, df=TRUE)
mydata$PresenceAbsence <- as.factor(animalPA$PresenceAbsence)
mydata <- mydata[,2:ncol(mydata)]
folds <- sp.blocks$folds # if the command does not work use: folds <- sp.blocks$folds_list
RF_testTable <- animalPA@data  
RF_testTable$pred <- NA 
RF_AUCs <- vector()  
RF_varimplist <- list()
rf_fold <- list()
setwd("C:/Users/Desktop/")
for(k in seq_len(length(folds))){
  trainSet <- unlist(folds[[k]][1]) 
  testSet <- unlist(folds[[k]][2])
  rf_fold[[k]] <- randomForest(PresenceAbsence~., na.exclude(mydata[trainSet, ]), ntree = 500, importance= TRUE) 
  RF_testTable$pred[testSet]<- predict(rf_fold[[k]], mydata[testSet, ], type = "prob")[,2] 
  variableimportance <- importance(rf_fold[[k]], type=1, scale=FALSE) 
  RF_varimplist[[k]] <- variableimportance
  rf_trainSetPredict = predict(Todaypredictors, rf_fold[[k]], type="prob", index=2)
  outfile<-paste0('fold', k, '_hma_Present_PresProb_RF','.tif')
  writeRaster(rf_trainSetPredict, filename=outfile, overwrite=TRUE, format="GTiff", datatype="FLT4S")
  auc <- calc_auc(ggplot(RF_testTable, aes(m=pred, d=PresenceAbsence)) + geom_roc(n.cuts = 0))[3]
  RF_AUCs[k] <- as.numeric(auc)
}
rf_fold1 <- rf_fold[[1]]  
rf_fold2 <- rf_fold[[2]]
rf_fold3 <- rf_fold[[3]]
rf_fold4 <- rf_fold[[4]]
rf_fold5 <- rf_fold[[5]]

# Calculate confusion matrix from testing data (using MaxSens+Spec from Optimal Thresholds) & export summary of accurracy measures:
RF_PA_pred<-RF_testTable[, c("PresenceAbsence", "pred")]
RF_PA_pred$plotID <- seq.int(nrow(RF_PA_pred))
RF_PA_pred<-RF_PA_pred[c("plotID", "PresenceAbsence", "pred")]
colnames(RF_PA_pred)[2] <- "Observed"
colnames(RF_PA_pred)[3] <- "Predicted1"
Optimal_Thresholds_RF<-optimal.thresholds(RF_PA_pred, na.rm=TRUE) 
nn<- Optimal_Thresholds_RF[ which(Optimal_Thresholds_RF$Method=="MaxSens+Spec"), ]
Confu_Mat_RF<-cmx(RF_PA_pred, threshold=nn$Predicted1[1], na.rm=TRUE) 
Sensitivity_RF<-sensitivity(Confu_Mat_RF, st.dev=TRUE)
Specificity_RF<-specificity(Confu_Mat_RF, st.dev=TRUE)
TSS_RF <- (Sensitivity_RF$sensitivity + Specificity_RF$specificity)-1
a1<-round(mean(RF_AUCs, na.rm=T),digits=2)
a2<-round(sd(RF_AUCs, na.rm=T),digits=2)
b1<-round(Sensitivity_RF[,1],digits=2)
b2<-round(Sensitivity_RF[,2],digits=2)
c1<-round(Specificity_RF[,1],digits=2)
c2<-round(Specificity_RF[,2],digits=2)
a<-paste(a1, a2, sep="+-")
b<-paste(b1, b2, sep="+-")
c<-paste(c1, c2, sep="+-")
dd<-as.data.frame(dd)
a<-as.data.frame(a)
b<-as.data.frame(b)
c<-as.data.frame(c)
mm<-round(TSS_RF, digits=2)
mm<-as.data.frame(mm)
nn<-nn$Predicted1[1]
nn<-as.data.frame(nn)
ee<-cbind(dd,a)
ee<-cbind(ee,b)
ee<-cbind(ee,c)
ee<-cbind(ee,mm)
ee<-cbind(ee,nn)
names(ee)[1]<-"host_species"
names(ee)[2]<-"Mean_AUC_+-_SD"
names(ee)[3]<-"Sensitivity_+-_SD"
names(ee)[4]<-"Specificity_+-_SD"
names(ee)[5]<-"TSS"
names(ee)[6]<-"MSS_threshold"
write.table(ee, file='C:/Users/Desktop/hma_Accuracy_measures_RF.txt', row.names=FALSE)

# Calculate mean of presence probability rasters
dir.create("C:/Users/Desktop/hma_PresenceProbabilityRasters")
dir.create("C:/Users/Desktop/hma_PresenceProbabilityRasters/Present")
file.copy(from = paste0("C:/Users/Desktop/fold1_hma_Present_PresProb_RF.tif"), to = paste0("C:/Users/Desktop/hma_PresenceProbabilityRasters/Present/fold1_hma_Present_PresProb_RF.tif"))
file.copy(from = paste0("C:/Users/Desktop/fold2_hma_Present_PresProb_RF.tif"), to = paste0("C:/Users/Desktop/hma_PresenceProbabilityRasters/Present/fold2_hma_Present_PresProb_RF.tif"))
file.copy(from = paste0("C:/Users/Desktop/fold3_hma_Present_PresProb_RF.tif"), to = paste0("C:/Users/Desktop/hma_PresenceProbabilityRasters/Present/fold3_hma_Present_PresProb_RF.tif"))
file.copy(from = paste0("C:/Users/Desktop/fold4_hma_Present_PresProb_RF.tif"), to = paste0("C:/Users/Desktop/hma_PresenceProbabilityRasters/Present/fold4_hma_Present_PresProb_RF.tif"))
file.copy(from = paste0("C:/Users/Desktop/fold5_hma_Present_PresProb_RF.tif"), to = paste0("C:/Users/Desktop/hma_PresenceProbabilityRasters/Present/fold5_hma_Present_PresProb_RF.tif"))
file.remove(from = paste0("C:/Users/Desktop/fold1_hma_Present_PresProb_RF.tif"))
file.remove(from = paste0("C:/Users/Desktop/fold2_hma_Present_PresProb_RF.tif"))
file.remove(from = paste0("C:/Users/Desktop/fold3_hma_Present_PresProb_RF.tif"))
file.remove(from = paste0("C:/Users/Desktop/fold4_hma_Present_PresProb_RF.tif"))
file.remove(from = paste0("C:/Users/Desktop/fold5_hma_Present_PresProb_RF.tif"))
rasterdir = "C:/Users/Desktop/hma_PresenceProbabilityRasters/Present" 
presenceprobfiles = list.files(path = paste(rasterdir, sep=""), pattern = "\\.tif$", full.names = F)
presprobstack = c()  
for(x in presenceprobfiles)  
{
  predname = paste(rasterdir,"/",x,sep="")  
  presprobstack = stack(c(presprobstack,raster(predname)))  
}
r_mean <- calc(presprobstack, mean)  
outfile1 = "hma_PresenceProbabilityRasters/hma_Present_MeanPresProbRaster_RF.tif"
writeRaster(r_mean, filename=outfile1, overwrite=TRUE, format="GTiff", datatype="FLT4S")

########## Run this script also for LMA sponges
########## by replacing "hma" by "lma"
