library(Hmsc)
library(colorspace)
library(corrplot)
library(writexl)
setwd("C:/Users/Desktop/modelling")
set.seed(1)
nfolds = NULL 
localDir = "."
modelDir = file.path(localDir, "models")
resultDir = file.path(localDir, "results")
if (!dir.exists(resultDir)) dir.create(resultDir)
if(is.null(nfolds)) nfolds = 2
samples_list = c(5,250,250,250,250,250)
thin_list = c(1,1,10,100,1000,10000)
nst = length(thin_list)
nChains = 4
for (Lst in nst:1) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  filename = file.path(modelDir,paste("MF_thin_", as.character(thin), "_samples_", as.character(samples), "_chains_",as.character(nChains), "_nfolds_", as.character(nfolds), ".Rdata",sep = ""))
  if(file.exists(filename)){break}
}
if(file.exists(filename)){
  load(filename)
  nm = length(MF)
  modelnames = names(MF)
  for(j in 1:nm){
    cMF = MF[[j]]
    cMFCV = MFCV[[j]]
    }  
  }
plot(cMF$SR2,cMFCV$SR2,xlim=c(-1,1),ylim=c(-1,1), xlab = "explanatory power", ylab = "predictive power", main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": SR2. \n", "mean(MF) = ",as.character(mean(cMF$SR2,na.rm=TRUE)),", mean(MFCV) = ",as.character(mean(cMFCV$SR2,na.rm=TRUE))))
abline(0,1)
abline(v=0)
abline(h=0)
data<-data.frame(cMF$SR2,cMFCV$SR2)
names(data)[1]<-"explanatory"
names(data)[2]<-"predictive"
data$check<-data$explanatory/data$predictive
data$check<-round(data$check, digits=1)
support.level.beta = NULL 
support.level.gamma = NULL 
support.level.omega = NULL
var.part.order.explained = NULL 
var.part.order.raw = NULL 
show.sp.names.beta = NULL 
plotTree = NULL
omega.order = NULL 
show.sp.names.omega = NULL
if (!dir.exists(resultDir)) dir.create(resultDir)
if(is.null(support.level.beta)) support.level.beta = 0.95
if(is.null(support.level.gamma)) support.level.gamma =  0.95
if(is.null(support.level.omega)) support.level.omega =  0.9
text.file = file.path(resultDir,"/parameter_estimates.txt")
cat(c("This file contains additional information regarding parameter estimates.","\n","\n",sep=""),file=text.file)
for (Lst in nst:1) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  filename = file.path(modelDir,paste("models_thin_", as.character(thin),"_samples_", as.character(samples),"_chains_",as.character(nChains),".Rdata",sep = ""))
  if(file.exists(filename)){break}
}
if(file.exists(filename)){
  load(filename)
  cat(c("\n",filename,"\n","\n"),file=text.file,sep="",append=TRUE)
  nm = length(models)
  modelnames = names(models)
  m = models[[j]]
}
names<-m$spNames
data$names<-m$spNames
data2 <- data[ which(data$check >= 1), ]  
data2 <- data2[ which(data2$check < 2), ]  
tax <- read.table(file='C:/Users/Desktop/modelling/outputs/ASV_tax.txt', header=T, sep = "")  
names(data2)[4]<-"FeatureID"
data3<-merge(data2, tax, by="FeatureID")
names(data3)[4]<-"ratio"
data3 <- data3[order(-data3$explanatory),] 
write.table(data3, file='C:/Users/Desktop/plotting/ASVs_perfect_model_fit.txt', row.names=FALSE)
data5<-data
names(data5)[4]<-"FeatureID"
data6<-merge(data5, tax, by="FeatureID")
names(data6)[4]<-"ratio"
data6 <- data6[order(-data6$ratio),] 
write.table(data6, file='C:/Users/Desktop/plotting/all_ASVs_model_fit.txt', row.names=FALSE)
