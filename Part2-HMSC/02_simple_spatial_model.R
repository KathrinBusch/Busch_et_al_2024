library(Hmsc)
setwd("C:/Users/Desktop/modelling")
set.seed(1)
localDir = "."
dataDir = file.path(localDir, "data")
modelDir = file.path(localDir, "models")
if(!dir.exists(modelDir)) dir.create(modelDir)
XData  <- read.table(file.path(dataDir, "XData.txt"), header=T, sep = "")
XData$Route = as.factor(XData$Route)
Y  <- read.table(file.path(dataDir, "Y.txt"), header=T, sep = "")
Y<-as.data.frame(lapply(Y,as.numeric))
Y<-as.matrix(Y)
xy <- read.table(file.path(dataDir, "xy.txt"), header=T, sep = "")
colnames(xy)=c("x-coordinate","y-coordinate")
xy<-as.matrix(xy)
studyDesign = data.frame(Route = XData$Route)
rL = HmscRandomLevel(sData=xy)
XFormula = ~clim
m = Hmsc(Y=Y, XData = XData, XFormula=XFormula,
         distr="poisson", studyDesign=studyDesign,
         ranLevels=list(Route=rL))
m
models = list(m)
names(models) = c("animal.pa.model")
save(models, file = file.path(modelDir, "unfitted_models.RData"))
for(i in 1:length(models)){
  print(i)
  sampleMcmc(models[[i]],samples=2)
}

