library(Hmsc)
library(tibble)
library(tidyr)
library(stringr)
library(ggplot2)
setwd("C:/Users/Desktop/modelling")
localDir = "."
dataDir = file.path(localDir, "data")
modelDir = file.path(localDir, "models")
resultDir = file.path(localDir, "results")
if (!dir.exists(resultDir)) dir.create(resultDir)
samples_list = c(5,250,250,250,250,250)
thin_list = c(1,1,10,100,1000,10000)
nst = length(thin_list)
nChains = 4
for (Lst in nst:1) {
  thin = thin_list[Lst]
  samples = samples_list[Lst]
  filename = file.path(modelDir,paste("models_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      "_chains_",as.character(nChains),
                                      ".Rdata",sep = ""))
  if(file.exists(filename)){break}
}
load(filename)
m = models[[1]]
grid <- read.table(file.path(dataDir, "grid.txt"), header=T, sep = "")
grid$x<-as.numeric(grid$x)
grid$y<-as.numeric(grid$y)
xy.grid = as.matrix(cbind(grid$x,grid$y))
XData.grid = data.frame(clim=grid$clim, stringsAsFactors = TRUE)
Gradient = prepareGradient(m, XDataNew = XData.grid, sDataNew = list(Route=xy.grid))
nParallel=2
predY = predict(m, Gradient = Gradient, predictEtaMean = TRUE)
EpredY=Reduce("+",predY)/length(predY) 
save(EpredY, file =file.path(resultDir, "predictions.RData"))
pred<-as.data.frame(EpredY)
df <- as.data.frame(t(do.call(rbind, pred)))
df2<-df[rowSums(is.na(df)) != ncol(df), ]
df2 <- tibble::rownames_to_column(df2, "id")
df <- tibble::rownames_to_column(df, "id")
df3<-df2
df3$dummy<-rep("no_NAs", length(df3$id))
df3[,2:(ncol(df3)-1)]<-NULL
df4<-merge(df3, df, by="id", all=T)
grid <- tibble::rownames_to_column(grid, "id")
gg<-merge( grid, df4, by="id")   
gg$id<-NULL
ggg<-gg
gg <- gg[ which(gg$dummy=='no_NAs' ), ]
sub<-gg[,5:ncol(gg)]
sub <- tibble::rownames_to_column(sub, "id")
sub<-as.data.frame(t(sub))
names(sub) <- sub[1,]
sub <- sub[-1,]
subi<-as.data.frame(lapply(sub,as.numeric))
sub[,1:nrow(sub)]<-as.data.frame(lapply(sub,as.numeric))
rela<-sub
rela1<-as.data.frame(rela)
rela1<-lapply(rela1,as.numeric)
rela1<-as.data.frame(rela1)
row.names(rela1)<-row.names(rela)
rela2<-sapply(rela1, function(x) x/sum(x),USE.NAMES = TRUE)  
rela2<-rela2*100
rela2<-as.data.frame(rela2)
row.names(rela2)<-row.names(rela1)
names(rela2)<-names(rela)
rela<-rela2
zz<-as.data.frame(t(rela))
zz <- tibble::rownames_to_column(zz, "id")
gg <- tibble::rownames_to_column(gg, "id")
dd<-gg[,1:5]
fin<-merge(dd,zz, by="id")
fin$id<-NULL
fin$dummy<-NULL
group1 <- fin[ which(fin$clim>=0 & fin$clim<1), ]
group2 <- fin[ which(fin$clim>=1 & fin$clim<2), ]
group3 <- fin[ which(fin$clim>=2 & fin$clim<3), ]
group4 <- fin[ which(fin$clim>=3 & fin$clim<4), ]
group5 <- fin[ which(fin$clim>=4 & fin$clim<5), ]
group6 <- fin[ which(fin$clim>=5 & fin$clim<6), ]
group7 <- fin[ which(fin$clim>=6 & fin$clim<7), ]
group8 <- fin[ which(fin$clim>=7 & fin$clim<8), ]
group9 <- fin[ which(fin$clim>=8 & fin$clim<9), ]
group10 <- fin[ which(fin$clim>=9 & fin$clim<10), ]
group11 <- fin[ which(fin$clim>=10 & fin$clim<11), ]
group12 <- fin[ which(fin$clim>=11 & fin$clim<12), ]
group13 <- fin[ which(fin$clim>=12 & fin$clim<13), ]
group14 <- fin[ which(fin$clim>=13 & fin$clim<14), ]
group15 <- fin[ which(fin$clim>=14 & fin$clim<15), ]
group16 <- fin[ which(fin$clim>=15 & fin$clim<16), ]
group17 <- fin[ which(fin$clim>=16 & fin$clim<17), ]
group18 <- fin[ which(fin$clim>=17 & fin$clim<18), ]
group19 <- fin[ which(fin$clim>=18 & fin$clim<19), ]
group20 <- fin[ which(fin$clim>=19 & fin$clim<20), ]
group21 <- fin[ which(fin$clim>=20 & fin$clim<21), ]
group22 <- fin[ which(fin$clim>=21 & fin$clim<22), ]
group23 <- fin[ which(fin$clim>=22 & fin$clim<23), ]
group24 <- fin[ which(fin$clim>=23 & fin$clim<24), ]
group25 <- fin[ which(fin$clim>=24 & fin$clim<25), ]
group26 <- fin[ which(fin$clim>=25 & fin$clim<26), ]
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
group21$group<-rep("group21", length(group21$clim))
group22$group<-rep("group22", length(group22$clim))
group23$group<-rep("group23", length(group23$clim))
group24$group<-rep("group24", length(group24$clim))
group25$group<-rep("group25", length(group25$clim))
group26$group<-rep("group26", length(group26$clim))
group1 <- gather(group1, taxon, relabund, names(group1)[4]:names(group1)[ncol(group1)-1], factor_key=TRUE)
group2 <- gather(group2, taxon, relabund, names(group2)[4]:names(group2)[ncol(group2)-1], factor_key=TRUE)
group3 <- gather(group3, taxon, relabund, names(group3)[4]:names(group3)[ncol(group3)-1], factor_key=TRUE)
group4 <- gather(group4, taxon, relabund, names(group4)[4]:names(group4)[ncol(group4)-1], factor_key=TRUE)
group5 <- gather(group5, taxon, relabund, names(group5)[4]:names(group5)[ncol(group5)-1], factor_key=TRUE)
group6 <- gather(group6, taxon, relabund, names(group6)[4]:names(group6)[ncol(group6)-1], factor_key=TRUE)
group7 <- gather(group7, taxon, relabund, names(group7)[4]:names(group7)[ncol(group7)-1], factor_key=TRUE)
group8 <- gather(group8, taxon, relabund, names(group8)[4]:names(group8)[ncol(group8)-1], factor_key=TRUE)
group9 <- gather(group9, taxon, relabund, names(group9)[4]:names(group9)[ncol(group9)-1], factor_key=TRUE)
group10 <- gather(group10, taxon, relabund, names(group10)[4]:names(group10)[ncol(group10)-1], factor_key=TRUE)
group11 <- gather(group11, taxon, relabund, names(group11)[4]:names(group11)[ncol(group11)-1], factor_key=TRUE)
group12 <- gather(group12, taxon, relabund, names(group12)[4]:names(group12)[ncol(group12)-1], factor_key=TRUE)
group13 <- gather(group13, taxon, relabund, names(group13)[4]:names(group13)[ncol(group13)-1], factor_key=TRUE)
group14 <- gather(group14, taxon, relabund, names(group14)[4]:names(group14)[ncol(group14)-1], factor_key=TRUE)
group15 <- gather(group15, taxon, relabund, names(group15)[4]:names(group15)[ncol(group15)-1], factor_key=TRUE)
group16 <- gather(group16, taxon, relabund, names(group16)[4]:names(group16)[ncol(group16)-1], factor_key=TRUE)
group17 <- gather(group17, taxon, relabund, names(group17)[4]:names(group17)[ncol(group17)-1], factor_key=TRUE)
group18 <- gather(group18, taxon, relabund, names(group18)[4]:names(group18)[ncol(group18)-1], factor_key=TRUE)
group19 <- gather(group19, taxon, relabund, names(group19)[4]:names(group19)[ncol(group19)-1], factor_key=TRUE)
group20 <- gather(group20, taxon, relabund, names(group20)[4]:names(group20)[ncol(group20)-1], factor_key=TRUE)
group21 <- gather(group21, taxon, relabund, names(group21)[4]:names(group21)[ncol(group21)-1], factor_key=TRUE)
group22 <- gather(group22, taxon, relabund, names(group22)[4]:names(group22)[ncol(group22)-1], factor_key=TRUE)
group23 <- gather(group23, taxon, relabund, names(group23)[4]:names(group23)[ncol(group23)-1], factor_key=TRUE)
group24 <- gather(group24, taxon, relabund, names(group24)[4]:names(group24)[ncol(group24)-1], factor_key=TRUE)
group25 <- gather(group25, taxon, relabund, names(group25)[4]:names(group25)[ncol(group25)-1], factor_key=TRUE)
group26 <- gather(group26, taxon, relabund, names(group26)[4]:names(group26)[ncol(group26)-1], factor_key=TRUE)
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
aa<-rbind(aa, group21)
aa<-rbind(aa, group22)
aa<-rbind(aa, group23)
aa<-rbind(aa, group24)
aa<-rbind(aa, group25)
aa<-rbind(aa, group26)
aa$x<-NULL
aa$y<-NULL
aa$clim<-NULL
aa$comb <- paste(aa$group, aa$taxon, sep="_")
aa$comb <- as.factor(aa$comb)
aa$relabund<-as.numeric(aa$relabund)
group_mean <- aggregate(aa$relabund, list(aa$comb), median)
names(group_mean)[1]<-"comb"
names(group_mean)[2]<-"meanrelabund"
datnew<-str_split_fixed(group_mean$comb, "_", 2)
d=as.data.frame(datnew)
nn<-cbind(d, group_mean)
nn$comb<-NULL
names(nn)[1]<-"clim"
names(nn)[2]<-"taxon"
nn$clim<-gsub("\\bgroup1\\b", "0.5", nn$clim)
nn$clim<-gsub("\\bgroup2\\b", "1.5", nn$clim)
nn$clim<-gsub("\\bgroup3\\b", "2.5", nn$clim)
nn$clim<-gsub("\\bgroup4\\b", "3.5", nn$clim)
nn$clim<-gsub("\\bgroup5\\b", "4.5", nn$clim)
nn$clim<-gsub("\\bgroup6\\b", "5.5", nn$clim)
nn$clim<-gsub("\\bgroup7\\b", "6.5", nn$clim)
nn$clim<-gsub("\\bgroup8\\b", "7.5", nn$clim)
nn$clim<-gsub("\\bgroup9\\b", "8.5", nn$clim)
nn$clim<-gsub("\\bgroup10\\b", "9.5", nn$clim)
nn$clim<-gsub("\\bgroup11\\b", "10.5", nn$clim)
nn$clim<-gsub("\\bgroup12\\b", "11.5", nn$clim)
nn$clim<-gsub("\\bgroup13\\b", "12.5", nn$clim)
nn$clim<-gsub("\\bgroup14\\b", "13.5", nn$clim)
nn$clim<-gsub("\\bgroup15\\b", "14.5", nn$clim)
nn$clim<-gsub("\\bgroup16\\b", "15.5", nn$clim)
nn$clim<-gsub("\\bgroup17\\b", "16.5", nn$clim)
nn$clim<-gsub("\\bgroup18\\b", "17.5", nn$clim)
nn$clim<-gsub("\\bgroup19\\b", "18.5", nn$clim)
nn$clim<-gsub("\\bgroup20\\b", "19.5", nn$clim)
nn$clim<-gsub("\\bgroup21\\b", "20.5", nn$clim)
nn$clim<-gsub("\\bgroup22\\b", "21.5", nn$clim)
nn$clim<-gsub("\\bgroup23\\b", "22.5", nn$clim)
nn$clim<-gsub("\\bgroup24\\b", "23.5", nn$clim)
nn$clim<-gsub("\\bgroup25\\b", "24.5", nn$clim)
nn$clim<-gsub("\\bgroup26\\b", "25.5", nn$clim)
nn$clim<-as.numeric(nn$clim)
nn$taxon<-as.factor(nn$taxon)
oo<-spread(nn, taxon, meanrelabund)
o2<-as.data.frame(t(oo))
names(o2) <- o2[1,]
o2 <- o2[-1,]
o3<-sapply(o2, function(x) x/sum(x),USE.NAMES = TRUE)  
o3<-o3*100
o3<-as.data.frame(o3)
row.names(o3)<-row.names(o2)
o4<-as.data.frame(t(o3))
o4<- tibble::rownames_to_column(o4, "clim")
o4 <-gather(o4, taxon, meanrelabund, names(o4)[2]:names(o4)[ncol(o4)], factor_key=TRUE)
setwd("C:/Users/Desktop")
dir.create("plotting")
write.table(fin, file='C:/Users/Desktop/plotting/spatial_predictions.txt', row.names=FALSE)
