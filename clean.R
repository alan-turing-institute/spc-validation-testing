library(gamlss)
library(fitdistrplus)
library(R.utils)
library(scales)
#library(ggplot2)

folderIn <- "Data"
fdl <- "dl"
farea <- "area"
folderOut <- "Output"
fplot <- "plots"

set.seed(18061815)


###########################
###########################
####### Common data #######
###########################
###########################


# Look-up table
if(!(file.exists(file.path(folderIn,fdl,"lookUp-GB.csv")))){
  download.file("https://ramp0storage.blob.core.windows.net/referencedata/lookUp-GB.csv.gz",file.path(folderIn,fdl,"lookUp-GB.csv.gz"))
  gunzip(file.path(folderIn,fdl,"lookUp-GB.csv.gz"))
}
lu <- read.csv(file.path(folderIn,fdl,"lookUp-GB.csv"))

# Load an area
loadArea <- function(name,date,country = "England"){
  fileName <- paste("pop_",name,"_",date,".csv",sep = "")
  if(!(file.exists(file.path(folderIn,farea,fileName)))){
    url <- paste("https://ramp0storage.blob.core.windows.net/countydata-v2/",country,"/",date,"/",fileName,".gz",sep = "")
    download.file(url,file.path(folderIn,farea,paste(fileName,".gz",sep = "")))
    gunzip(file.path(folderIn,farea,paste(fileName,".gz",sep = "")))
  }
  res <- read.csv(file.path(folderIn,farea,fileName))
  return(res)
}

# Test areas
dataWY <- loadArea("west-yorkshire",2020)
dataL <- loadArea("greater-london",2020)
dataL$MSOA11CD <- substr(dataL$pid,1,9)
dataR <- loadArea("rutland",2020)
dataC <- loadArea("cheshire",2020)


##########################################################
##########################################################
####### Algorithm 1: Cluster within synthetic data #######
##########################################################
##########################################################


#####
##### Moment-based approach
#####

# Extract first four moments for one distribution as vector
findmoments <- function(dstrb){
  step <- descdist(dstrb, discrete = FALSE)
  return(c(step$mean,step$sd,step$skewness,step$kurtosis))
}

# Extract first four moments for geographic area at a specific scale
moments <- function(var,data,scale = c("All","OA11CD","LSOA11CD","MSOA11CD","LAD20CD"),lu){
  varN <- which(colnames(data) == var)
  scale <- match.arg(scale)
  if(scale == "All"){
    scaleV <- list(unique(data$OA11CD))
    names(scaleV) <- "ALL"
    l <- 1
  } else{
    luS <- lu[which(lu$OA11CD %in% data$OA11CD),]
    scaleN <- which(colnames(lu) == scale)
    temp <- unique(luS[,scaleN])
    l <- length(temp)
    scaleV <- rep(list(NA),l)
    names(scaleV) <- temp
    for(i in 1:l){
      scaleV[[i]] <- unique(luS$OA11CD[luS$MSOA11CD == temp[i]])
    }
  }
  res <- data.frame(area = rep(NA,l), mean = NA, sd = NA, skewness = NA, kurtosis = NA)
  for(i in 1:l){
    ref <- which(data$OA11CD %in% scaleV[[i]])
    res[i,1] <- names(scaleV)[i]
    res[i,2:5] <- findmoments(data[ref,varN])
  }
  return(res)
}

# Create heatmap
hmap <- function(data,title){
  data2 <- data[,2:5]
  rownames(data2) <- data[,1]
  heatmap(t(as.matrix(data2)),margins =c(10,0),main = title,Rowv = NA)
}

## Tests

# Age / MSOA
testR <- moments("age",dataR,scale = "MSOA11CD",lu)
testWY <- moments("age",dataWY,scale = "MSOA11CD",lu)
testL <- moments("age",dataL,scale = "MSOA11CD",lu)
testC <- moments("age",dataC,scale = "MSOA11CD",lu)

png(file=file.path(folderOut,fplot,"Age_Rutland_MSOA.png"), width=700, height=500)
hmap(testR,"Rutland - Age")
dev.off()
png(file=file.path(folderOut,fplot,"Age_WY_MSOA.png"), width=700, height=500)
hmap(testWY,"West Yorkshire - Age")
dev.off()
png(file=file.path(folderOut,fplot,"Age_London_MSOA.png"), width=700, height=500)
hmap(testL,"London - Age")
dev.off()
png(file=file.path(folderOut,fplot,"Age_Cheshire_MSOA.png"), width=700, height=500)
hmap(testC,"Cheshire - Age")
dev.off()

# Distributions for some MSOAS in London

age1 <- dataL$age[grep("E02000593",dataL$pid)]
age2 <- dataL$age[grep("E02000358",dataL$pid)]
age3 <- dataL$age[grep("E02000668",dataL$pid)]

png(file=file.path(folderOut,fplot,"Comparison_different_clusters.png"), width=700, height=500)
hist(age2, main = "Age: E02000358 grey, E02000593 red, E02000668 green")
hist(age1,col = alpha(2,0.5), add = T)
hist(age3,col = alpha(3,0.5), add = T)
dev.off()


#### More serious clustering

# Explicit from above
x <- testL[,2:5]
dx <- dist(x)
hmapL <- hclust(dx)
plot(hmapL)

# Normalised clustering
xp <- testL[1:5]
xp$mean <- xp$mean/max(xp$mean)
xp$sd <- xp$sd/max(xp$sd)
xp$skewness <- xp$skewness/max(xp$skewness)
xp$kurtosis <- xp$kurtosis/max(xp$kurtosis)
xpp <- xp
xp <- testL[2:5]
rownames(xp) <- testL$area
dxp <- dist(xp)

hmapLp <- hclust(dxp)
plot(hmapLp)

png(file=file.path(folderOut,fplot,"clusters/Age_normalised_London_MSOA.png"), width=700, height=500)
hmap(xpp,"London - Age (norm.)")
dev.off()

# Get first 4 clusters
temp <- cutree(hmapLp, k = 4)
c1 <- names(temp[which(temp == 1)])
c2 <- names(temp[which(temp == 2)])
c3 <- names(temp[which(temp == 3)])
c4 <- names(temp[which(temp == 4)])

# Histogram for each cluster
age1 <- dataL$age[dataL$MSOA11CD %in% c1]
age2 <- dataL$age[dataL$MSOA11CD %in% c2]
age3 <- dataL$age[dataL$MSOA11CD %in% c3]
age4 <- dataL$age[dataL$MSOA11CD %in% c4]

# Not normalised
png(file=file.path(folderOut,fplot,"clusters/Full_clusters_counts.png"), width=700, height=500)
hist(age2, main = "Age: Left to right {blue, red, green, grey}")
hist(age1,col = alpha(2,0.5), add = T)
hist(age3,col = alpha(3,0.5), add = T)
hist(age4,col = alpha(4,0.5), add = T)
dev.off()

# Normalised
png(file=file.path(folderOut,fplot,"clusters/Full_clusters_dens.png"), width=700, height=500)
hist(age2, freq = F, main = "Age: Left to right {blue, red, green, grey}",ylim = c(0,0.045))
hist(age1,col = alpha(2,0.5), freq = F, add = T)
hist(age3,col = alpha(3,0.5), freq = F, add = T)
hist(age4,col = alpha(4,0.5), freq = F, add = T)
dev.off()

png(file=file.path(folderOut,fplot,"clusters/Full_clusters_dens_multi.png"), width=700, height=500)
par(mfrow = c(2, 2))
hist(age2, freq = F, main = "Age: Left to right {blue, red, green, grey}",ylim = c(0,0.045))
hist(age1,col = alpha(2,0.5), freq = F,ylim = c(0,0.045))
hist(age3,col = alpha(3,0.5), freq = F,ylim = c(0,0.045))
hist(age4,col = alpha(4,0.5), freq = F,ylim = c(0,0.045))
dev.off()


# Table for each cluster
clusterCarac <- data.frame(clsuter = c("blue","red","green","grey"), mean = NA, sd = NA, skewness = NA, kurtosis = NA)
clusterCarac[1,2:5] <- findmoments(age4)
clusterCarac[2,2:5] <- findmoments(age1)
clusterCarac[3,2:5] <- findmoments(age3)
clusterCarac[4,2:5] <- findmoments(age2)
write.table(clusterCarac,file.path(folderOut,fplot,"clusters/clusters_moments.csv"),row.names = F, sep = ",")




hmapLp$height

head(dx)
head(dxp)

# Age / Entire county
testR2 <- moments("age",dataR,scale = "All",lu)
testWY2 <- moments("age",dataWY,scale = "All",lu)
testL2 <- moments("age",dataL,scale = "All",lu)
testC2 <- moments("age",dataC,scale = "All",lu)

all <- rbind(testR2,testWY2,testL2,testC2)
all$area <- c("Rutland","West_Yorkshire","London","Cheshire")
png(file=file.path(folderOut,fplot,"Age_Counties.png"), width=700, height=500)
hmap(all,"Counties")
dev.off()

# Salary / MSOA
dataRnotNA <- dataR[!is.na(dataR$incomeH),]
testRSal <- moments("incomeH",dataRnotNA,scale = "MSOA11CD",lu)
dataWYnotNA <- dataWY[!is.na(dataWY$incomeH),]
testWYSal <- moments("incomeH",dataWYnotNA,scale = "MSOA11CD",lu)
#testLSal <- moments("incomeH",dataL,scale = "MSOA11CD",lu)
#testCSal <- moments("incomeH",dataC,scale = "MSOA11CD",lu)

png(file=file.path(folderOut,fplot,"Salary_Rutland_MSOA.png"), width=700, height=500)
hmap(testRSal,"Rutland - Salary")
dev.off()
png(file=file.path(folderOut,fplot,"Salary_WY_MSOA.png"), width=700, height=500)
hmap(testWYSal,"West Yorkshire - Salary")
dev.off()
#hmap(testLSal,"London - Salary")
#hmap(testCSal,"Cheshire - Salary")


#####
##### Distance-based approach
#####

test <- dataWY$age

# 1. using fitDistPred

system.time(res1 <- fitDistPred(test, k = 2, type = "realplus", trace = FALSE, rand = rand, try.gamlss = TRUE)) # minimum prediction global deviance.
system.time(res2 <- fitDistPred(dataR$age, k = 2, type = "realplus", trace = FALSE, rand = rand, try.gamlss = TRUE))
system.time(res3 <- fitDistPred(dataC$age, k = 2, type = "realplus", trace = FALSE, rand = rand, try.gamlss = TRUE))

res1bis <- res1$fits[order(names(res1$fits))]
res2bis <- res2$fits[order(names(res2$fits))]
res3bis <- res3$fits[order(names(res3$fits))]
res2bis <- c(res2bis[1:7],NA,NA,res2bis[8:18])
res3bis <- c(res3bis[1:7],NA,res3bis[8:19])

plot(res1bis,res2bis,pch = 20, col = alpha(1, alpha = 0.3))
cor(res1bis,res2bis,use = "pairwise.complete.obs")
cor(res1bis,res3bis,use = "pairwise.complete.obs")
cor(res3bis,res2bis,use = "pairwise.complete.obs")

# 2. using chooseDist

testDF <- data.frame(y = dataR$age, x = 1)
resCD <- gamlss(y~1, family=NO, data=testDF)
system.time(t2 <- chooseDist(resCD, type="realline", parallel="snow", ncpus=4))
t2

#getOrder(t2,3)


#################################################
#################################################
####### Algorithm 2: Compare to real data #######
#################################################
#################################################

# /!\ /!\ /!\ try SDMetrics 

