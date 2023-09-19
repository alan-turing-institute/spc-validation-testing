### Cluster matching

library(scales)
library(funtimes)

area_name <- "west-yorkshire"
date <- 2020
scale <- "MSOA11CD"
i = 1

test <- loadLabels(area_name,date, scale)

colnames(test)

head(test)
x <- test[,2:ncol(test)]
dx <- dist(x)
hmapT <- hclust(dx)
plot(hmapT)

colnames(test)
cols <- c("IMD19_ranks","medAge","popDens","distHPD","betweenness","closeness_all")
x2 <- test[,cols]
rownames(x2) <- test[,1]

# Normalised clustering
for(i in 1:ncol(x)){
  x[,i] <- x[,i]/max(x[,i])
  print(range(x[,i]))
}
dx <- dist(x)
hmapT <- hclust(dx)
plot(hmapT)

heatmap(t(as.matrix(x)),margins =c(10,0),main = "All",Rowv = NA)

# Normalised clustering
for(i in 1:ncol(x2)){
  x2[,i] <- x2[,i]/max(x2[,i])
  print(range(x2[,i]))
}
dx2 <- dist(x2)
hmapT2 <- hclust(dx2)
plot(hmapT2)

heatmap(t(as.matrix(x2)),margins =c(10,0),main = "Some",Rowv = NA)

# Get first 9 clusters
nCl = 9
temp <- cutree(hmapT2, k = nCl)
for(i in 1:nCl){
  assign(paste("cc",i,sep = ""),names(temp[which(temp == i)]))
}

sapply(1:nCl,function(x){length(get(paste("cc",x,sep = "")))})

cc <- cc1
data <- test
ceiling(4.5)

plotClusterContent <- function(cc,data, cols){
  par(mfrow = c(2,ceiling(length(cols)/2)))
  for(i in 1:length(cols)){
    plot(data[data$MSOA11CD %in% cc, cols[i]], ylim = c(0, max(data[,cols[i]])), ylab = cols[i],pch = 20, col=alpha(1,0.4))
    text(floor(length(cc)/5),0,paste("Mean =",round(mean(data[data$MSOA11CD %in% cc, cols[i]]),1)))
  }
}

for(x in c("cc1","cc2","cc3","cc4","cc5","cc6","cc7","cc8","cc9")){
  png(file=file.path(folderOut,fplot,paste("clusters/Labels_",x,".png")), width=800, height=500)
  plotClusterContent(get(x),data, cols)
  dev.off()
}

ccs <- cc2

i = ccs
i = 1

### Get moment clusters
scale = "MSOA11CD"
variable_name <- "incomeH"
new_area_list <- data$MSOA11CD
l <- length(new_area_list)

moments <- data.frame(area = rep(NA,l), mean = NA, sd = NA, skewness = NA, kurtosis = NA)
colnames(moments)[1] <- scale
for(i in 1:l){
  ref <- which(predictions[,scale] %in% new_area_list[i])
  moments[i,1] <- new_area_list[i]
  moments[i,2:5] <- findmoments(predictions[ref,variable_name], graph = FALSE)
}
moments <- moments[order(moments[,scale]),]
row.names(moments) <- 1:nrow(moments)

nrow(moments)


xp <- moments[,2:5]
xp$mean <- xp$mean/max(xp$mean)
xp$sd <- xp$sd/max(xp$sd)
xp$skewness <- xp$skewness/max(xp$skewness)
xp$kurtosis <- xp$kurtosis/max(xp$kurtosis)
rownames(xp) <- moments[,1]
dxp <- dist(xp)

hmapWYp <- hclust(dxp)
plot(hmapWYp)

heatmap(t(as.matrix(xp)),margins =c(10,0),main = "Some",Rowv = NA)

# First 4 clusters
temp <- cutree(hmapWYp, k = 4)
c1 <- names(temp[which(temp == 1)])
c2 <- names(temp[which(temp == 2)])
c3 <- names(temp[which(temp == 3)])
c4 <- names(temp[which(temp == 4)])
###

ccs <- list(c1,c2,c3,c4)
ccs2 <- list(cc1,cc2,cc3,cc4,cc5,cc6,cc7,cc8,cc9)
interClust <- function(ccs,ccs2){
  res <- matrix(NA,nrow = length(ccs), ncol = length(ccs2))
  for(i in 1:length(ccs)){
    res[i,] <- sapply(ccs2,function(x){length(which(ccs[[i]] %in% x))})
  }
  return(res)
}
ccs[[2]] %in% ccs2[[1]]

length(ccs[[1]]) + length(ccs[[2]]) + length(ccs[[3]]) + length(ccs[[4]])

i = 1

testY <- interClust(ccs,ccs2)
testY

testZ <- testY
for(i in 1:nrow(testY)){
  testZ[i,] <- round(testZ[i,]/rowSums(testZ)[i] * 100,1)
}
testZ

testZ2 <- testY
for(i in 1:ncol(testY)){
  testZ2[,i] <- round(testZ2[,i]/colSums(testZ2)[i] * 100,1)
}
testZ2



#########
#########
######### Proper functions (see functions/Cluster intersection analysis)
#########
#########




########################################
##### Simple intersection approach #####
########################################


### Metrics for general cluster intersection 

# Optional formatting function assuming data is in a "classic" data frame format
# formatFeat(
#            feat,
#            normalised = FALSE,
#            colNames = colnames(feat)[2:ncol(feat)]
#            )

test <- data.frame(name = letters[1:9], A = 1:9, B = 1:9)
formatFeat(test)
formatFeat(test, normalised = TRUE)
formatFeat(test, colNames = "B")

# Note that pre-processing options within the function are limited.
# Features to be formatted as formatFeat results.
# clustMatching(
#               feat1,
#               feat2,
#               nclust,
#               nclust2 = nclust,
#               format = FALSE,
#               normalised = FALSE,
#               colNames1 = colnames(feat1)[2:ncol(feat1)],
#               colNames2 = colnames(feat2)[2:ncol(feat2)]
#               )

dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)
moments <- makeMoments(dataWY,"MSOA11CD","incomeH")
momentsWY <- formatFeat(moments,normalised = TRUE)
#
labels <- loadLabels("west-yorkshire",2020, "MSOA11CD")
labelsWY <- formatFeat(labels,normalised = TRUE,colNames = c("closeness_all","betweenness","distHPD","popDens","medAge","IMD19_ranks"))
#
test <- clustMatching(momentsWY, labelsWY, nclust = 4)


####################################
##### Cluster characterisation #####
####################################

# Isolate cluster to be plotted
# extractCluster(
#                feat,
#                nclust,
#                format = FALSE,
#                normalised = FALSE,
#                colNames = colnames(feat)[2:ncol(feat)]
#                )

test <- extractCluster(momentsWY, nclust = 4)

# Scatterplot of cluster content
# clusterCarac(
#              cluster,
#              folderOut,
#              fplot,
#              title = NA,
#              breaks = 10
#              )

# Density distributionn of chosen c=variable for each cluster  
# clusterCaracFeature(cluster,
#                     folderOut,
#                     fplot,
#                     data,
#                     variable_name,
#                     scale,
#                     title = NA,
#                     skip = 0,
#                     breaks = 10,
#                     height = "find",
#                     res = 400
#                     )
    
clusterCarac(test, folderOut, fplot, title = "test")
dataWY$MSOA11CD <- substr(dataWY$pid,1,9)
clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "test")


###########################
##### Purity approach #####
###########################

getPurity()

purity()



