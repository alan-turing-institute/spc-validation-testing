#############
### Setup ###
#############


library(scales)
library(funtimes)
library(fitdistrplus)

source('functions.R')

folderIn <- "Data"
fdl <- "dl"
farea <- "area"
folderOut <- "Output"
fplot <- "plots"
fproc <- "processed"

dir.create(folderIn)
dir.create(file.path(folderIn,fdl))
dir.create(file.path(folderIn,farea))
dir.create(file.path(folderIn,fproc))
dir.create(folderOut)
dir.create(file.path(folderOut,fplot))


#############################
### Intersection approach ###
#############################


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
# Format as formatFeat results.
# Discards clusters of size less than skip
# clustMatching(
#               feat1,
#               feat2,
#               nclust,
#               nclust2 = nclust,
#               skip = floor(0.05 * nrow(feat1)),
#               format = FALSE,
#               normalised = FALSE,
#               colNames1 = colnames(feat1)[2:ncol(feat1)],
#               colNames2 = colnames(feat2)[2:ncol(feat2)]
#               )

dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)
moments <- makeMoments(dataWY, "MSOA11CD", "incomeH")
momentsWY <- formatFeat(moments, normalised = TRUE)
#
labels <- loadLabels("west-yorkshire",2020, "MSOA11CD")
labelsWY <- formatFeat(labels, normalised = TRUE, colNames = c("closeness_all","betweenness","distHPD","popDens","medAge","IMD19_ranks"))
#
test <- clustMatching(momentsWY, labelsWY, nclust = 3, nclust2 = 4, skip = 13)

# Utility to estimate the best number of clusters
# Format as formatFeat results
# findNumberClusters(
#                    feat1,
#                    feat2,
#                    min,
#                    max)

test <- findNumberClusters(momentsWY, labelsWY, 3, 10)
test


################################
### Cluster characterisation ###
################################


# Isolate clusters to be plotted
# extractCluster(
#                feat,
#                nclust,
#                format = FALSE,
#                normalised = FALSE,
#                colNames = colnames(feat)[2:ncol(feat)]
#                )

test <- extractCluster(momentsWY, nclust = 3)
testa <- extractCluster(labelsWY, nclust = 4)

# Density distributions of clustering features within each cluster
# clusterCarac(
#              cluster,
#              folderOut,
#              fplot,
#              title = NA,
#              breaks = 10
#              )

# Density distributions of chosen variable for each cluster  
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
clusterCarac(testa, folderOut, fplot, title = "testa")

dataWY$MSOA11CD <- substr(dataWY$pid,1,9)

clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "test")
clusterCaracFeature(testa, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "testa", height = FALSE, skip = 3)

colnames(dataWY)

clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "age", scale = "MSOA11CD", title = "test2")

clusterCaracFeature(testa, folderOut, fplot, data = dataWY, variable_name = "age", scale = "MSOA11CD", title = "testa3", height = FALSE)




