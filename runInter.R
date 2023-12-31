#############
### Setup ###
#############

library(shapr)
library(R.utils)
library(igraph)
library(readxl)
library(geojsonio)
library(broom)
library(tidyr)
library(xgboost)
library(funtimes)
library(fitdistrplus)
library(ggplot2)
library(ggbeeswarm)
library(geosphere)
library(scales)
library(viridis)
library(gridExtra)
library(plotly)
#library(geojsonR)
# Export to JSON
library(jsonlite)
# Spatial autocorrelation
library(spdep)

source("functions.R")

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

areas <- unique(lu$AzureRef[lu$Country == "England"])
length(areas)


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
#.              flags = F
#               )

dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)
moments <- makeMoments(dataWY, "MSOA11CD", "incomeH")
momentsWY <- formatFeat(moments, normalised = TRUE)
#
labels <- loadLabels("west-yorkshire",2020, "MSOA11CD")
labelsWY <- formatFeat(labels, normalised = TRUE, colNames = c("closeness_all","betweenness","distHPD","popDens","medAge","IMD19_ranks"))
#
test <- clustMatching(momentsWY, labelsWY, nclust = 3, nclust2 = 4, skip = 13, flags = T)
View(test$flags)

test <- clustMatching(momentsWY, labelsWY, nclust = 4, nclust2 = 4, skip = 13, flags = T)
View(test$flags)

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

# Mapping
# clusterMapping(
#                cluster,
#                scale = c("LSOA11CS","MSOA11CD","LAD20CD")
#               )

clusterMapping(test,"MSOA11CD")
clusterMapping(testa,"MSOA11CD")

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

clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "test3", height = "find", skip = 3)
clusterCaracFeature(testa, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "test3a", height = "find", skip = 3)


test <- extractCluster(momentsWY, nclust = 10)
clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "test4", height = "find", skip = 3)


