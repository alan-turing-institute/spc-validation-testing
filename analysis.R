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


#####################################################
##### Spatial analysis: all Counties in England #####
#####################################################


loadPrerequisites(folderIn,fdl,fproc,"MSOA11CD")
for(i in areas){
  prepareLabels(i, 2020, "MSOA11CD", folderIn, fdl, fproc, skipLoad = TRUE)
}


#########################################################
##### Scale analysis: "west-yorkshire" for 3 scales #####
#########################################################


prepareLabels("west-yorkshire", 2020, "LSOA11CD", folderIn, fdl, fproc)
prepareLabels("west-yorkshire", 2020, "MSOA11CD", folderIn, fdl, fproc)
prepareLabels("west-yorkshire", 2020, "LAD20CD", folderIn, fdl, fproc)


### MSOA11CD

# Get moments
dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)
moments <- makeMoments(dataWY, "MSOA11CD", "incomeH")
momentsWY <- formatFeat(moments, normalised = TRUE)

# Get labels
labels <- loadLabels("west-yorkshire",2020, "MSOA11CD")
labelsWY <- formatFeat(labels, normalised = TRUE, colNames = c("closeness_all","betweenness","distHPD","popDens","medAge","IMD19_ranks"))
columnsPreds <- c("closeness_all","betweenness","popDens","medAge","IMD19_ranks" )

# Run cluster intersection approach
findNumberClusters(momentsWY, labelsWY, 3, 10)
# Optimal: {3,4}, but let's take {5,5}
interWY <- clustMatching(momentsWY, labelsWY, nclust = 5, nclust2 = 5, skip = 13, flags = T)
readFlags(interWY$flags, map = T, scale = "MSOA11CD")

# Plot results
test <- extractCluster(momentsWY, nclust = 5)
clusterMapping(test,"MSOA11CD")
clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "moments", height = "find", width = 100, skip = 3,pdf = T)
clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "age", scale = "MSOA11CD", title = "moments", height = "find", width = 100, skip = 3,pdf = T)
clusterCarac(test, folderOut, fplot, title = "moments", pdf = T)
#
testa <- extractCluster(labelsWY, nclust = 5)
clusterMapping(testa,"MSOA11CD")
clusterCaracFeature(testa, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "labels", height = "find", width = 100, skip = 3,pdf = T)
clusterCaracFeature(testa, folderOut, fplot, data = dataWY, variable_name = "age", scale = "MSOA11CD", title = "labels", height = "find", width = 100, skip = 3,pdf = T)
clusterCarac(testa, folderOut, fplot, title = "labels", pdf = T)
#
testb <- extractCluster(momentsWY, nclust = 10)
clusterCaracFeature(testb, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "moments_more", height = "find", width = 100, skip = 3,pdf = T)

# Run SHAP approach
train <- rep(list(NULL),10)
for(i in 1:9){
  train[[i]] <- (i-1)*30 + 1:30
}
train[[10]] <- 271:299
#
shapRes <- runSHAP2.3("west-yorkshire", 2020, "MSOA11CD", "incomeH", ntrain = train[[1]], predNames = columnsPreds, data = dataWY)
SHAP_all <- flagSHAP(shapRes,areas = sort(unique(dataWY$MSOA11CD))[train[[1]]])
SHAP_flags <- SHAP_all[[1]]
for(i in 2:10){
  shapRes <- runSHAP2.3("west-yorkshire", 2020, "MSOA11CD", "incomeH", ntrain = train[[i]], predNames = columnsPreds, data = dataWY)
  SHAP_all2 <- flagSHAP(shapRes,areas = sort(unique(dataWY$MSOA11CD))[train[[i]]])
  SHAP_flags2 <- SHAP_all2[[1]]
  SHAP_flags <- dplyr::bind_rows(SHAP_flags, SHAP_flags2)
  print("**********")
  print(paste(i,"/10",sep = ''))
  print("**********")
}

# Result plots
plot(shapRes[[1]], index_x_explain = 1:10)
plot(shapRes[[1]], index_x_explain = 1:10, bar_plot_phi0 = FALSE)

png(file=file.path(folderOut,fplot,paste(title, "beeswarm_", variable_name, ".png", sep = "")), width= 300, height=1025/5)
plot(shapRes[[1]], plot_type = "beeswarm", cex = 1)
dev.off()

pdf(file=file.path(folderOut,fplot,paste(title, "beeswarm_", variable_name, ".pdf", sep = "")), width= 3, height=10.75/5)
plot(shapRes[[1]], plot_type = "beeswarm", cex = 1)
dev.off()

# Combined area flagging
all_flags <- mergeFlags(interWY$flags,SHAP_flags, weight = 6)
readFlags(all_flags, over = 8000)
readFlags(all_flags, map = T, scale = "MSOA11CD")

# Area queries
queryArea("E02002189", all_flags, dataWY, labels, "MSOA11CD", "incomeH", 5, 5, colNames)
queryArea("E02002312", all_flags, dataWY, labels, "MSOA11CD", "incomeH", 5, 5, colNames)

# Autocorrelation
autocor <- applyAutocor(dataWY, scale = "MSOA11CD", "incomeH")



#############################
##### Variable analysis #####
#############################


num_vars <- colnames(data)[c(5,22,31)]

# + add categorical variables

cat_vars <- colnames(data)[c(7,13,17,18,20,21)]


################################################
##### Temporal analysis: not available yet #####
################################################


# ...


#################
##### TO DO #####
#################


##### 1. find nclust (control = purity)   <- fix problem "purity" skips small classes???
##### 2. characterise clusters
########## 2.1 Fix last function; indexing is obviously wrong
########## 2.2 Write explicit outlier detection
##### 3. check global SHAP values
##### 4. track down where SHAP values are inconsistent
##### 5. write code to run indicators for a specific MSOA
########## 5.1 distributions
########## 5.2 maps
##### 6. Add spatial autocorrelation
##### 7. convert and export flagged output as JSON

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv #

##### 8. Add renv

##### 9. online documentation
##### 10. journal paper

##### 11. Distribution reshaping

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ #
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #

