
areas <- unique(lu$AzureRef[lu$Country == "England"])
length(areas)

##### Spatial analysis: all Counties in England #####
loadPrerequisites(folderIn,fdl,fproc,"MSOA11CD")
for(i in areas){
  prepareLabels(i, 2020, "MSOA11CD", folderIn, fdl, fproc, skipLoad = TRUE)
}

##### Scale analysis: "west-yorkshire" for 3 scales #####
prepareLabels("west-yorkshire", 2020, "LSOA11CD", folderIn, fdl, fproc)
prepareLabels("west-yorkshire", 2020, "LAD20CD", folderIn, fdl, fproc)


### MSOA11CD

dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)
moments <- makeMoments(dataWY, "MSOA11CD", "incomeH")
momentsWY <- formatFeat(moments, normalised = TRUE)
#
labels <- loadLabels("west-yorkshire",2020, "MSOA11CD")
labelsWY <- formatFeat(labels, normalised = TRUE, colNames = c("closeness_all","betweenness","distHPD","popDens","medAge","IMD19_ranks"))
#
findNumberClusters(momentsWY, labelsWY, 3, 10)
# Optimal: {3,4}, but let's take {5,5}
interWY <- clustMatching(momentsWY, labelsWY, nclust = 5, nclust2 = 5, skip = 13, flags = T)
readFlags(interWY$flags, map = T, scale = "MSOA11CD")
#
test <- extractCluster(momentsWY, nclust = 5)
testa <- extractCluster(labelsWY, nclust = 5)
clusterMapping(test,"MSOA11CD")
clusterMapping(testa,"MSOA11CD")
clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "moments", height = "find", width = 100, skip = 3)
clusterCaracFeature(testa, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "labels", height = "find", width = 100, skip = 3)
clusterCaracFeature(test, folderOut, fplot, data = dataWY, variable_name = "age", scale = "MSOA11CD", title = "moments", height = "find", width = 100, skip = 3)
clusterCaracFeature(testa, folderOut, fplot, data = dataWY, variable_name = "age", scale = "MSOA11CD", title = "labels", height = "find", width = 100, skip = 3)
testb <- extractCluster(momentsWY, nclust = 10)
clusterCaracFeature(testb, folderOut, fplot, data = dataWY, variable_name = "incomeH", scale = "MSOA11CD", title = "moments_more", height = "find", width = 100, skip = 3)
clusterCarac(test, folderOut, fplot, title = "moments")
clusterCarac(testa, folderOut, fplot, title = "labels")

##### Variable analysis #####

num_vars <- colnames(data)[c(5,22,31)]

# + add categorical variables

cat_vars <- colnames(data)[c(7,13,17,18,20,21)]

##### Temporal analysis: not available yet #####


flagKeys <- data.frame(name = "Key1", explanation = "belongs to a small cluster")
flagKeys <- rbind(flagKeys, data.frame(name = "Key2", explanation = "belongs to a minority cluster for labels"))
flagKeys <- rbind(flagKeys, data.frame(name = "X_Y", explanation = "further than that number of SD for characteristic X explained by Y"))
flagKeys

readFlags(flags, map = T, scale = "MSOA11CD")



##### 1. find nclust (control = purity)   <- fix problem "purity" skips small classes???
##### 2. characterise clusters
########## 2.1 Fix last function; indexing is obviously wrong
########## 2.2 Write explicit outlier detection

# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #
# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv #

##### 3. check global SHAP values
##### 4. track down where SHAP values are inconsistent !! + Possibility to run indicators for a specific MSOA

##### 5. Convert and export flagged output as JSON
########## 5.1 Change colours in flag map + 500-bins instead of1000?

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ #
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #

