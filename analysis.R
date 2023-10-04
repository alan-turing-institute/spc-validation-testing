
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

##### Variable analysis #####

num_vars <- colnames(data)[c(5,22,31)]

# + add categorical variables

cat_vars <- colnames(data)[c(7,13,17,18,20,21)]

##### Temporal analysis: not available yet #####


flagKeys <- data.frame(name = "Key1", explanation = "belongs to a small cluster")
flagKeys <- rbind(flagKeys, data.frame(name = "Key2", explanation = "belongs to a minority cluster for labels"))
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

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ #
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #
# |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| #

