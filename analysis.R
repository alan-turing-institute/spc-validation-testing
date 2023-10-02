
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




##### 1. find nclust (control = purity)   <- fix problem "purity" skips small classes???




##### 2. characterise clusters <- !!!!! Fix last function; indexing is obviously wrong



##### 3. check global SHAP values
##### 4. track down where SHAP values are inconsistent !! = Possibility to run indicators for a specific MSOA
