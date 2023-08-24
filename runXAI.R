### Labels for each geography:
#   - NUM: Undirected closeness centrality within commuting network
#   - NUM: Inwards directed closeness centrality within commuting network
#   - NUM: Outwards directed closeness centrality within commuting network
#   - NUM: Undirected betweenness centrality within commuting network
#   - NUM: Distance from local high density areas (calculated within the commuting network)
#   - NUM: Population density
#   - NUM: Median age
#   - NUM: Rank Index of deprivation (pop weighted min for larger areas)
#   - NUM: Score Index of deprivation (pop weighted min for larger areas)

### METHODS: xgboost, SHAP


#############
### Setup ###
#############


install.packages(c('igraph','readxl','geojsonR','geosphere','tidyr','xgboost','shapr','fitdistrplus'))

library(igraph)
library(readxl)
library(geojsonR)
library(geosphere)
library(tidyr)
library(xgboost)
library(shapr)
library(fitdistrplus)

source('functions.R')

# The seed is now randomly chosen within the function and exported to the metadata for reproducibility

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

downloadPrerequisites(folderIn,fdl)


########################
### Run an analysis ####
########################


# <area_name> and <date> must point to one of the .csv files on Azure; the data is only complete for England at this stage.
# Min scale is LSOA due to source control data (scale = c("LSOA11CD", "MSOA11CD", "LAD20CD")).


### Calculate predictors

# prepareLabels(
#               area_name,
#               date,
#               scale = c("LSOA11CD","MSOA11CD","LAD20CD"),
#               folderIn,
#               fdl,
#               fproc,
#               data = NULL,
#               skipLoad = FALSE
#               )

preds <- prepareLabels("west-yorkshire", 2020, "LAD20CD", folderIn, fdl, fproc)

# The first analysis at LSOA scale can be very slow, since all the data must first be prepped and saved once at that scale.
# The long bits can be done directly by running:
#   createCentrality(folderIn,fproc,"LSOA11CD")
# or
#   loadPrerequisites(folderIn,fdl,fproc,"LSOA11CD")

# It is also possible to 
#   - pre-load a specific area:
#       dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)
#   - pre-load all the prerequisites to the global environment once if the scale will remain the same:
#       loadPrerequisites(folderIn,fdl,fproc,"MSOA11CD")
# Then, call the function as
#   prepareLabels("west-yorkshire", 2020, "MSOA11CD", data = dataWY, skipLoad = TRUE)

# Note that the following variables at national level will be accessible directly from the global environment, so that they can be checked easily: 
#   lu, betweenness_global, closeness_global_all, closeness_global_in, closeness_global_out, OD_net_global, popArea_lsoa_global, depriv_global, depriv_scores_global


### Run SHAP

# Check first the basic Pearson correlation between variables and avoid selecting more than one among the ones that are very similar,
# otherwise, they may cancel each other out inside the xgboost model
cor(preds[,-c(1)])
# Variables to be included in the analysis (default is all):
colnames(preds)
columnsPreds <- colnames(preds)[c(2,5:(length(colnames(preds)) - 1))]

# test <- runSHAP(
#                 area_name,
#                 date,
#                 scale = c("LSOA11CD","MSOA11CD","LAD20CD"),
#                 variable_name,
#                 ntrain,
#                 predNames = "all",
#                 data = NULL,
#                 predictors = NULL,
#                 seed = NULL
#                 )

test <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20)
test <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20, predNames = columnsPreds)

test1 <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20, data = dataWY)
test2 <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20, data = dataWY)
test3 <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20, data = dataWY, seed = test1[[5]][5])
test4 <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20, data = dataWY, seed = 18061815)


##

rm(.Random.seed, envir=globalenv())
test <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 30, data = dataWY)
tests <- test[[1]]
for(i in 1:18){
  rm(.Random.seed, envir=globalenv())
  test <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 30, data = dataWY)
  ggplotSHAP2(test, folderOut, file.path(fplot,"tests"))
  tests <- rbind(tests,test[[1]])
}
colSums(tests[,2:ncol(resMean)])/600

##

rm(.Random.seed, envir=globalenv())
test <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 100, data = dataWY)
ggplotSHAP2(test, folderOut, file.path(fplot))
test[[5]]

##

cor(preds[,-c(1)])
# Variables to be included in the analysis (default is all):
colnames(preds)
columnsPreds <- colnames(preds)[c(2,5,7:(length(colnames(preds)) - 1))]
cor(preds[,columnsPreds])
rm(.Random.seed, envir=globalenv())
test <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 60, predNames = columnsPreds)
ggplotSHAP2(test, folderOut, file.path(fplot))
test[[5]]

ggplotSHAP3(test, folderOut, file.path(fplot))

for(i in 1:20){
  test <- testSHAP(8)
  print(test)
}

testSHAP <- function(ntrain, seed = NULL){
  # set seed
  if(is.null(seed)){
    seed <- floor(runif(5,10000000,99999999))[5]
    set.seed(seed)
  }else{
    set.seed(seed)
  }
  testSet <- sample(1:l, ntrain)
  return(c(seed,testSet))
}

### Visualise output plots (writes a png)

ggplotSHAP2(test, folderOut, fplot)

ggplotSHAP2(test1, folderOut, fplot)
ggplotSHAP2(test2, folderOut, fplot)
ggplotSHAP2(test3, folderOut, fplot)
ggplotSHAP2(test4, folderOut, fplot)

rm(.Random.seed, envir=globalenv())
runif(1)
head(.Random.seed)
rm(.Random.seed, envir=globalenv())
runif(1)
head(.Random.seed)

set.seed(.)

test1[[5]]
test2[[5]]
test3[[5]]
test4[[5]]

test3[[1]]
test4[[1]]

