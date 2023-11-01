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

preds <- prepareLabels("west-yorkshire", 2020, "MSOA11CD", folderIn, fdl, fproc)

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
columnsPreds <- colnames(preds)[c(2,5,6,8:(length(colnames(preds)) - 1))]

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

testa <- runSHAP2.3("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20, predNames = columnsPreds)

plot(testa[[1]], index_x_explain = 1:10)
plot(testa[[1]], index_x_explain = 1:10, bar_plot_phi0 = FALSE)

plot(testa[[1]], plot_type = "beeswarm")


info <- testa[[5]]
png(file=file.path(folderOut,fplot,paste(info[1], info[2], info[3], info[4], info[5], "feature_importance_beeswarm.png", sep = "-")), width=1250, height=1000)
plot(testa[[1]], plot_type = "beeswarm")
dev.off()

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

