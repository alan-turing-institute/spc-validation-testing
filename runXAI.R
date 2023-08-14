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

set.seed(18061815)

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


# <area_name> and <date> must point to one of the .csv files on Azure
# Min scale is LSOA due to source control data (scale = c("LSOA11CD", "MSOA11CD", "LAD20CD"))


### Output predictors (writes a csv)

# test <- prepareLabels(area_name, date, scale = c("LSOA11CD","MSOA11CD","LAD20CD"), folderIn = folderIn, fdl = fdl, fproc = fproc, data = NULL)
prepareLabels("west-yorkshire", 2020, "MSOA11CD")
preds <- prepareLabels("west-yorkshire", 2020, "MSOA11CD")

# The first analysis at LSOA level can be very slow, since all data must be prepped and saved once at that scale. This can be done directly by running:
# createCentrality(folderIn,fproc,"LSOA11CD")
# or loadPrerequisites(folderIn,fdl,fproc,"LSOA11CD")

# It is also possible to pre-load an area, e.g.:
# dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)
# Then call the function as
# prepareLabels("west-yorkshire", 2020, "MSOA11CD", data = dataWY)

# Note that the following will be saved to the global environment, so that the raw data can be checked easily: 
#   lu, betweenness_global, closeness_global_all, closeness_global_in, closeness_global_out, OD_net_global, popArea_lsoa_global, depriv_global, depriv_scores_global


### Run SHAP

test <- runSHAP(area_name = "west-yorkshire", date = 2020, scale = "LSOA11CD", variable_name = "incomeH", ntrain = 20, data = NULL, predictors = NULL)
test <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20)
#test <- runSHAP("west-yorkshire", 2020, "LSOA11CD", "incomeH", 20, data = dataWY, predictors = preds)


### Visualise output plots (writes a png)

ggplotSHAP2(test, folderOut, fplot)