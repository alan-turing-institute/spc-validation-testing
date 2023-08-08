### Labels for each geography:
#   - NUM: Closeness centrality from ONS OD matrix (Azure)
#   - NUM: Betweenness centrality from ONS OD matrix (Azure)
#   - NUM: Distance from nearest local high density area (?)
#   - NUM: Population density (Compute from Shp)
#   - NUM: Median age (Compute from SPC - Azure)
#   - NUM: Index of deprivation (2019; https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)

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

dir.create(folderIn)
dir.create(file.path(folderIn,fdl))
dir.create(file.path(folderIn,farea))
dir.create(folderOut)
dir.create(file.path(folderOut,fplot))

downloadPrerequisites(folderIn,fdl)
# Make sure to select the scale you intend to work with later (for LAD20CD, use MSOA11CD here)
loadPrerequisites(folderIn,fdl,scale = "MSOA11CD")
# After these two steps, the following should be loaded into the global environment: 
#   folderIn, farea, lu, betweenness_global, closeness_global_all, closeness_global_in, closeness_global_out, popArea_lsoa_global, depriv_global


########################
### Run an analysis ####
########################

# Optional: load once, then pass as argument to avoid lengthy loading times
#dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)

# <area_name> and <date> must point to one of the .csv files on Azure
# Min scale is LSOA due to source control data (scale = c("LSOA11CD", "MSOA11CD", "LAD20CD"))

# Output predictors (writes a csv)
test <- prepareLabels(area_name = "west-yorkshire", date = 2020, scale = "LSOA11CD", data = NULL)
test <- prepareLabels("west-yorkshire", 2020, "MSOA11CD")
test <- prepareLabels("west-yorkshire", 2020, "LAD20CD")
#test <- prepareLabels("west-yorkshire", 2020, "LSOA11CD", data = dataWY)

# Results must be passed to a variable to be plotted;
test <- runSHAP(area_name = "west-yorkshire", date = 2020, scale = "LSOA11CD", variable_name = "incomeH", ntrain = 20, data = NULL, predictors = NULL)
test <- runSHAP("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20)
#test <- runSHAP("west-yorkshire", 2020, "LSOA11CD", "incomeH", 20, data = dataWY, predictors = test)

# Output plots (writes a png)
#plotSHAP(test, folderOut, fplot)
ggplotSHAP2(test, folderOut, fplot)
