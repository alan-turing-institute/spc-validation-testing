# Dev version of SHAPR
install.packages('remotes')
library(remotes)
remotes::install_github("NorskRegnesentral/shapr")

# Core
install.packages(c('R.utils','igraph','readxl','geojsonio','broom','tidyr','xgboost','funtimes','fitdistrplus','ggplot2','ggbeeswarm',
                   'geosphere','scales','viridis','gridExtra','plotly'))
#install.packages('geojsonR')
# Export to JSON
install.packages('jsonlite')
# Spatial autocorrelation
install.packages('spdep')

# Directories
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