# Random sandboxes
#install.packages(c('gamlss','gamlss.tr','gamlss.dist','gamlss.add','fitdistrplus','univariateML','R.utils','ggplot2','tidyr'))

# Test XAI:
install.packages('shapr')
install.packages('xgboost')
install.packages('igraph')
install.packages('readxl')
install.packages('geojsonR')
install.packages('geosphere')
install.packages('tidyr')
install.packages('fitdistrplus')

# Test intersection:
install.packages('scales')
install.packages('funtimes')

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