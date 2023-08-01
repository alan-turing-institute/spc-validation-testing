library(igraph)
library(readxl)
library(geojsonR)
library(geosphere)
library(tidyr)

### Labels for each geography:
#   - NUM: Closeness centrality from ONS OD matrix (Azure)
#   - NUM: Betweenness centrality from ONS OD matrix (Azure)
#   - NUM: Distance from nearest local high density area (?)
#   - NUM: Population density (Compute from Shp)
#   - NUM: Median age (Compute from SPC - Azure)
#   - NUM: Index of deprivation (2019; https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)

### METHODS: xgboost, SHAP

### TO DO:
#   Check def of betweenness
#   Check def of closeness
#   Refine distance to nearest HPD area

folderIn <- "Data"
fdl <- "dl"
farea <- "area"
folderOut <- "Output"
fplot <- "plots"

set.seed(18061815)


##############################
##############################
####### Make test data #######
##############################
##############################


downloadPrerequisites <- function(folderIn,fdl){
  # LookUp table
  if(!(file.exists(file.path(folderIn,fdl,"lookUp-GB.csv")))){
    download.file("https://ramp0storage.blob.core.windows.net/referencedata/lookUp-GB.csv.gz",file.path(folderIn,fdl,"lookUp-GB.csv.gz"))
    gunzip(file.path(folderIn,fdl,"lookUp-GB.csv.gz"))
  }
  # OD matrix
  if(!(file.exists(file.path(folderIn,fdl,"commutingOD.csv")))){
    download.file(url = "https://ramp0storage.blob.core.windows.net/nationaldata/commutingOD.gz", destfile = file.path(folderIn,fdl,"commutingOD.csv.gz"))
    gunzip(file.path(folderIn,fdl,"commutingOD.csv.gz"))
  }
  # Pop at LSOA scale
  if(!(file.exists(file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson")))){
    download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/LSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))
  }
}

loadPrerequisites <- function(folderIn,fdl){
  # LookUp table
  assign("lu", read.csv(file.path(folderIn,fdl,"lookUp-GB.csv")), envir = globalenv())
  # Centrality from OD matrix as network
  OD <- read.csv(file.path(folderIn,fdl,"commutingOD.csv"), header = T)
  OD_wide <- pivot_wider(OD, names_from = HomeMSOA, values_from = Total_Flow)
  OD_wide <- as.matrix(OD_wide)
  OD_wide_prep <- OD_wide[,2:ncol(OD_wide)]
  rownames(OD_wide_prep) <- OD_wide[,1]
  OD_net <- graph_from_adjacency_matrix(OD_wide_prep, mode = "directed", weighted = TRUE, diag = TRUE)
  assign("betweenness_global", betweenness(OD_net,directed = TRUE, normalized = TRUE),  envir = globalenv())
  assign("closeness_global", closeness(OD_net, mode = "all", normalized = TRUE),  envir = globalenv())
  # LSOA pop
  lsoa_js <- FROM_GeoJson(url_file_string = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))
  lsoa_feat <- lsoa_js$features
  lsoa_pop <- rep(NA,length(lsoa_feat))
  lsoa_names <- rep(NA,length(lsoa_feat))
  for(i in 1:length(lsoa_feat)){
    lsoa_pop[i] <- lsoa_feat[[i]]$properties$Pop20
    lsoa_names[i] <- lsoa_feat[[i]]$properties$LSOA11CD
  }
  assign("pop_lsoa_global", data.frame(LSOA11CD = lsoa_names, pop = lsoa_pop),  envir = globalenv())
}

# area_name and date must point to one of the .csv files on Azure
loadArea <- function(name,date,folderIn,farea,country = "England"){
  fileName <- paste("pop_",name,"_",date,".csv",sep = "")
  if(!(file.exists(file.path(folderIn,farea,fileName)))){
    url <- paste("https://ramp0storage.blob.core.windows.net/countydata-v2-1/",country,"/",date,"/",fileName,".gz",sep = "")
    download.file(url,file.path(folderIn,farea,paste(fileName,".gz",sep = "")))
    gunzip(file.path(folderIn,farea,paste(fileName,".gz",sep = "")))
  }
  res <- read.csv(file.path(folderIn,farea,fileName))
  return(res)
}

#####🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠#####
# For testing
area_name = "west-yorkshire"
scale = "LSOA11CD"
scale = "MSOA11CD"
scale = "LAD20CD"
data = dataWY
#####🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠#####

# area_name and date must point to one of the .csv files on Azure
# Min scale is LSOA due to source control data
# Taken from global environment: lu, betweenness_global, closeness_global, pop_lsoa_global
prepareLabels <- function(area_name, date, scale = c("LSOA11CD","MSOA11CD","LAD20CD")){
  scale <- match.arg(scale)
  ref <- which(lu$AzureRef == area_name)
  lu_area <- data.frame(lu[ref,c("LSOA11CD","MSOA11CD","LAD20CD")])
  lu_area <- lu_area[!duplicated(lu_area),]
  lu_area <- merge(lu_area, pop_lsoa_global, by.x = "LSOA11CD", by.y = "LSOA11CD", all.x = T)
  list_area <- unique(lu_area[,scale])
  labels_area <- data.frame(area = list_area, closeness = NA, betweenness = NA, distHPD = NA,
                          popDens = NA, medAge = NA, deprivation = NA)
  colnames(labels_area)[1] <- scale
  # Centrality
  if(scale == "LSOA11CD"){
    print("OD data is at MSOA scale, using parent MSOA")
    for(i in 1:length(list_area)){
      labels_area$betweenness[i] <- betweenness_global[names(betweenness_global) == lu_area$MSOA11CD[i]] * 100000
      labels_area$closeness[i] <- closeness_global[names(closeness_global) == lu_area$MSOA11CD[i]] * 10
    }
  }else if(scale == "MSOA11CD"){
    for(i in 1:length(list_area)){
      labels_area$betweenness[i] <- betweenness_global[names(betweenness_global) == list_area[i]] * 100000
      labels_area$closeness[i] <- closeness_global[names(closeness_global) == list_area[i]] * 10
    }
  }else if(scale == "LAD20CD"){
    print("OD data is at MSOA scale, using population weighted averages")
    for(i in 1:length(list_area)){
      ref <- unique(lu_area$MSOA11CD[lu_area$LAD20CD == list_area[i]])
      betw <- rep(NA,length(ref))
      clos <- rep(NA,length(ref))
      pops <- rep(NA,length(ref))
      for(j in 1:length(ref)){
        betw[j] <- betweenness_global[names(betweenness_global) == ref[j]] * 10000
        clos[j] <- closeness_global[names(closeness_global) == ref[j]] * 10
        pops[j] <- sum(lu_area$pop[lu_area$MSOA11CD == ref[j]])
      }
      labels_area$betweenness[i] <- sum(betw * pops) / sum(pops)
      labels_area$closeness[i] <- sum(clos * pops) / sum(pops)
    }
  }
  # Median age
  data <- loadArea(area_name,date)
  median_age <- rep(NA,length(list_area))
  for(i in 1:length(list_area)){
    ref <- lu_area$OA11CD[lu_area[,scale] == list_area[i]]
    median_age[i] <- median(data$age[data$OA11CD %in% ref])
  }
  labels_area$medAge <- median_age
  # Pop density and distace to nearest highest density area
  
  
  return(labels_area)
}




### Shapefile -> pop density and distance to nearest highest density area
download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/MSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson"))
msoa_js <- FROM_GeoJson(url_file_string = file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson"))

msoa_feat <- msoa_js$features
msoa_pop <- rep(NA,length(msoa_feat))
msoa_names <- rep(NA,length(msoa_feat))
msoa_areas <- rep(NA,length(msoa_feat))
for(i in 1:length(msoa_feat)){
  msoa_pop[i] <- msoa_feat[[i]]$properties$PopCount
  msoa_names[i] <- msoa_feat[[i]]$properties$MSOA11CD
  msoa_areas[i] <- msoa_feat[[i]]$properties$Shape_Area
}
msoa_areas <- msoa_areas / 1000000

for(i in 1:nrow(labels_WY)){
  labels_WY$popDens[i] <- msoa_pop[msoa_names == labels_WY$MSOA11CD[i]] / msoa_areas[msoa_names == labels_WY$MSOA11CD[i]] 
}

max(labels_WY$popDens)
sort(labels_WY$popDens, decr = T)

which.max(labels_WY$popDens)

# For now, strictly distance to highest dens area <--- CHANGE TO SOMETHING MORE FINE !!!!
# PB: WEIRD INABILITY TO OPEN GEOJSON WITH OGR, USING AVERAGE CENTROID FROM OA INSTEAD

x_ref <- mean(dataWY$lng[dataWY$MSOA11CD == labels_WY$MSOA11CD[which.max(labels_WY$popDens)]])
y_ref <- mean(dataWY$lat[dataWY$MSOA11CD == labels_WY$MSOA11CD[which.max(labels_WY$popDens)]])
for(i in 1:nrow(labels_WY)){
  x <- mean(dataWY$lng[dataWY$MSOA11CD == labels_WY$MSOA11CD[i]])
  y <- mean(dataWY$lat[dataWY$MSOA11CD == labels_WY$MSOA11CD[i]])
  labels_WY$distHPD[i] <- distHaversine(c(x_ref,y_ref),c(x,y))
}


### Index of deprivation (pop weighted average rank <---- FIND BETTER !!?!)

# Download from gov.uk

url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
download.file(url = url, destfile = file.path(folderIn,fdl,"deprivation.xlsx"))

# Pop for weighted average
download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/LSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))
lsoa_js <- FROM_GeoJson(url_file_string = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))

lsoa_feat <- lsoa_js$features
lsoa_pop <- rep(NA,length(lsoa_feat))
lsoa_names <- rep(NA,length(lsoa_feat))
for(i in 1:length(lsoa_feat)){
  lsoa_pop[i] <- lsoa_feat[[i]]$properties$Pop20
  lsoa_names[i] <- lsoa_feat[[i]]$properties$LSOA11CD
}

depriv <- read_excel(file.path(folderIn,fdl,"deprivation.xlsx"), sheet = 2)

for(i in 1:nrow(labels_WY)){
  msoa <- labels_WY$MSOA11CD[i]
  lsoas <- unique(lu$LSOA11CD[lu$MSOA11CD == msoa])
  pops <- rep(NA, length(lsoas))
  deprivs <- rep(NA,length(lsoas))
  for(j in 1:length(lsoas)){
    deprivs[j] <- depriv$`Index of Multiple Deprivation (IMD) Rank`[depriv$`LSOA code (2011)` == lsoas[j]]
    pops[j] <- lsoa_pop[lsoa_names == lsoas[j]]
  }
  labels_WY$deprivation[i] <- sum(deprivs * pops) / sum(pops)
}

i = 1
j = 1


write.table(labels_WY,file.path(folderIn,"labels_WY.csv"),sep = ",",row.names = F)


####################
####################
####### SHAP #######
####################
####################


library(xgboost)
library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)
#> The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)
#>      none     lstat         rm       dis      indus
#> 1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
#> 2: 22.446 0.1671901 -0.7088401 0.9689005  0.3786871
#> 3: 22.446 5.9888022  5.5450858 0.5660134 -1.4304351
#> 4: 22.446 8.2142204  0.7507572 0.1893366  1.8298304
#> 5: 22.446 0.5059898  5.6875103 0.8432238  2.2471150
#> 6: 22.446 1.9929673 -3.6001958 0.8601984  3.1510531

# Plot the resulting explanations for observations 1 and 6
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))

# Use the Gaussian approach
explanation_gaussian <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6
plot(explanation_gaussian, plot_phi0 = FALSE, index_x_test = c(1, 6))

# Use the Gaussian copula approach
explanation_copula <- explain(
  x_test,
  approach = "copula",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_copula, plot_phi0 = FALSE, index_x_test = c(1, 6))

# Use the conditional inference tree approach
explanation_ctree <- explain(
  x_test,
  approach = "ctree",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding 
# the no-covariate effect
plot(explanation_ctree, plot_phi0 = FALSE, index_x_test = c(1, 6))


###


#######
####### Test of SHAP on WY data
#######


# Predictors
labels_WY

# Predictions
testWY_sal <- moments("incomeH",dataWYnotNA,scale = "MSOA11CD",lu)

# Check MSOAs are ordered properlyt
labels_WY$MSOA11CD == testWY$area

# Training set
x_train <- as.matrix(labels_WY[-1:-20,2:7])

# Results: Order 1-4: mean, sd, skewness, kurtosis
y_train_1 <- testWY_sal[-1:-20, "mean"]
y_train_2 <- testWY_sal[-1:-20, "sd"]
y_train_3 <- testWY_sal[-1:-20, "skewness"]
y_train_4 <- testWY_sal[-1:-20, "kurtosis"]

# Test set
x_test <- as.matrix(labels_WY[1:20, 2:7])

### Mean

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train_1,
  nround = 20,
  verbose = FALSE
)
# Explanation
explainer <- shapr(x_train, model)
explanation_1 <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = mean(y_train_1)
)

###
model <- xgboost(
  data = x_train,
  label = y_train_2,
  nround = 20,
  verbose = FALSE
)
# Explanation
explainer <- shapr(x_train, model)
explanation_2 <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = mean(y_train_2)
)

###
model <- xgboost(
  data = x_train,
  label = y_train_3,
  nround = 20,
  verbose = FALSE
)
# Explanation
explainer <- shapr(x_train, model)
explanation_3 <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = mean(y_train_3)
)

###
model <- xgboost(
  data = x_train,
  label = y_train_4,
  nround = 20,
  verbose = FALSE
)
# Explanation
explainer <- shapr(x_train, model)
explanation_4 <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = mean(y_train_4)
)

# Printing the Shapley values for the test data.
print(explanation_1$dt)

# Plot the resulting explanations for observations 1 and 6
plot(explanation_1, plot_phi0 = FALSE, index_x_test = c(1:4))

avg1 <- colSums(abs(explanation_1$dt[,2:7]))/20
avg2 <- colSums(abs(explanation_2$dt[,2:7]))/20
avg3 <- colSums(abs(explanation_3$dt[,2:7]))/20
avg4 <- colSums(abs(explanation_4$dt[,2:7]))/20

res <- rbind(avg1,avg2,avg3,avg4)
row.names(res) <- c("Mean", "Sd", "Skewness", "Kurotsis")
res <- t(res)

for(i in 1:4){
  res[,i] <- res[,i]/max(res[,i])
}

image(1:ncol(res), 1:nrow(res), t(res), axes = FALSE, main = "West Yorshire salaries; relative importance norm. per column")
axis(1, 1:ncol(res), colnames(res))
axis(2, 1:nrow(res), rownames(res))

image(res, axes = FALSE)



