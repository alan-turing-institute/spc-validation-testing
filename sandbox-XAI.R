
### Label clustering. Possible labels:
#     - [ ] Centrality measures within transportation network
#     - [ ] “Geographic centrality” aka distance from highest density areas
#     - [ ] Population density (better than categorical rural urban -- see def pb and World Pop 1000 inh./sqkm)
#     - [ ] Age; ethnicity? (<- also pb is categorical; maybe keep numerical initially)
#     - [ ] Index of deprivation
#     - [ ] Life satisfaction?
#
# SUPPORTED MODELS: stats::lm; stats::glm; ranger::ranger; mgcv::gam; xgboost::xgb.train
# Custom model possible, but implementation looks wonky

# Note: categorical supported (see full doc online: https://cran.r-project.org/web/packages/shapr/vignettes/understanding_shapr.html), but more restrictive
# Also, no NA

## For initial analysis, let's compute for each MSOA:
#   - NUM: Closeness centrality from ONS OD matrix (Azure)
#   - NUM: Betweenness centrality from ONS OD matrix (Azure)
#   - NUM: Distance from nearest local high density area (?)
#   - NUM: Population density (Compute from Shp)
#   - NUM: Median age (Compute from SPC - Azure)
#   - NUM: Index of deprivation (2019; https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)
#
# MODEL: xgboost (Ref: https://xgboost.readthedocs.io/en/stable/tutorials/model.html)

## Define:
#   - "Local high density area"


### METHODS:
#     - SHAP <- Prob best?
#             Four approaches: "empirical" (default), "gaussian", "copula", or "ctree" (gausian and copula require gaussian assumptions; only ctree for cat var; no independence assumption vs original kernel approach).
#     - LIME <- Similar, less theoretically sound (supposedly)
#     - TREPAN? (model specific)


#########################
#########################
####### Test data #######
#########################
#########################


library(igraph)
library(readxl)
library(geojsonR)
library(geosphere)

# We use West Yorshire as a test area
MSOAs_WY <- unique(lu$MSOA11CD[lu$AzureRef == "west-yorkshire"])

labels_WY <- data.frame(MSOA11CD = MSOAs_WY, closeness = NA, betweenness = NA, distHPD = NA,
                        popDens = NA, medAge = NA, deprivation = NA)

### OD matrix from Azure -> closeness and betweenness
download.file(url = "https://ramp0storage.blob.core.windows.net/nationaldata/commutingOD.gz", destfile = file.path(folderIn,fdl,"commutingOD.csv.gz"))
gunzip(file.path(folderIn,fdl,"commutingOD.csv.gz"))
OD <- read.csv(file.path(folderIn,fdl,"commutingOD.csv"), header = T)

OD_wide <- pivot_wider(OD, names_from = HomeMSOA, values_from = Total_Flow)
OD_wide <- as.matrix(OD_wide)
OD_wide_prep <- OD_wide[,2:ncol(OD_wide)]
rownames(OD_wide_prep) <- OD_wide[,1]

ncol(OD_wide_prep)
nrow(OD_wide_prep)

M <- matrix(0:8,ncol = 3, nrow = 3)
M_net <- graph_from_adjacency_matrix(M, mode = "directed", weighted = TRUE, diag = TRUE)
plot(M_net)

OD_net <- graph_from_adjacency_matrix(OD_wide_prep, mode = "directed", weighted = TRUE, diag = TRUE)

plot(OD_net)
dev.off()

get.edgelist(OD_net)

e(OD_net)

test <- betweenness(OD_net,directed = TRUE, normalized = TRUE)
#test <- betweenness(OD_net,directed = TRUE, weights = , normalized = TRUE)
between_WY <- test[which(names(test) %in% MSOAs_WY)]

range(between_WY * 100000)

# Arbitrary normalisation so that it looks nice
for(i in 1:nrow(labels_WY)){
  labels_WY$betweenness[i] <- between_WY[names(between_WY) == labels_WY$MSOA11CD[i]] * 100000
}


test <- closeness(OD_net, mode = "all", normalized = TRUE)
close_WY <- test[which(names(test) %in% MSOAs_WY)]

range(close_WY * 10)

for(i in 1:nrow(labels_WY)){
  labels_WY$closeness[i] <- close_WY[names(close_WY) == labels_WY$MSOA11CD[i]] * 10
}


### SPC output -> Median age

i = 1 
for(i in 1:nrow(labels_WY)){
  labels_WY$medAge[i] <- median(dataWY$age[dataWY$MSOA11CD == labels_WY$MSOA11CD[i]])
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


write.table(labels_WY,file.path(folderIn,"labels_WY.csv"),sep = ",")

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
#######
#######