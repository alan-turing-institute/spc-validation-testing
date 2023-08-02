### Labels for each geography:
#   - NUM: Closeness centrality from ONS OD matrix (Azure)
#   - NUM: Betweenness centrality from ONS OD matrix (Azure)
#   - NUM: Distance from nearest local high density area (?)
#   - NUM: Population density (Compute from Shp)
#   - NUM: Median age (Compute from SPC - Azure)
#   - NUM: Index of deprivation (2019; https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)

### METHODS: xgboost, SHAP

# <area_name> and <date> must point to one of the .csv files on Azure
# Min scale is LSOA due to source control data (scale = c("LSOA11CD", "MSOA11CD", "LAD20CD"))
# Check the following are loaded into the global environment after running <loadPrerequisites>: 
#   folderIn, farea, lu, betweenness_global, closeness_global, popArea_lsoa_global, depriv_global

library(igraph)
library(readxl)
library(geojsonR)
library(geosphere)
library(tidyr)
library(xgboost)
library(shapr)
library(fitdistrplus)

folderIn <- "Data"
fdl <- "dl"
farea <- "area"
folderOut <- "Output"
fplot <- "plots"

set.seed(18061815)

source('functions.R')

downloadPrerequisites(folderIn,fdl)
loadPrerequisites(folderIn,fdl)


########################################################################

#dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)

test <- prepareLabels("west-yorkshire", 2020, scale = "LSOA11CD", data = NULL)
test <- prepareLabels("west-yorkshire", 2020, scale = "MSOA11CD", data = NULL)
test <- prepareLabels("west-yorkshire", 2020, scale = "LAD20CD", data = NULL)
#test <- prepareLabels("west-yorkshire", 2020, scale = "LSOA11CD", data = dataWY)

test <- runSHAP(area_name, date, scale, "incomeH", 3)






### Plot: absolute impact (fairly useless)
avg_mean <- colSums(abs(explanation_mean$dt[,2:ncol(predicors)]))/20
avg_sd <- colSums(abs(explanation_sd$dt[,2:ncol(predicors)]))/20
avg_skew <- colSums(abs(explanation_skew$dt[,2:ncol(predicors)]))/20
avg_kurt <- colSums(abs(explanation_kurt$dt[,2:ncol(predicors)]))/20

res <- rbind(avg1,avg2,avg3,avg4)
row.names(res) <- c("Mean", "Sd", "Skewness", "Kurotsis")
res <- t(res)

image(1:ncol(res), 1:nrow(res), t(res), axes = FALSE, main = "West Yorshire salaries; absolute importance")
axis(1, 1:ncol(res), colnames(res))
axis(2, 1:nrow(res), rownames(res))

### Plot without abs
avg1 <- colSums(explanation_1$dt[,2:7])/20
avg2 <- colSums(explanation_2$dt[,2:7])/20
avg3 <- colSums(explanation_3$dt[,2:7])/20
avg4 <- colSums(explanation_4$dt[,2:7])/20

res <- rbind(avg1,avg2,avg3,avg4)
row.names(res) <- c("Mean", "Sd", "Skewness", "Kurotsis")
res <- t(res)

image(1:ncol(res), 1:nrow(res), t(res), axes = FALSE, col = hcl.colors(12, "Plasma", rev = TRUE),
      main = "West Yorshire salaries; average importance")
axis(1, 1:ncol(res), colnames(res))
axis(2, 1:nrow(res), rownames(res))


### Plot without abs and norm by feature
avg1 <- colSums(explanation_1$dt[,2:7])/20
avg2 <- colSums(explanation_2$dt[,2:7])/20
avg3 <- colSums(explanation_3$dt[,2:7])/20
avg4 <- colSums(explanation_4$dt[,2:7])/20

res <- rbind(avg1,avg2,avg3,avg4)
row.names(res) <- c("Mean", "Sd", "Skewness", "Kurotsis")
res <- t(res)

for(i in 1:4){
  res[,i] <- res[,i]/max(res[,i])
}

image(1:ncol(res), 1:nrow(res), t(res), axes = FALSE, col = hcl.colors(12, "Plasma", rev = TRUE),
      main = "West Yorshire salaries; average importance normalised per column")
axis(1, 1:ncol(res), colnames(res))
axis(2, 1:nrow(res), rownames(res))


