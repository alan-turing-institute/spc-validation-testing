### TO DO:
#   Check def of betweenness
#   Check def of closeness
#   Refine distance to nearest HPD area
#   Do better than deprivation rank?


##############################
##############################
####### Make test data #######
##############################
##############################


######🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠######
# For testing
#area_name = "west-yorkshire"
#scale = "LSOA11CD"
#scale = "MSOA11CD"
#scale = "LAD20CD"
#date = 2020
#data = dataWY
#dataS <- data
#data <- dataS
#i = 1

#colnames(data)
#head(data)
######🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠######

downloadPrerequisites <- function(folderIn,fdl){
  n <- 0
  # LookUp table
  if(!(file.exists(file.path(folderIn,fdl,"lookUp-GB.csv")))){
    print("Downloading look-up...")
    download.file("https://ramp0storage.blob.core.windows.net/referencedata/lookUp-GB.csv.gz",file.path(folderIn,fdl,"lookUp-GB.csv.gz"))
    gunzip(file.path(folderIn,fdl,"lookUp-GB.csv.gz"))
    n <- n+1
  }
  # OD matrix
  if(!(file.exists(file.path(folderIn,fdl,"commutingOD.csv")))){
    print("Downloading commuting matrix...")
    download.file(url = "https://ramp0storage.blob.core.windows.net/nationaldata/commutingOD.gz", destfile = file.path(folderIn,fdl,"commutingOD.csv.gz"))
    gunzip(file.path(folderIn,fdl,"commutingOD.csv.gz"))
    n <- n+1
  }
  # Pop at LSOA scale
  if(!(file.exists(file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson")))){
    print("Downloading geojson at LSOA scale...")
    download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/LSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))
    n <- n+1
  }
  # Deprivation ranks
  if(!(file.exists(file.path(folderIn,fdl,"deprivation.xlsx")))){
    print("Downloading deprivation ranks from gov.uk...")
    url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
    download.file(url = url, destfile = file.path(folderIn,fdl,"deprivation.xlsx"))
    n <- n+1
  }
  print(paste("Downloaded", n, "new files, skipped", 4-n, "files that already exists", sep = " "))
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
  print("Calculating betweenness centrality, this can take several minutes...")
  assign("betweenness_global", betweenness(OD_net,directed = TRUE, normalized = TRUE), envir = globalenv())
  print("Calculating closeness centrality, this can take several minutes...")
  assign("closeness_global", closeness(OD_net, mode = "all", normalized = TRUE), envir = globalenv())
  # LSOA pop
  print("Loading the heavy geojson file...")
  lsoa_js <- FROM_GeoJson(url_file_string = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))
  print("Almost done...")
  lsoa_feat <- lsoa_js$features
  lsoa_pop <- rep(NA,length(lsoa_feat))
  lsoa_names <- rep(NA,length(lsoa_feat))
  lsoa_areas <- rep(NA,length(lsoa_feat))
  for(i in 1:length(lsoa_feat)){
    lsoa_pop[i] <- lsoa_feat[[i]]$properties$Pop20
    lsoa_names[i] <- lsoa_feat[[i]]$properties$LSOA11CD
    lsoa_areas[i] <- lsoa_feat[[i]]$properties$Shape_Area
  }
  lsoa_areas <- lsoa_areas / 1000000
  assign("popArea_lsoa_global", data.frame(LSOA11CD = lsoa_names, pop = lsoa_pop, area = lsoa_areas), envir = globalenv())
  # Deprivation index
  assign("depriv_global", read_excel(file.path(folderIn,fdl,"deprivation.xlsx"), sheet = 2), envir = globalenv())
  print("Done! Loaded lu, betweenness_global, closeness_global, popArea_lsoa_global, depriv_global into the global environment")
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

# area_name and date must point to one of the .csv files on Azure
# Min scale is LSOA due to source control data
# Taken from global environment: folderIn, farea, lu, betweenness_global, closeness_global, popArea_lsoa_global, depriv_global
prepareLabels <- function(area_name, date, scale = c("LSOA11CD","MSOA11CD","LAD20CD"), data = NULL){
  # Load and trim Azure file (needed for median age)
  if(is.null(data)){
    data <- loadArea(area_name,date,folderIn,farea)
  }
  data <- data[,c("OA11CD","pid","age","lng","lat")]
  data <- merge(data,lu[,c("OA11CD","LSOA11CD","MSOA11CD","LAD20CD")], by.x = "OA11CD", by.y = "OA11CD", all.x = T)
  # Prepare look-up with pop and areas + list of areas at chosen scale
  scale <- match.arg(scale)
  lu_area <- data.frame(lu[which(lu$AzureRef == area_name),c("LSOA11CD","MSOA11CD","LAD20CD")])
  lu_area <- lu_area[!duplicated(lu_area),]
  lu_area <- merge(lu_area, popArea_lsoa_global, by.x = "LSOA11CD", by.y = "LSOA11CD", all.x = T)
  list_area <- unique(lu_area[,scale])
  #
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
        betw[j] <- betweenness_global[names(betweenness_global) == ref[j]] * 100000
        clos[j] <- closeness_global[names(closeness_global) == ref[j]] * 10
        pops[j] <- sum(lu_area$pop[lu_area$MSOA11CD == ref[j]])
      }
      labels_area$betweenness[i] <- sum(betw * pops) / sum(pops)
      labels_area$closeness[i] <- sum(clos * pops) / sum(pops)
    }
  }
  # Median age
  for(i in 1:length(list_area)){
    labels_area$medAge[i] <- median(data$age[data[,scale] == list_area[i]])
  }
  # Pop density and distance to nearest highest density area
  for(i in 1:length(list_area)){
    ref <- lu_area$LSOA11CD[lu_area[,scale] == list_area[i]]
    labels_area$popDens[i] <- sum(lu_area$pop[lu_area$LSOA %in% ref]) / sum(lu_area$area[lu_area$LSOA %in% ref])
  }
  x_ref <- mean(data$lng[data[,scale] == labels_area[which.max(labels_area$popDens),scale]])
  y_ref <- mean(data$lat[data[,scale] == labels_area[which.max(labels_area$popDens),scale]])
  for(i in 1:length(list_area)){
    x <- mean(data$lng[data[,scale] == labels_area[i,scale]])
    y <- mean(data$lat[data[,scale] == labels_area[i,scale]])
    labels_area$distHPD[i] <- distHaversine(c(x_ref,y_ref),c(x,y))
  }
  # Deprivation
  if(scale != "LSOA11CD"){
    print("Deprivation data is at LSOA scale, using population weighted averages")
  }
  for(i in 1:length(list_area)){
    lsoas <- unique(lu$LSOA11CD[lu[,scale] == list_area[i]])
    pops <- rep(NA, length(lsoas))
    deprivs <- rep(NA,length(lsoas))
    for(j in 1:length(lsoas)){
      deprivs[j] <- depriv_global$`Index of Multiple Deprivation (IMD) Rank`[depriv_global$`LSOA code (2011)` == lsoas[j]]
      pops[j] <- lu_area$pop[lu_area$LSOA11CD == lsoas[j]]
    }
    labels_area$deprivation[i] <- sum(deprivs * pops) / sum(pops)
  }
  write.table(labels_area,file.path(folderOut,paste("labels_",area_name,"_",date,"_",scale,".csv",sep = "")),sep = ",",row.names = F)
  print(paste("Done! Saved labels to",file.path(folderOut,paste("labels_",area_name,"_",date,"_",scale,".csv",sep = "")),sep = " "))
  return(labels_area)
}

######🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠######
# For testing
#downloadPrerequisites(folderIn,fdl)
#loadPrerequisites(folderIn,fdl)

#dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)

#test <- prepareLabels("west-yorkshire", 2020, scale = "LSOA11CD", data = NULL)
#test <- prepareLabels("west-yorkshire", 2020, scale = "MSOA11CD", data = NULL)
#test <- prepareLabels("west-yorkshire", 2020, scale = "LAD20CD", data = NULL)
#test <- prepareLabels("west-yorkshire", 2020, scale = "LSOA11CD", data = dataWY)
######🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠######


####################
####################
####### SHAP #######
####################
####################


# Taken from global environment: folderOut
loadLabels <- function(area_name,date, scale){
  if(file.exists(file.path(folderOut,paste("labels_",area_name,"_",date,"_",scale,".csv",sep = "")))){
    print(paste("Loading",file.path(folderOut,paste("labels_",area_name,"_",date,"_",scale,".csv",sep = "")),sep = " "))
    ret <- read.csv(file.path(folderOut,paste("labels_",area_name,"_",date,"_",scale,".csv",sep = "")))
    return(ret)
  }else{
    print(paste("File",file.path(folderOut,paste("labels_",area_name,"_",date,"_",scale,".csv",sep = "")),"is missing",sep = " "))
  }
}

# Extract first four moments for one distribution as vector
findmoments <- function(dstrb, graph = TRUE){
  step <- descdist(dstrb, discrete = FALSE, graph = graph)
  return(c(step$mean,step$sd,step$skewness,step$kurtosis))
}

######🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠######
# For testing
#area_name = "west-yorkshire"
#scale = "LSOA11CD"
#scale = "MSOA11CD"
#scale = "LAD20CD"
#date = 2020
#variable_name = "incomeH"
#aggregation = "mean"
#ntrain = 2

#data = dataWY
#dataS <- data
#data <- dataS
#i = 1

#colnames(data)
#head(data)
#unique(dataS$LAD20CD)

#test <- runSHAP(area_name, date, scale, "incomeH", 3)
######🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠######

# Taken from global environment: folderIn, farea, lu
runSHAP <- function(area_name, date, scale = c("LSOA11CD","MSOA11CD","LAD20CD"), variable_name, ntrain, data = NULL, predictors = NULL){
  scale <- match.arg(scale)
  # Load and trim Azure file
  if(is.null(data)){
    data <- loadArea(area_name,date,folderIn,farea)
  }
  data <- merge(data,lu[,c("OA11CD","LSOA11CD","MSOA11CD","LAD20CD")], by.x = "OA11CD", by.y = "OA11CD", all.x = T)
  predictions <- data[,c(variable_name,scale)]
  # Load predictors
  if(is.null(predictors)){
    predictors <- loadLabels(area_name,date,scale)
  }
  # Remove NA values and check area lists match
  save_area_list <- unique(predictions[,scale])
  save_pred_list <- unique(predictors[,scale])
  #
  predictions <- predictions[complete.cases(predictions), ]
  new_area_list <- unique(predictions[,scale])
  removed_area_NA <- setdiff(save_area_list, new_area_list)
  #
  predictors <- predictors[complete.cases(predictors), ]
  new_pred_list <- unique(predictors[,scale])
  removed_pred_NA <- setdiff(save_pred_list, new_pred_list)
  #
  final_area_list <- sort(intersect(new_area_list,new_pred_list), decreasing = FALSE)
  l <- length(final_area_list)
  removed_dont_match <- setdiff(new_area_list, new_pred_list)
  #
  if(length(removed_area_NA) > 0){
    print(paste("Removed",removed_area_NA,"due to all", variable_name,"are NA inside the area", sep = " "))
  }
  if(length(removed_area_NA) > 0){
    print(paste("Removed",removed_pred_NA,"due to some NA values within the predictors", sep = " "))
  }
  if(length(removed_dont_match) > 0){
    print(paste("Removed",removed_dont_match,"due to these areas not existing in one of the datasets", sep = " "))
  }
  # 
  predictors <- predictors[predictors[,scale] %in% final_area_list,]
  predictors <- predictors[order(predictors[,scale]),]
  row.names(predictors) <- 1:nrow(predictors)
  # Get predictions (moments)
  print("Finished loading the data. Calculating predictions...")
  moments <- data.frame(area = rep(NA,l), mean = NA, sd = NA, skewness = NA, kurtosis = NA)
  colnames(moments)[1] <- scale
  for(i in 1:l){
    ref <- which(predictions[,scale] %in% new_area_list[i])
    moments[i,1] <- new_area_list[i]
    moments[i,2:5] <- findmoments(predictions[ref,variable_name], graph = FALSE)
  }
  moments <- moments[order(moments[,scale]),]
  row.names(moments) <- 1:nrow(moments)
  # Run SHAP
  print("Running SHAP...")
  testSet <- sample(1:l, ntrain)
  x_train <- as.matrix(predictors[-testSet,2:ncol(predictors)])
  y_train_mean <- moments[-testSet, "mean"]
  y_train_sd <- moments[-testSet, "sd"]
  y_train_skew <- moments[-testSet, "skewness"]
  y_train_kurt <- moments[-testSet, "kurtosis"]
  x_test <- as.matrix(predictors[testSet, 2:ncol(predictors)])
  model <- xgboost(data = x_train,label = y_train_mean,nround = 20,verbose = FALSE)
  explainer <- shapr(x_train, model)
  explanation_mean <- explain(x_test,approach = "empirical",explainer = explainer,prediction_zero = mean(y_train_mean))
  model <- xgboost(data = x_train,label = y_train_sd,nround = 20,verbose = FALSE)
  explainer <- shapr(x_train, model)
  explanation_sd <- explain(x_test,approach = "empirical",explainer = explainer,prediction_zero = mean(y_train_sd))
  model <- xgboost(data = x_train,label = y_train_skew,nround = 20,verbose = FALSE)
  explainer <- shapr(x_train, model)
  explanation_skew <- explain(x_test,approach = "empirical",explainer = explainer,prediction_zero = mean(y_train_skew))
  model <- xgboost(data = x_train,label = y_train_kurt,nround = 20,verbose = FALSE)
  explainer <- shapr(x_train, model)
  explanation_kurt <- explain(x_test,approach = "empirical",explainer = explainer,prediction_zero = mean(y_train_kurt))
  return(list(explanation_mean$dt,explanation_sd$dt,explanation_skew$dt,explanation_kurt$dt,c(area_name, date, scale, variable_name)))
}

plotSHAP <- function(resSHAP, folderOut, fplot){
  resMean <- resSHAP[[1]]
  resSd <- resSHAP[[2]]
  resSkew <- resSHAP[[3]]
  resKurt <- resSHAP[[4]]
  info <- resSHAP[[5]]
  ### Plot means
  avg1 <- colSums(resMean[,2:7])/nrow(resMean)
  avg2 <- colSums(resSd[,2:7])/nrow(resSd)
  avg3 <- colSums(resSkew[,2:7])/nrow(resSkew)
  avg4 <- colSums(resKurt[,2:7])/nrow(resKurt)
  res <- rbind(avg1,avg2,avg3,avg4)
  row.names(res) <- c("Mean", "Sd", "Skewness", "Kurotsis")
  res <- t(res)
  ### Plot means norm by moment column
  avg11 <- colSums(resMean[,2:ncol(resMean)])/nrow(resMean)
  avg21 <- colSums(resSd[,2:ncol(resSd)])/nrow(resSd)
  avg31 <- colSums(resSkew[,2:ncol(resSkew)])/nrow(resSkew)
  avg41 <- colSums(resKurt[,2:ncol(resKurt)])/nrow(resKurt)
  res2 <- rbind(avg11,avg21,avg31,avg41)
  for(i in 1:4){
    res2[,i] <- res2[,i]/max(res2[,i])
  }
  row.names(res2) <- c("Mean", "Sd", "Skewness", "Kurotsis")
  res2 <- t(res2)
  #
  png(file=file.path(folderOut,fplot,paste(info[1], info[2], info[3], info[4], "feature_importance.png", sep = "-")), width=1250, height=625)
  par(mfrow = c(1,2))
  image(1:ncol(res), 1:nrow(res), t(res), axes = FALSE, col = hcl.colors(12, "Plasma", rev = TRUE),
        main = paste(info[1], info[2], info[3], info[4], "; average importance", sep = " "))
  axis(1, 1:ncol(res), colnames(res))
  axis(2, 1:nrow(res), rownames(res))
  image(1:ncol(res2), 1:nrow(res2), t(res2), axes = FALSE, col = hcl.colors(12, "Plasma", rev = TRUE),
        main = paste(info[1], info[2], info[3], info[4], "; average importance norm. p. column", sep = " "))
  axis(1, 1:ncol(res2), colnames(res2))
  axis(2, 1:nrow(res2), rownames(res2))
  dev.off()
  print(paste("Plot saved in ", file.path(folderOut,fplot), "/", sep = ""))
}

ggplotSHAP2 <- function(resSHAP, folderOut, fplot){
  resMean <- resSHAP[[1]]
  resSd <- resSHAP[[2]]
  resSkew <- resSHAP[[3]]
  resKurt <- resSHAP[[4]]
  info <- resSHAP[[5]]
  ### Plot means
  avg1 <- colSums(resMean[,2:7])/nrow(resMean)
  avg2 <- colSums(resSd[,2:7])/nrow(resSd)
  avg3 <- colSums(resSkew[,2:7])/nrow(resSkew)
  avg4 <- colSums(resKurt[,2:7])/nrow(resKurt)
  res <- rbind(avg1,avg2,avg3,avg4)
  row.names(res) <- c("Mean", "Sd", "Skewness", "Kurotsis")
  res <- t(res)
  ### Plot means norm by moment column
  avg11 <- colSums(resMean[,2:ncol(resMean)])/nrow(resMean)
  avg21 <- colSums(resSd[,2:ncol(resSd)])/nrow(resSd)
  avg31 <- colSums(resSkew[,2:ncol(resSkew)])/nrow(resSkew)
  avg41 <- colSums(resKurt[,2:ncol(resKurt)])/nrow(resKurt)
  res2 <- rbind(avg11,avg21,avg31,avg41)
  for(i in 1:4){
    res2[,i] <- res2[,i]/max(res2[,i])
  }
  row.names(res2) <- c("Mean", "Sd", "Skewness", "Kurotsis")
  res2 <- t(res2)
  #
  png(file=file.path(folderOut,fplot,paste(info[1], info[2], info[3], info[4], "feature_importance_gg.png", sep = "-")), width=1250, height=625)
  par(mfrow = c(1,2))
  image(1:ncol(res), 1:nrow(res), t(res), axes = FALSE, col = hcl.colors(7, "Blue-Red", rev = TRUE),
        main = paste(info[1], info[2], info[3], info[4], "; average importance", sep = " "))
  axis(1, 1:ncol(res), colnames(res))
  axis(2, 1:nrow(res), rownames(res))
  image(1:ncol(res2), 1:nrow(res2), t(res2), axes = FALSE, col = hcl.colors(7, "Blue-Red", rev = TRUE),
        main = paste(info[1], info[2], info[3], info[4], "; average importance norm. p. column", sep = " "))
  axis(1, 1:ncol(res2), colnames(res2))
  axis(2, 1:nrow(res2), rownames(res2))
  dev.off()
  print(paste("Plot saved in ", file.path(folderOut,fplot), "/", sep = ""))
}
