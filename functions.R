################################
################################
####### Create test data #######
################################
################################


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
  if(!(file.exists(file.path(folderIn,fdl,"wf01bew_oa.zip")))){
    print("Downloading commuting matrix...")
    download.file(url = "https://www.nomisweb.co.uk/output/census/2011/wf01bew_oa.zip", destfile = file.path(folderIn,fdl,"wf01bew_oa.zip"))
    unzip(file.path(folderIn,fdl,"wf01bew_oa.zip"), exdir = file.path(folderIn,fdl))
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
  # Deprivation scores
  if(!(file.exists(file.path(folderIn,fdl,"deprivation_scores.xlsx")))){
    print("Downloading deprivation scores from gov.uk...")
    url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833978/File_5_-_IoD2019_Scores.xlsx"
    download.file(url = url, destfile = file.path(folderIn,fdl,"deprivation_scores.xlsx"))
    n <- n+1
  }
  print(paste("Downloaded", n, "new file(s); skipped", 5-n, "file(s) that already exist", sep = " "))
}

######🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠######
###### Check centrality meaning ######
#M <- matrix(c(1,0,2,3,0,3,0,2,3,1,1,3,0,2,1,1), ncol = 4)
#M
#n <- graph_from_adjacency_matrix(M, mode = "directed",weight = TRUE)
#n2 <- graph_from_adjacency_matrix(M, mode = "undirected",weight = TRUE)
#plot(n)
#plot(n2)

#betweenness(n,directed = TRUE, weights = 1/edge_attr(n, "weight"), normalized = FALSE)
#betweenness(n,directed = TRUE, weights = edge_attr(n, "weight"), normalized = FALSE)
#betweenness(n,directed = TRUE, weights = rep(1,length(E(n))), normalized = FALSE)
#betweenness(n,directed = FALSE, weights = rep(1,length(E(n))), normalized = FALSE)
#betweenness(n,directed = TRUE, normalized = FALSE)
######🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠🛠######


createOD <- function(folderIn,fdl,fproc,scale = c("LSOA11CD", "MSOA11CD")){
  scale = match.arg(scale)
  if(file.exists(file.path(folderIn,fproc,paste("commutingOD_",scale,".csv",sep = "")))){
    print(paste("Loading OD at ", scale," scale from save", sep = ""))
    OD <- read.csv(file.path(folderIn,fproc,paste("commutingOD_",scale,".csv",sep = "")))
  }else{
    print(paste("Creating OD and saving to file in ", folderIn, "/", fproc, "/", sep = ""))
    OD <- read.csv(file.path(folderIn,fdl,"wf01bew_oa.csv"), header = T)
    print("Raw data loaded")
    lu2 <- lu[,c("OA11CD",scale)]
    temp <- merge(OD,lu2,by.x="Area.of.workplace",by.y="OA11CD", all.x = TRUE)
    temp <- temp[!is.na(temp[,scale]),c(2,4,3)]
    colnames(temp)[2] <- c("Destination")
    temp <- merge(temp,lu2,by.x="Area.of.usual.residence",by.y="OA11CD",all.x = TRUE)
    temp <- temp[!is.na(temp[,scale]),c(4,2,3)]
    colnames(temp)[1] <- c("Home")
    print("Data sorted, aggregating...")
    OD <- aggregate(temp$count, by = list(temp$Home,temp$Destination),FUN=sum)
    colnames(OD) <- c("Home","Destination","Count")
    OD <- OD[order(OD$Home,OD$Destination),]
    row.names(OD) <- 1:nrow(OD)
    print("Writing...")
    write.table(OD,file.path(folderIn,fproc,paste("commutingOD_",scale,".csv",sep = "")),sep = ",",row.names = F)
  }
  return(OD)
}

createCentrality <- function(folderIn,fproc,scale = c("LSOA11CD", "MSOA11CD")){
  betwPath <- file.path(folderIn,fproc,paste("betweenness_",scale,".csv",sep = ""))
  closPath <- file.path(folderIn,fproc,paste("closeness_",scale,".csv",sep = ""))
  # Check if saves exist and load them if true
  if(file.exists(betwPath) & file.exists(closPath)){
    print("Loading closeness and betweenness centrality data from existing save files...")
    betw0 <- read.csv(betwPath)
    betw <- betw0$betweenness
    names(betw) <- betw0$area
    clos <- read.csv(closPath)
    clos_all <- clos$closeness_all
    clos_in <- clos$closeness_in
    clos_out <- clos$closeness_out
    names(clos_all) <- clos$area
    names(clos_in) <- clos$area
    names(clos_out) <- clos$area
    OD_net <- read_graph(paste(folderIn,"/",fproc,"/","OD_net_",scale,".txt",sep = ""), format = "pajek")
    assign("betweenness_global", betw, envir = globalenv())
    assign("closeness_global_all", clos_all, envir = globalenv())
    assign("closeness_global_in", clos_in, envir = globalenv())
    assign("closeness_global_out", clos_out, envir = globalenv())
    assign("OD_net_global",OD_net, envir = globalenv())
  }else{
    OD <- createOD(folderIn,fdl,fproc,scale)
    if(length(grep("E",unique(lu[,scale]))) +  length(grep("W",unique(lu[,scale]))) > length(unique(OD$Home))){
      warning("Some areas are missing from the OD matrix as home locations")
    }
    if(length(grep("E",unique(lu[,scale]))) +  length(grep("W",unique(lu[,scale]))) > length(unique(OD$Destination))){
      warning("Some areas are missing from the OD matrix as destinations")
    }
    print("Preparing network...")
    OD_wide <- pivot_wider(OD, names_from = Home, values_from = Count)
    OD_wide[is.na(OD_wide)] <- 0
    OD_wide <- as.matrix(OD_wide)
    OD_wide_prep <- matrix(as.numeric(OD_wide[,2:ncol(OD_wide)]), ncol = ncol(OD_wide) - 1)
    rownames(OD_wide_prep) <- OD_wide[,1]
    colnames(OD_wide_prep) <- colnames(OD_wide)[2:ncol(OD_wide)]
    OD_wide_prep <- OD_wide_prep[order(rownames(OD_wide_prep)),order(colnames(OD_wide_prep))]
    OD_wide_prep <- t(OD_wide_prep)
    if(length(which(rownames(OD_wide_prep) != colnames(OD_wide_prep))) > 1){
      stop("OD matrix isn't square")
    }
    OD_net <- graph_from_adjacency_matrix(OD_wide_prep, mode = "directed", weighted = TRUE, diag = TRUE)
    edge_attr(OD_net, "weight") <- 1/edge_attr(OD_net, "weight")
    print("Writing network to the global environment and creating a save file...")
    V(OD_net)$id <- as.character(V(OD_net)$name)
    write_graph(OD_net, file = paste(folderIn,"/",fproc,"/","OD_net_",scale,".txt",sep = ""), format = "pajek")
    assign("OD_net_global",OD_net, envir = globalenv())
    print("Calculating betweenness centrality, this can take several minutes...")
    betw <- betweenness(OD_net,directed = TRUE, normalized = FALSE)
    assign("betweenness_global", betw, envir = globalenv())
    write.table(data.frame(area = names(betw), betweenness = betw),betwPath,sep = ",",row.names = F)
    print("Calculating closeness centrality, this can take several minutes...")
    print("1/3: undirected")
    clos_all <- closeness(OD_net, mode = "all", normalized = FALSE)
    print("2/3: inwards directed")
    clos_in <- closeness(OD_net, mode = "in", normalized = FALSE)
    print("3/3: outwards directed")
    clos_out <- closeness(OD_net, mode = "out", normalized = FALSE)
    assign("closeness_global_all", clos_all, envir = globalenv())
    assign("closeness_global_in", clos_in, envir = globalenv())
    assign("closeness_global_out", clos_out, envir = globalenv())
    write.table(data.frame(area = names(clos_all), closeness_all = clos_all, closeness_in = clos_in, closeness_out = clos_out),closPath,sep = ",",row.names = F)
    print(paste("Saved for future use to ", folderIn, "/", fproc, sep = ""))
  }
}

loadPrerequisites <- function(folderIn,fdl,fproc,scale = c("LSOA11CD", "MSOA11CD")){
  scale = match.arg(scale)
  ### LookUp table
  print("Loading look-up table...")
  assign("lu", read.csv(file.path(folderIn,fdl,"lookUp-GB.csv")), envir = globalenv())
  ### Centrality from OD matrix as network
  createCentrality(folderIn,fproc,scale)
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
  # Deprivation indices
  assign("depriv_global", read_excel(file.path(folderIn,fdl,"deprivation.xlsx"), sheet = 2), envir = globalenv())
  assign("depriv_scores_global", read_excel(file.path(folderIn,fdl,"deprivation_scores.xlsx"), sheet = 2), envir = globalenv())
  print("Done! Loaded lu, betweenness_global, closeness_global_all, closeness_global_in, closeness_global_out, OD_net_global, popArea_lsoa_global, depriv_global, depriv_scores_global into the global environment")
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
# Taken from global environment: folderIn, farea, lu, betweenness_global, closeness_global_all, closeness_global_in, closeness_global_out, OD_net_global, popArea_lsoa_global, depriv_global, depriv_scores_global
prepareLabels <- function(area_name, date, scale = c("LSOA11CD","MSOA11CD","LAD20CD"), folderIn, fdl, fproc, data = NULL, skipLoad = FALSE){
  scale <- match.arg(scale)
  print("Loading required data, this will overwrite the global environment in case of conflicts...")
  if(skipLoad == FALSE){
    if(scale == "LAD20CD"){
      loadPrerequisites(folderIn,fdl,fproc,"MSOA11CD")
    }else{
      loadPrerequisites(folderIn,fdl,fproc,scale)
    }
  }
  # Load and trim Azure file (needed for median age)
  if(is.null(data)){
    data <- loadArea(area_name,date,folderIn,farea)
  }
  data <- data[,c("OA11CD","pid","age","lng","lat")]
  data <- merge(data,lu[,c("OA11CD","LSOA11CD","MSOA11CD","LAD20CD")], by.x = "OA11CD", by.y = "OA11CD", all.x = T)
  # Prepare look-up with pop and areas + list of areas at chosen scale
  lu_area <- data.frame(lu[which(lu$AzureRef == area_name),c("LSOA11CD","MSOA11CD","LAD20CD")])
  lu_area <- lu_area[!duplicated(lu_area),]
  lu_area <- merge(lu_area, popArea_lsoa_global, by.x = "LSOA11CD", by.y = "LSOA11CD", all.x = T)
  list_area <- unique(lu_area[,scale])
  #
  labels_area <- data.frame(area = list_area, closeness_all = NA, closeness_in = NA, closeness_out = NA, betweenness = NA, distHPD = NA,
                            popDens = NA, medAge = NA, IMD19_ranks = NA, IMD19_scores = NA)
  colnames(labels_area)[1] <- scale
  # Centrality
  if(scale == "MSOA11CD" | scale == "LSOA11CD"){
    for(i in 1:length(list_area)){
      labels_area$betweenness[i] <- betweenness_global[names(betweenness_global) == list_area[i]] / mean(betweenness_global)
      labels_area$closeness_all[i] <- closeness_global_all[names(closeness_global_all) == list_area[i]] / mean(closeness_global_all)
      labels_area$closeness_in[i] <- closeness_global_in[names(closeness_global_in) == list_area[i]] / mean(closeness_global_in)
      labels_area$closeness_out[i] <- closeness_global_out[names(closeness_global_out) == list_area[i]] / mean(closeness_global_out)
    }
  }else if(scale == "LAD20CD"){
    print("Centrality data loaded at MSOA scale, using population weighted averages")
    for(i in 1:length(list_area)){
      ref <- unique(lu_area$MSOA11CD[lu_area$LAD20CD == list_area[i]])
      betw <- rep(NA,length(ref))
      clos_all <- rep(NA,length(ref))
      clos_in <- rep(NA,length(ref))
      clos_out <- rep(NA,length(ref))
      pops <- rep(NA,length(ref))
      for(j in 1:length(ref)){
        betw[j] <- betweenness_global[names(betweenness_global) == ref[j]] / mean(betweenness_global)
        clos_all[j] <- closeness_global_all[names(closeness_global_all) == ref[j]] / mean(closeness_global_all)
        clos_in[j] <- closeness_global_in[names(closeness_global_in) == ref[j]] / mean(closeness_global_in)
        clos_out[j] <- closeness_global_out[names(closeness_global_out) == ref[j]] / mean(closeness_global_out)
        pops[j] <- sum(lu_area$pop[lu_area$MSOA11CD == ref[j]])
      }
      labels_area$betweenness[i] <- sum(betw * pops) / sum(pops)
      labels_area$closeness_all[i] <- sum(clos_all * pops) / sum(pops)
      labels_area$closeness_in[i] <- sum(clos_in * pops) / sum(pops)
      labels_area$closeness_out[i] <- sum(clos_out * pops) / sum(pops)
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
  if(scale == "LAD20CD"){
    print("Warning (methods): Distances are computed from LAD centroid to MSOA centroid")
  }
  OD_net2 <- subgraph(OD_net_global, labels_area[,scale])
  if(!all(V(OD_net2)$name == labels_area[order(labels_area[,scale]),scale])){
    stop("Area lists not matching between network and labels")
  }
  V(OD_net2)$popDens <- labels_area$popDens[order(labels_area[,scale])]
  V(OD_net2)$lng <- NA
  V(OD_net2)$lat <- NA
  loc_max <- NULL
  for(i in 1:length(V(OD_net2))){
    V(OD_net2)$lng[i] <- mean(data$lng[data[,scale] == V(OD_net2)$name[i]])
    V(OD_net2)$lat[i] <- mean(data$lat[data[,scale] == V(OD_net2)$name[i]])
    if(V(OD_net2)$popDens[i] == max(V(OD_net2)$popDens[neighbors(OD_net2, V(OD_net2)[i], mode = "in")])){
      loc_max <- c(loc_max,V(OD_net2)[i])
    }
  }
  el <- get.edgelist(OD_net2, names=FALSE)
  E(OD_net2)$weight <- sapply(1:length(E(OD_net2)),function(i){distHaversine(c(V(OD_net2)$lng[el[i,1]],V(OD_net2)$lat[el[i,1]]),c(V(OD_net2)$lng[el[i,2]],V(OD_net2)$lat[el[i,2]]))})
  comps <- components(OD_net2, mode = "weak")
  distT <- rep(NA,length(V(OD_net2)))
  for(i in 1:length(V(OD_net2))){
    comp <- comps$membership[V(OD_net2)[i]]
    maxs <- loc_max[comps$membership[loc_max] == comp]
    distT[i] <- sum(distances(OD_net2,V(OD_net2)[i],maxs,mode = "all"))
  }
  labels_area$distHPD[order(labels_area[,scale])] <- distT
  # Deprivation
  if(scale != "LSOA11CD"){
    print("Deprivation data is at LSOA scale, using population weighted averages")
  }
  for(i in 1:length(list_area)){
    lsoas <- unique(lu$LSOA11CD[lu[,scale] == list_area[i]])
    pops <- rep(NA, length(lsoas))
    deprivs <- rep(NA,length(lsoas))
    deprivs_scores <- rep(NA,length(lsoas))
    for(j in 1:length(lsoas)){
      deprivs[j] <- depriv_global$`Index of Multiple Deprivation (IMD) Rank`[depriv_global$`LSOA code (2011)` == lsoas[j]]
      deprivs_scores[j] <- max(depriv_scores_global$`Index of Multiple Deprivation (IMD) Score`) - depriv_scores_global$`Index of Multiple Deprivation (IMD) Score`[depriv_scores_global$`LSOA code (2011)` == lsoas[j]]
      pops[j] <- lu_area$pop[lu_area$LSOA11CD == lsoas[j]]
    }
    labels_area$IMD19_ranks[i] <- sum(deprivs * pops) / sum(pops)
    labels_area$IMD19_scores[i] <- sum(deprivs_scores * pops) / sum(pops)
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
    stop(paste("File",file.path(folderOut,paste("labels_",area_name,"_",date,"_",scale,".csv",sep = "")),"is missing",sep = " "))
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
  avg1 <- colSums(resMean[,2:ncol(resMean)])/nrow(resMean)
  avg2 <- colSums(resSd[,2:ncol(resSd)])/nrow(resSd)
  avg3 <- colSums(resSkew[,2:ncol(resSkew)])/nrow(resSkew)
  avg4 <- colSums(resKurt[,2:ncol(resKurt)])/nrow(resKurt)
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
  png(file=file.path(folderOut,fplot,paste(info[1], info[2], info[3], info[4], "feature_importance.png", sep = "-")), width=1250, height=1000)
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
  avg1 <- colSums(resMean[,2:ncol(resMean)])/nrow(resMean)
  avg2 <- colSums(resSd[,2:ncol(resSd)])/nrow(resSd)
  avg3 <- colSums(resSkew[,2:ncol(resSkew)])/nrow(resSkew)
  avg4 <- colSums(resKurt[,2:ncol(resKurt)])/nrow(resKurt)
  res <- rbind(avg1,avg2,avg3,avg4)
  row.names(res) <- c("Mean", "Sd", "Skewness", "Kurotsis")
  res1 <- t(res)
  ### Plot means norm by moment column
  res2 <- res1
  for(i in 1:4){
    res2[,i] <- res2[,i]/max(res2[,i])
  }
  #
  png(file=file.path(folderOut,fplot,paste(info[1], info[2], info[3], info[4], "feature_importance_gg.png", sep = "-")), width=1250, height=1000)
  par(mfrow = c(1,2))
  image(1:ncol(res1), 1:nrow(res1), t(res1), axes = FALSE, col = hcl.colors(7, "Blue-Red", rev = TRUE),
        main = paste(info[1], info[2], info[3], info[4], "; average importance", sep = " "))
  axis(1, 1:ncol(res1), colnames(res1))
  axis(2, 1:nrow(res1), rownames(res1))
  image(1:ncol(res2), 1:nrow(res2), t(res2), axes = FALSE, col = hcl.colors(7, "Blue-Red", rev = TRUE),
        main = paste(info[1], info[2], info[3], info[4], "; average importance norm. p. column", sep = " "))
  axis(1, 1:ncol(res2), colnames(res2))
  axis(2, 1:nrow(res2), rownames(res2))
  dev.off()
  print(paste("Plot saved in ", file.path(folderOut,fplot), "/", sep = ""))
}
