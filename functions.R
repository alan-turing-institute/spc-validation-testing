################################
################################
####### Create test data #######
################################
################################


###### Toolbox for testing ######
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
###### Toolbox for testing ######

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

###### Toolbox for testing ######
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
###### Toolbox for testing ######


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
  print("Progressing...")
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
  print("Loaded lu, betweenness_global, closeness_global_all, closeness_global_in, closeness_global_out, OD_net_global, popArea_lsoa_global, depriv_global, depriv_scores_global into the global environment")
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
  labels_area <- data.frame(area = list_area, closeness_all = NA, closeness_in = NA, closeness_out = NA, betweenness = NA, distHPD = NA, distAllHPD = NA,
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
    print("Warning (methods): At LAD20CD scale, distances are computed directly from LAD centroid to MSOA centroid without using the commuting network.")
    OD_net2 <- subgraph(OD_net_global, lu_area[,"MSOA11CD"])
    for(i in 1:length(V(OD_net2))){
      ref <- lu_area$LSOA11CD[lu_area[,"MSOA11CD"] == V(OD_net2)$name[i]]
      V(OD_net2)$popDens[i] <- sum(lu_area$pop[lu_area$LSOA %in% ref]) / sum(lu_area$area[lu_area$LSOA %in% ref])
    }
    loc_max <- c(NA,NA)
    for(i in 1:length(V(OD_net2))){
      if(V(OD_net2)$popDens[i] == max(V(OD_net2)$popDens[neighbors(OD_net2, V(OD_net2)[i], mode = "in")])){
        loc_max <- rbind(loc_max,c(mean(data$lng[data[,"MSOA11CD"] == V(OD_net2)$name[i]]),mean(data$lat[data[,"MSOA11CD"] == V(OD_net2)$name[i]])))
      }
    }
    loc_max <- loc_max[2:nrow(loc_max),]
    distT <- rep(NA,length(list_area))
    distT2 <- rep(NA,length(list_area))
    for(i in 1:length(list_area)){
      base <- c(mean(data$lng[data[,"LAD20CD"] == list_area[i]]),mean(data$lat[data[,"LAD20CD"] == list_area[i]]))
      if(nrow(loc_max) > 1){
        for(j in 1:(nrow(loc_max) - 1)){
          base <- rbind(base,base)
        }
      }
      distT[i] <- sum(distHaversine(loc_max,base))
      distT2[i] <- min(distHaversine(loc_max,base))
    }
  }else{
    OD_net2 <- subgraph(OD_net_global, labels_area[,scale])
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
    if(!all(V(OD_net2)$name == labels_area[order(labels_area[,scale]),scale])){
      stop("Area lists not matching between network and labels")
    }
    el <- get.edgelist(OD_net2, names=FALSE)
    E(OD_net2)$weight <- sapply(1:length(E(OD_net2)),function(i){distHaversine(c(V(OD_net2)$lng[el[i,1]],V(OD_net2)$lat[el[i,1]]),c(V(OD_net2)$lng[el[i,2]],V(OD_net2)$lat[el[i,2]]))})
    comps <- components(OD_net2, mode = "weak")
    distT <- rep(NA,length(V(OD_net2)))
    distT2 <- rep(NA,length(V(OD_net2)))
    for(i in 1:length(V(OD_net2))){
      comp <- comps$membership[V(OD_net2)[i]]
      maxs <- loc_max[comps$membership[loc_max] == comp]
      distT[i] <- sum(distances(OD_net2,V(OD_net2)[i],maxs,mode = "all"))
      distT2[i] <- min(distances(OD_net2,V(OD_net2)[i],maxs,mode = "all"))
    }
  }
  labels_area$distAllHPD[order(labels_area[,scale])] <- distT
  labels_area$distHPD[order(labels_area[,scale])] <- distT2
  # Deprivation
  if(scale != "LSOA11CD"){
    print("Deprivation data is at LSOA scale, using population weighted averages")
  }
  for(i in 1:length(list_area)){
    lsoas <- unique(lu$LSOA11CD[lu[,scale] == list_area[i]])
    pops <- rep(NA,length(lsoas))
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

###### Toolbox for testing ######
#downloadPrerequisites(folderIn,fdl)
#loadPrerequisites(folderIn,fdl)

#dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)

#test <- prepareLabels("west-yorkshire", 2020, scale = "LSOA11CD", data = NULL)
#test <- prepareLabels("west-yorkshire", 2020, scale = "MSOA11CD", data = NULL)
#test <- prepareLabels("west-yorkshire", 2020, scale = "LAD20CD", data = NULL)
#test <- prepareLabels("west-yorkshire", 2020, scale = "LSOA11CD", data = dataWY)
###### Toolbox for testing ######


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

###### Toolbox for testing ######
#area_name = "west-yorkshire"
#scale = "LSOA11CD"
#scale = "MSOA11CD"
#scale = "LAD20CD"
#date = 2020
#variable_name = "incomeH"
#aggregation = "mean"
#ntrain = 20

#data = dataWY
#dataS <- data
#data <- dataS
#i = 1

#colnames(data)
#head(data)
#unique(dataS$LAD20CD)

#test <- runSHAP(area_name, date, scale, "incomeH", 3)
###### Toolbox for testing ######

# Taken from global environment: folderIn, farea, lu
runSHAP <- function(area_name, date, scale = c("LSOA11CD","MSOA11CD","LAD20CD"), variable_name, ntrain, predNames = "all", data = NULL, predictors = NULL, seed = NULL){
  scale <- match.arg(scale)
  # set seed
  if(is.null(seed)){
    seed <- floor(runif(5,10000000,99999999))[5]
    set.seed(seed)
  }else{
    set.seed(seed)
  }
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
  if(any(predNames != "all")){
    predictors <- predictors[complete.cases(predictors),c(scale,predNames)]
  }else{
    predictors <- predictors[complete.cases(predictors), ]
  }
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
  if(length(removed_pred_NA) > 0){
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
  return(list(explanation_mean$dt,explanation_sd$dt,explanation_skew$dt,explanation_kurt$dt,c(area_name, date, scale, variable_name, seed)))
}

runSHAP2.3 <- function(area_name, date, scale = c("LSOA11CD","MSOA11CD","LAD20CD"), variable_name, ntrain, predNames = "all", data = NULL, predictors = NULL, seed = NULL){
  scale <- match.arg(scale)
  # set seed
  if(is.null(seed)){
    seed <- floor(runif(5,10000000,99999999))[5]
    set.seed(seed)
  }else{
    set.seed(seed)
  }
  # Load and trim Azure file
  if(is.null(data)){
    data <- loadArea(area_name,date,folderIn,farea)
  }
  if(is.null(data[,scale]))(
    data <- merge(data,lu[,c("OA11CD","LSOA11CD","MSOA11CD","LAD20CD")], by.x = "OA11CD", by.y = "OA11CD", all.x = T)
  )
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
  if(any(predNames != "all")){
    predictors <- predictors[complete.cases(predictors),c(scale,predNames)]
  }else{
    predictors <- predictors[complete.cases(predictors), ]
  }
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
  if(length(removed_pred_NA) > 0){
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
  if(length(ntrain) == 1){
    testSet <- sample(1:l, ntrain)
  } else if(length(ntrain) > 1){
    testSet <- ntrain
  } else{
    stop("ntrain must be a number (test sample size) of an index vector (rows to be used as test sample)")
  }
  x_train <- as.matrix(predictors[-testSet,2:ncol(predictors)])
  y_train_mean <- moments[-testSet, "mean"]
  y_train_sd <- moments[-testSet, "sd"]
  y_train_skew <- moments[-testSet, "skewness"]
  y_train_kurt <- moments[-testSet, "kurtosis"]
  x_test <- as.matrix(predictors[testSet, 2:ncol(predictors)])
  model <- xgboost(data = x_train,label = y_train_mean,nround = 20,verbose = FALSE)
  explanation_mean <- shapr::explain(model = model, x_explain = x_test, x_train = x_train, approach = "empirical", prediction_zero = mean(y_train_mean))
  model <- xgboost(data = x_train,label = y_train_sd,nround = 20,verbose = FALSE)
  explanation_sd <- shapr::explain(model = model, x_explain = x_test, x_train = x_train, approach = "empirical", prediction_zero = mean(y_train_sd))
  model <- xgboost(data = x_train,label = y_train_skew,nround = 20,verbose = FALSE)
  explanation_skew <- shapr::explain(model = model, x_explain = x_test, x_train = x_train, approach = "empirical", prediction_zero = mean(y_train_skew))
  model <- xgboost(data = x_train,label = y_train_kurt,nround = 20,verbose = FALSE)
  explanation_kurt <- shapr::explain(model = model, x_explain = x_test, x_train = x_train, approach = "empirical", prediction_zero = mean(y_train_kurt))
  return(list(explanation_mean,explanation_sd,explanation_skew,explanation_kurt,c(area_name, date, scale, variable_name, seed)))
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
    res2[,i] <- res2[,i]/max(abs(res2[,i]))
  }
  i = 2
  #
  max(abs(range(res1)))
  png(file=file.path(folderOut,fplot,paste(info[1], info[2], info[3], info[4], info[5], "feature_importance_gg.png", sep = "-")), width=1250, height=1000)
  par(mfrow = c(1,2))
  image(1:ncol(res1), 1:nrow(res1), t(res1), axes = FALSE, ylab ="", xlab = "", col = hcl.colors(11, "Blue-Red", rev = TRUE),
        zlim = c(-max(abs(range(res1))),max(abs(range(res1)))), main = paste(info[1], info[2], info[3], info[4], "; average importance", sep = " "))
  axis(1, 1:ncol(res1), colnames(res1))
  axis(2, 1:nrow(res1), rownames(res1))
  image(1:ncol(res2), 1:nrow(res2), t(res2), axes = FALSE, ylab ="", xlab = "", col = hcl.colors(11, "Blue-Red", rev = TRUE),
        zlim = c(-max(abs(range(res2))),max(abs(range(res2)))), main = paste(info[1], info[2], info[3], info[4], "; average importance norm. p. column", sep = " "))
  axis(1, 1:ncol(res2), colnames(res2))
  axis(2, 1:nrow(res2), rownames(res2))
  mtext(paste("seed: ",info[5]), side = 1, line=3, adj = 1)
  dev.off()
  print(paste("Plot saved in ", file.path(folderOut,fplot), "/", sep = ""))
}

ggplotSHAP3 <- function(resSHAP, folderOut, fplot){
  resMean <- resSHAP[[1]]
  resSd <- resSHAP[[2]]
  resSkew <- resSHAP[[3]]
  resKurt <- resSHAP[[4]]
  info <- resSHAP[[5]]
  ### Plot means
  avg1 <- colSums(abs(resMean[,2:ncol(resMean)]))/nrow(resMean)
  avg2 <- colSums(abs(resSd[,2:ncol(resSd)]))/nrow(resSd)
  avg3 <- colSums(abs(resSkew[,2:ncol(resSkew)]))/nrow(resSkew)
  avg4 <- colSums(abs(resKurt[,2:ncol(resKurt)]))/nrow(resKurt)
  res <- rbind(avg1,avg2,avg3,avg4)
  row.names(res) <- c("Mean", "Sd", "Skewness", "Kurotsis")
  res1 <- t(res)
  ### Plot means norm by moment column
  res2 <- res1
  for(i in 1:4){
    res2[,i] <- res2[,i]/max(abs(res2[,i]))
  }
  i = 2
  #
  max(abs(range(res1)))
  png(file=file.path(folderOut,fplot,paste(info[1], info[2], info[3], info[4], info[5], "feature_importance_gg_abs.png", sep = "-")), width=1250, height=1000)
  par(mfrow = c(1,2))
  image(1:ncol(res1), 1:nrow(res1), t(res1), axes = FALSE, ylab ="", xlab = "", col = hcl.colors(11, "Blue-Red", rev = TRUE),
        zlim = c(0,max(abs(range(res1)))), main = paste(info[1], info[2], info[3], info[4], "; average importance", sep = " "))
  axis(1, 1:ncol(res1), colnames(res1))
  axis(2, 1:nrow(res1), rownames(res1))
  image(1:ncol(res2), 1:nrow(res2), t(res2), axes = FALSE, ylab ="", xlab = "", col = hcl.colors(11, "Blue-Red", rev = TRUE),
        zlim = c(0,max(abs(range(res2)))), main = paste(info[1], info[2], info[3], info[4], "; average importance norm. p. column", sep = " "))
  axis(1, 1:ncol(res2), colnames(res2))
  axis(2, 1:nrow(res2), rownames(res2))
  mtext(paste("seed: ",info[5]), side = 1, line=3, adj = 1)
  dev.off()
  print(paste("Plot saved in ", file.path(folderOut,fplot), "/", sep = ""))
}

#############################################
#############################################
####### Cluster intersection analysis #######
#############################################
#############################################


# Formatting function
formatFeat <- function(feat, normalised = FALSE, colNames = colnames(feat)[2:ncol(feat)]){
  if(length(colNames) == 1){
    featRes <- data.frame(temp = feat[,colNames])
    colnames(featRes) <- colNames
  }else{
    featRes <- feat[,colNames]
  }
  if(normalised == TRUE){
    for(i in 1:ncol(featRes)){
      featRes[,i] <- featRes[,i]/max(featRes[,i])
    }
  }
  rownames(featRes) <- feat[,1]
  return(featRes)
}

# Sub-function
interClust <- function(ccs,ccs2){
  res <- matrix(NA,nrow = length(ccs), ncol = length(ccs2))
  for(i in 1:length(ccs)){
    res[i,] <- sapply(ccs2,function(x){length(which(ccs[[i]] %in% x))})
  }
  return(res)
}

###### Toolbox for testing ######
#feat1 <- momentsWY
#feat2 <- labelsWY
#nclust <- 3
#nclust2 <- 6
#data <- dataWY
#skip = floor(0.05 * nrow(feat1))
###### Toolbox for testing ######

# Results of intersection
clustMatching <- function(feat1, feat2, nclust, nclust2 = nclust, skip = floor(0.05 * nrow(feat1)), format = FALSE, normalised = FALSE, colNames1 = colnames(feat1)[2:ncol(feat1)], colNames2 = colnames(feat2)[2:ncol(feat2)], flags = F){
  if(format == TRUE){
    feat1 <- formatFeat(feat1, normalised, colNames1)
    feat2 <- formatFeat(feat2, normalised, colNames2)
  }
  feat1a <- feat1[order(row.names(feat1)),]
  feat2a <- feat2[order(row.names(feat2)),]
  if(any(row.names(feat1a) != row.names(feat2a))){
    stop("Feature data frames do not contain the same areas")
  }
  #
  dfeat1 <- dist(feat1a)
  clust1 <- hclust(dfeat1)
  cut1 <- cutree(clust1, k = nclust)
  ccs1 <- list(NULL)
  for(i in 1:nclust){
    ccs1[[i]] <- names(cut1[which(cut1 == i)])
  }
  #
  dfeat2 <- dist(feat2a)
  clust2 <- hclust(dfeat2)
  cut2 <- cutree(clust2, k = nclust2)
  ccs2 <- list(NULL)
  for(i in 1:nclust2){
    ccs2[[i]] <- names(cut2[which(cut2 == i)])
  }
  #
  if(flags == T){
    flagsData <- data.frame(area = row.names(feat1a), badness = 0)
  }
  # Remove small clusters
  name1 <- deparse(substitute(feat1))
  name2 <- deparse(substitute(feat2))
  remove1 <- NULL
  remove2 <- NULL
  ccs1a <- ccs1
  ccs2a <- ccs2
  clustind1 <- 1:nclust
  clustind2 <- 1:nclust2
  for(i in 1:nclust){
    if(length(ccs1a[[i]]) <= skip){
      remove1 <- c(remove1,ccs1a[[i]])
      ccs1 <- ccs1[-i]
      clustind1 <- clustind1[-i]
      print(paste("Removed cluster", i ,"of", name1, "with only", length(ccs1a[[i]]), "elements", sep = " "))
    }
  }
  for(i in 1:nclust2){
    if(length(ccs2a[[i]]) <= skip){
      remove2 <- c(remove2,ccs2a[[i]])
      ccs2 <- ccs2[-i]
      clustind2 <- clustind2[-i]
      print(paste("Removed cluster", i ,"of", name2, "with only", length(ccs2a[[i]]), "elements", sep = " "))
    }
  }
  if(flags == T){
    flagsData$key1 <- NA
    flagsData$key1[flagsData$area %in% remove1] <- paste("belongs to a cluster of only", length(remove1), "areas", sep = " ")
    flagsData$badness[flagsData$area %in% remove1] <- flagsData$badness[flagsData$area %in% remove1] + min(1000, 2000/length(remove1))
  }
  #
  res1 <- interClust(ccs1,ccs2)
  rownames(res1) <- sapply(clustind1,function(x){paste(name1,x,sep = ".")})
  colnames(res1) <- sapply(clustind2,function(x){paste(name2,x,sep = ".")})
  res2 <- res1
  res3 <- res1
  for(i in 1:nrow(res1)){
    res2[i,] <- round(res2[i,]/rowSums(res2)[i] * 100,1)
  }
  for(i in 1:ncol(res1)){
    res3[,i] <- round(res3[,i]/colSums(res3)[i] * 100,1)
  }
  if(flags == T){
    flagsData$key2 <- NA
    for(i in 1:length(ccs1)){
      ref <- res2[i,]
      for(j in 1:length(ccs1[[i]])){
        area <- ccs1[[i]][j]
        if(!(area %in% remove2)){
          clust2 <- which(sapply(lapply(ccs2, function(x){grep(area, x)}), function(x){length(x) > 0}))
          percent <- res2[i,clust2]
          if(percent < max(res2[i,])){
            flagsData$key2[flagsData$area == area] <- paste("belongs to cluster", clustind2[clust2], "of", name2, "with only", percent, "% of such areas", sep = " ")
            flagsData$badness[flagsData$area == area] <- flagsData$badness[flagsData$area == area] + round(20 * (50 - percent))
          }
        }
      }
    }
    res <- list(flagsData,res1,res2,res3,purity(cut2,cut1)[[1]])
    names(res) <- c("flags","interNumbers","inter1perCent","inter2perCent","purity")
  } else{
    res <- list(res1,res2,res3,purity(cut2,cut1)[[1]])
    names(res) <- c("interNumbers","inter1perCent","inter2perCent","purity")
  }
  return(res)
}

findNumberClusters <- function(feat1, feat2, min, max){
  res <- matrix(NA, ncol = max, nrow = max)
  row <- rep(NA,max)
  col <- rep(NA,max)
  val <- rep(NA,max)
  for(i in min:max){
    for(j in min:max){
      invisible(capture.output(temp <- clustMatching(feat1, feat2, nclust = i, nclust2 = j)))
      res[i,j] <- temp$purity
      row
    }
  }
  # Plot
  res2 <- rbind(rep(NA,max),res)
  res2 <- cbind(rep(NA,max + 1),res2)
  res2 <- t(res2)
  name1 <- deparse(substitute(feat1))
  name2 <- deparse(substitute(feat2))
  axx <- list(title = name1)
  axy <- list(title = name2)
  axz <- list(title = "Purity")
  fig <- plot_ly(z = res2, type = 'surface')
  fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
  fig
  # Result
  maxInd <- which.max(res)
  l <- list(maxInd %% max,floor(maxInd/max) + 1,res[maxInd],fig)
  names(l) <- c(paste("nclust",name1,sep = "_"),paste("nclust",name2,sep = "_"),"purity","figure")
  return(l)
}


### Build moments

###### Toolbox for testing ######
#scale = "MSOA11CD"
#variable_name <- "incomeH"
#data <- dataWY
###### Toolbox for testing ######

makeMoments <- function(data,scale = c("LSOA11CD","MSOA11CD","LAD20CD"),variable_name){
  print("Loading look-up table...")
  assign("lu", read.csv(file.path(folderIn,fdl,"lookUp-GB.csv")), envir = globalenv())
  print("Merging with data...")
  data2 <- merge(data,lu[,c("OA11CD","LSOA11CD","MSOA11CD","LAD20CD")], by.x = "OA11CD", by.y = "OA11CD", all.x = T)
  print("Calculating moments...")
  new_area_list <- unique(data2[,scale])
  l <- length(new_area_list)
  moments <- data.frame(area = rep(NA,l), mean = NA, sd = NA, skewness = NA, kurtosis = NA)
  colnames(moments)[1] <- scale
  for(i in 1:l){
    ref <- which(data2[,scale] %in% new_area_list[i] & !is.na(data2[,variable_name]))
    moments[i,1] <- new_area_list[i]
    moments[i,2:5] <- findmoments(data2[ref,variable_name], graph = FALSE)
  }
  moments <- moments[order(moments[,scale]),]
  row.names(moments) <- 1:nrow(moments)
  print("Done")
  return(moments)
}

# Same but assumes scale already exists
makeMoments2 <- function(data,scale = c("LSOA11CD","MSOA11CD","LAD20CD"),variable_name){
  new_area_list <- unique(data[,scale])
  l <- length(new_area_list)
  moments <- data.frame(area = rep(NA,l), mean = NA, sd = NA, skewness = NA, kurtosis = NA)
  colnames(moments)[1] <- scale
  for(i in 1:l){
    ref <- which(data[,scale] %in% new_area_list[i] & !is.na(data[,variable_name]))
    moments[i,1] <- new_area_list[i]
    moments[i,2:5] <- findmoments(data[ref,variable_name], graph = FALSE)
  }
  moments <- moments[order(moments[,scale]),]
  row.names(moments) <- 1:nrow(moments)
  return(moments)
}

### Characterise content of clusters

extractCluster <- function(feat, nclust, format = FALSE, normalised = FALSE, colNames = colnames(feat)[2:ncol(feat)]){
  if(format == TRUE){
    feat <- formatFeat(feat, normalised, colNames)
  }
  feata <- feat[order(row.names(feat)),]
  #
  dfeat <- dist(feata)
  clust <- hclust(dfeat)
  cut <- cutree(clust, k = nclust)
  ccs <- list(NULL)
  for(i in 1:nclust){
    ccs[[i]] <- feata[names(cut[which(cut == i)]),]
  }
  return(ccs)
}

clusterCarac <- function(cluster, folderOut, fplot, title = NA, breaks = 10, freqOpt = T,pdf = F){
  cols <- viridis(length(cluster))
  nrows = length(cluster)
  ncols = ncol(cluster[[1]])
  if(pdf == F){
    png(file=file.path(folderOut,fplot,paste(title,"clusters_content.png",sep = "_")), width=200*ncols, height=200*nrows)
    par(mfrow = c(nrows, ncols))
    for(i in 1:nrows){
      for(j in 1:ncols){
        hist(cluster[[i]][,j], breaks, xlim = c(0,max(1,max(cluster[[i]][,j]))), col = cols[i], freq = freqOpt, main = paste("Cluster ", i, "; ", names(cluster[[i]][j]), sep = ""), xlab = "")
      }
    }
    dev.off()
    print(paste("Saved in", file.path(folderOut,fplot,paste(title,"clusters_content.png",sep = "_")),sep = " "))
  }
  if(pdf == T){
    pdf(file=file.path(folderOut,fplot,paste(title,"clusters_content.pdf",sep = "_")), width=2.5*ncols, height=2.5*nrows)
    par(mfrow = c(nrows, ncols))
    for(i in 1:nrows){
      for(j in 1:ncols){
        hist(cluster[[i]][,j], breaks, xlim = c(0,max(1,max(cluster[[i]][,j]))), col = cols[i], freq = freqOpt, main = paste("Cluster ", i, "; ", names(cluster[[i]][j]), sep = ""), xlab = "")
      }
    }
    dev.off()
    print(paste("Saved in", file.path(folderOut,fplot,paste(title,"clusters_content.pdf",sep = "_")),sep = " "))
  }
}

clusterCaracFeature <- function(cluster, folderOut, fplot, data, variable_name, scale, title = NA, skip = 0, breaks = 10, pdf = F, height = "find", width = "find", res = 400){
  a <- c(1,1,1,2,2,2,2,2,3,2,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5)
  b <- c(1,2,3,2,3,3,4,4,3,5,4,4,5,5,5,4,5,5,5,5,6,6,6,6,5)
  if(length(cluster) > 25){
    stop("Come on... that's a lot of clusters")
  }
  nrows = a[length(cluster)]
  ncols = b[length(cluster)]
  cols <- viridis(length(cluster))
  l <- length(cluster)
  data <- data[,c(scale,variable_name)]
  data$cluster <- 0
  data$skip <- F
  for(i in 1:l){
    rn <- row.names(cluster[[i]])
    data$cluster[data[,scale] %in% rn] <- i
    if(length(rn) > (skip + 1) * (skip + 1)){
      ind <- (1:length(rn))[-pmin((skip + 1)*unique(floor((1:length(rn)) / (skip + 1)) + 1),length(rn))]
      data$skip[data[,scale] %in% rn[ind]] <- T
    }
  }
  data <- data[data$skip == F,]
  if(pdf == F){
    png(file=file.path(folderOut,fplot,paste(title, "_clusters_content_variable_", variable_name, ".png", sep = "")), width= res*ncols, height=res*nrows)
    par(mfrow = c(nrows, ncols))
    for(i in 1:length(cluster)){
      dataTemp <- data[data$cluster == i,]
      ref <- unique(dataTemp[,scale])
      MX <- matrix(NA, nrow = length(ref), ncol = 2*breaks)
      MY <- matrix(NA, nrow = length(ref), ncol = 2*breaks)
      
      for(j in 1:length(ref)){
        h <- hist(data[data[,scale] == ref[j], variable_name], breaks = breaks, plot = F)
        MX[j,1:length(h$mids)] <- h$mids
        MY[j,1:length(h$density)] <- h$density
      }
      if(height == "find"){
        h <- max(MY, na.rm = T) * 1.1
      } else{
        h <- height
      }
      if(width == "find"){
        w <- max(MX, na.rm = T) * 1.1
      } else{
        w <- width
      }
      plot(MX[1,],MY[1,], ylim = c(0,h), xlim = c(0,w), pch = NA, main = paste("Cluster", i, sep = " "), ylab = "frequency", xlab = variable_name)
      for(j in 1:length(ref)){
        lines(MX[j,], MY[j,], col = alpha(cols[i], 0.5))
      }
    }
    dev.off()
    print(paste("Saved in", file.path(folderOut,fplot,paste(title, "_clusters_content_variable_", variable_name, ".png", sep = "")), sep = " "))
  }
  if(pdf == T){
    pdf(file=file.path(folderOut,fplot,paste(title, "_clusters_content_variable_", variable_name, ".pdf", sep = "")), width= 2.5*ncols, height=2.5*nrows)
    par(mfrow = c(nrows, ncols))
    for(i in 1:length(cluster)){
      dataTemp <- data[data$cluster == i,]
      ref <- unique(dataTemp[,scale])
      MX <- matrix(NA, nrow = length(ref), ncol = 2*breaks)
      MY <- matrix(NA, nrow = length(ref), ncol = 2*breaks)
      
      for(j in 1:length(ref)){
        h <- hist(data[data[,scale] == ref[j], variable_name], breaks = breaks, plot = F)
        MX[j,1:length(h$mids)] <- h$mids
        MY[j,1:length(h$density)] <- h$density
      }
      if(height == "find"){
        h <- max(MY, na.rm = T) * 1.1
      } else{
        h <- height
      }
      if(width == "find"){
        w <- max(MX, na.rm = T) * 1.1
      } else{
        w <- width
      }
      plot(MX[1,],MY[1,], ylim = c(0,h), xlim = c(0,w), pch = NA, main = paste("Cluster", i, sep = " "), ylab = "frequency", xlab = variable_name)
      for(j in 1:length(ref)){
        lines(MX[j,], MY[j,], col = alpha(cols[i], 0.5))
      }
    }
    dev.off()
    print(paste("Saved in", file.path(folderOut,fplot,paste(title, "_clusters_content_variable_", variable_name, ".pdf", sep = "")), sep = " "))
  }
}

clusterMapping <- function(cluster, scale = c("LSOA11CS","MSOA11CD","LAD20CD")){
  scale = match.arg(scale)
  cols <- viridis(length(cluster))
  spdf_fortified <- NULL
  if(scale == "MSOA11CD"){
    if(!(file.exists(file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson")))){
      print("Downloading geojson at MSOA scale...")
      download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/MSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson"))
    }
    data_json <- geojson_read(file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson"), what = "sp")
    for(i in 1:length(cluster)){
      spdf_region = data_json[data_json@data$MSOA11CD %in% row.names(cluster[[i]]), ]
      spdf_fortified_temp <- tidy(spdf_region)
      spdf_fortified_temp$col <- cols[i]
      spdf_fortified <- rbind(spdf_fortified,spdf_fortified_temp)
    }
  } else if(scale == "LSOA11CD"){
    if(!(file.exists(file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson")))){
      print("Downloading geojson at LSOA scale...")
      download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/LSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))
    }
    data_json <- geojson_read(file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"), what = "sp")
    for(i in 1:length(cluster)){
      spdf_region = data_json[data_json@data$LSOA11CD %in% row.names(cluster[[i]]), ]
      spdf_fortified_temp <- tidy(spdf_region)
      spdf_fortified_temp$col <- cols[i]
      spdf_fortified <- rbind(spdf_fortified,spdf_fortified_temp)
    }
  } else if(scale == "LAD20CD"){
    stop("Coming soon")
  }
  g <- ggplot() +
                geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill=spdf_fortified$col, color="white") +
                theme_void() +
                coord_map()
  return(g)
}


#############################
#############################
####### Area flagging #######
#############################
#############################


flagSHAPOne <- function(shapRes,imp1th = 0.2,imp2th = 0.1,corth = 0.3){
  shapValues <- as.data.frame(shapRes$shapley_values)
  shapFeatures <- shapRes$pred_explain
  importance <- colSums(abs(shapValues))[-1]
  importance1 <- importance / mean(shapFeatures)
  importance2 <- importance / max(importance)
  #
  cors <- NULL
  for(i in colnames(shapValues)[-1]){
    cors <- c(cors,cor(shapValues[,i],shapFeatures))
  }
  names(cors) <- names(importance)
  df <- data.frame(feat = 1:length(shapFeatures))
  wh <- which(importance1 >= imp1th & importance2 > imp2th & cors > corth)
  for(i in wh){
    model <- lm(shapValues[,i] ~ shapFeatures)
    df[,colnames(shapValues)[i+1]] <- floor(abs(model$residuals) / sd(model$residuals))
  }
  return(list(importance,cors,df))
}

flagSHAP <- function(shapRes,areas,imp1th = 0.2,imp2th = 0.1,corth = 0.3,distribNames=c("Mean","Sd","Skewness","Kurtosis")){
  all_res <- rep(list(NULL),length(shapRes) - 1)
  for(i in 1:(length(shapRes) - 1)){
    all_res[[i]] <- flagSHAPOne(shapRes[[i]],imp1th = 0.2,imp2th = 0.1,corth = 0.3)
  }
  print("Summary:")
  for(i in 1:(length(shapRes) - 1)){
    print(paste("    ",distribNames[i],":",sep = ""))
    print("        importance:")
    print(all_res[[i]][[1]])
    print("        correllation:")
    print(all_res[[i]][[2]])
    print("-----")
  }
  flagsData <- data.frame(area = areas, badness = 0)
  for(i in 1:(length(shapRes) - 1)){
    df <- all_res[[i]][[3]]
    for(j in 1:(ncol(df) - 1)){
      if(sum(df[,j+1]) > 0){
        flagsData$badness <- flagsData$badness + df[,j+1] * (1000 / (ncol(df) - 1))
        flagsData[,paste(distribNames[i],colnames(df)[j+1],sep = "_")] <- NA
        wh <- which(df[,j+1] > 0)
        flagsData[,paste(distribNames[i],colnames(df)[j+1],sep = "_")][wh] <- paste("is above",df[wh,j+1],"standard deviation(s) for characteristic",distribNames[i],"explained by",colnames(df)[j+1],sep = " ")
      }
    }
  }
  return(flagsData)
}

readFlagsLine <- function(v){
  print(paste(v[1], "| Total badness:", v[2], sep = " "))
  pbs <- v[2 + which(!is.na(v[3:length(v)]))]
  names <- names(v[2 + which(!is.na(v[3:length(v)]))])
  for(i in 1:length(pbs)){
    print(paste("   ", names[i], "-", pbs[i]))
  }
}

readFlags <- function(flags, map = F, scale = NA, over = 0, under = 50000){
  mm <- max(flags$badness)
  ref <- rev(0:floor((mm - 1) / 1000))
  ref <- ref[ref >= over/1000 & ref <= under/1000]
  for(i in ref){
    print(paste("Badness over ", i*1000 ,":", sep = ""))
    bad_areas <- flags$area[flags$badness > i*1000 & flags$badness <= (i + 1)*1000]
    bad_areas <- bad_areas[order(-flags$badness[flags$badness > i*1000 & flags$badness <= (i + 1)*1000])]
    for(j in bad_areas){
      readFlagsLine(flags[flags$area == j,])
    }
  }
  if(map == T){
    cols <- heat.colors(length(ref))
    spdf_fortified <- NULL
    if(scale == "MSOA11CD"){
      if(!(file.exists(file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson")))){
        print("Downloading geojson at MSOA scale...")
        download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/MSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson"))
      }
      data_json <- geojson_read(file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson"), what = "sp")
      non_bad_areas <- flags$area[flags$badness < 1]
      if(length(non_bad_areas) > 0){
        spdf_region = data_json[data_json@data$MSOA11CD %in% non_bad_areas,]
        spdf_fortified_temp <- tidy(spdf_region)
        spdf_fortified_temp$col <- "grey"
        spdf_fortified <- rbind(spdf_fortified,spdf_fortified_temp)
      }
      for(i in 1:length(ref)){
        bad_areas <- flags$area[flags$badness > ref[i]*1000 & flags$badness <= (ref[i] + 1)*1000]
        if(length(bad_areas) > 0){
          spdf_region = data_json[data_json@data$MSOA11CD %in% bad_areas,]
          spdf_fortified_temp <- tidy(spdf_region)
          spdf_fortified_temp$col <- cols[i]
          spdf_fortified <- rbind(spdf_fortified,spdf_fortified_temp)
        }
      }
    } else if(scale == "LSOA11CD"){
      if(!(file.exists(file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson")))){
        print("Downloading geojson at LSOA scale...")
        download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/LSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))
      }
      data_json <- geojson_read(file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"), what = "sp")
      non_bad_areas <- flags$area[flags$badness < 1]
      if(length(non_bad_areas) > 0){
        spdf_region = data_json[data_json@data$MSOA11CD %in% non_bad_areas,]
        spdf_fortified_temp <- tidy(spdf_region)
        spdf_fortified_temp$col <- "grey"
        spdf_fortified <- rbind(spdf_fortified,spdf_fortified_temp)
      }
      for(i in 1:length(ref)){
        bad_areas <- flags$area[flags$badness > ref[i]*1000 & flags$badness <= (ref[i] + 1)*1000]
        if(length(bad_areas) > 0){
          spdf_region = data_json[data_json@data$LSOA11CD %in% bad_areas,]
          spdf_fortified_temp <- tidy(spdf_region)
          spdf_fortified_temp$col <- cols[i]
          spdf_fortified <- rbind(spdf_fortified,spdf_fortified_temp)
        }
      }
    } else if(scale == "LAD20CD"){
      stop("Coming soon")
    }
    ggplot() +
      geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill=spdf_fortified$col, color="white") +
      theme_void() +
      coord_map()
  }
}

mergeFlags <- function(flag1,flag2,weight = 1){
  flag1 <- flag1[order(flag1$area),]
  rownames(flag1) <- 1:nrow(flag1)
  flag2 <- flag2[order(flag2$area),]
  rownames(flag2) <- 1:nrow(flag2)
  flag_res <- data.frame(area = flag2$area)
  flag_res$badness <- weight * flag1$badness + flag2$badness
  flag_res <- cbind(flag_res,flag1[,3:ncol(flag1)])
  flag_res <- cbind(flag_res,flag2[,3:ncol(flag2)])
  return(flag_res)
}

# It is now expected that data contains the column corresponding to the right scale
queryArea <- function(name, flags, data, labels, scale, variable_name, nclust, nclust2 = nclust, colNames = colnames(labels)[2:ncol(labels), maps = T]){
  readFlagsLine(flags[flags$area == name,])
  print("---------------------------------------------------------------------------------------------------------")
  # Get clusters
  moments_all <- makeMoments2(data, scale, variable_name)
  moments_all <- moments_all[order(moments_all[,scale]),]
  row.names(moments_all) <- 1:nrow(moments_all)
  moments_all_norm <- formatFeat(moments_all, normalised = TRUE)
  clusters_moments <- extractCluster(moments_all_norm, nclust)
  for(i in 1:length(clusters_moments)){
    if(name %in% row.names(clusters_moments[[i]])){
      cid_m <- i
    }
  }
  moments_ids <- which(moments_all[,scale] %in% row.names(clusters_moments[[cid_m]]))
  #
  labels_all <- labels[order(labels[,scale]),]
  row.names(labels_all) <- 1:nrow(labels_all)
  labels_norm <- formatFeat(labels_all, normalised = TRUE, colNames)
  clusters_labels <- extractCluster(labels_norm, nclust2)
  for(i in 1:length(clusters_labels)){
    if(name %in% row.names(clusters_labels[[i]])){
      cid_l <- i
    }
  }
  labels_ids <- which(labels_all[,scale] %in% row.names(clusters_labels[[cid_l]]))
  # Distrib data
  data_moments <- data.frame(
    name = c("mean","sd","skewness","kurtosis"),
    area = c(round(moments_all[moments_all[,scale] == name,2],2),round(moments_all[moments_all[,scale] == name,3],2),round(moments_all[moments_all[,scale] == name,4],2),round(moments_all[moments_all[,scale] == name,5],2)),
    from = c(round(mean(moments_all[moments_ids,2]),2),round(mean(moments_all[moments_ids,3]),2),round(mean(moments_all[moments_ids,4]),2),round(mean(moments_all[moments_ids,5]),2)),
    from_sd = c(round(sd(moments_all[moments_ids,2]),2),round(sd(moments_all[moments_ids,3]),2),round(sd(moments_all[moments_ids,4]),2),round(sd(moments_all[moments_ids,5]),2)),
    labels = c(round(mean(moments_all[labels_ids,2]),2),round(mean(moments_all[labels_ids,3]),2),round(mean(moments_all[labels_ids,4]),2),round(mean(moments_all[labels_ids,5]),2)),
    labels_sd = c(round(sd(moments_all[labels_ids,2]),2),round(sd(moments_all[labels_ids,3]),2),round(sd(moments_all[labels_ids,4]),2),round(sd(moments_all[labels_ids,5]),2))
  )
  # Labels data
  labels_all_reduced <- labels_all
  for(i in 2:ncol(labels_all_reduced )){
    labels_all_reduced[,i] <- labels_all_reduced[,i] / 10^floor(log10(max(labels_all_reduced[,i])))
  }
  data_labels <- data.frame(
    name = colNames,
    area = unlist(unname(labels_all_reduced[labels_all_reduced[,scale] == name,colNames])),
    all = NA,
    all_sd = NA
  )
  for(i in 1:length(colNames)){
    data_labels$all[i] = mean(labels_all_reduced[labels_ids,colNames[i]],na.rm = T)
    data_labels$all_sd[i] = sd(labels_all_reduced[labels_ids,colNames[i]],na.rm = T)
  }
  # Print summary
  print(paste("Moments for ", variable_name," are: ", data_moments$area[1], ", ", data_moments$area[2],", ", data_moments$area[3], " and ", data_moments$area[4], sep = ""))
  print(paste("Average values for the cluster are: ", data_moments$from[1], ", ", data_moments$from[2],", ", data_moments$from[3], " and ", data_moments$from[4], sep = ""))
  print(paste("Average values for cluster ", cid_l, " of labels are: ", data_moments$labels[1], ", ", data_moments$labels[2],", ", data_moments$labels[3], " and ", data_moments$labels[4], sep = ""))
  y_lim <- max(c(data_moments$from + 3 * data_moments$from_sd, data_moments$labels + 3 * data_moments$labels_sd)) * 1.05
  g1 <- ggplot(data_moments) +
    geom_bar( aes(x=name, y=from), stat="identity", fill="skyblue", alpha=0.5) +
    geom_errorbar( aes(x=name, ymin=from-from_sd, ymax=from+from_sd), width=0.8, colour="darkblue", alpha=0.9, size=1) +
    geom_errorbar( aes(x=name, ymin=from-2*from_sd, ymax=from+2*from_sd), width=0.5, colour="darkblue", alpha=0.9, size=1) +
    geom_errorbar( aes(x=name, ymin=from-3*from_sd, ymax=from+3*from_sd), width=0.2, colour="darkblue", alpha=0.9, size=1) +
    geom_point( aes(x=name, y=area), colour="red", alpha=0.9, size=3) +
    coord_flip() + theme(axis.title = element_blank()) + ggtitle(paste("Cluster",cid_m,"of moments",sep = " ")) + ylim(0, y_lim)
  g2 <- ggplot(data_moments) +
    geom_bar( aes(x=name, y=labels), stat="identity", fill="darkseagreen2", alpha=0.5) +
    geom_errorbar( aes(x=name, ymin=labels-labels_sd, ymax=labels+labels_sd), width=0.8, colour="darkgreen", alpha=0.9, size=1) +
    geom_errorbar( aes(x=name, ymin=labels-2*labels_sd, ymax=labels+2*labels_sd), width=0.5, colour="darkgreen", alpha=0.9, size=1) +
    geom_errorbar( aes(x=name, ymin=labels-3*labels_sd, ymax=labels+3*labels_sd), width=0.2, colour="darkgreen", alpha=0.9, size=1) +
    geom_point( aes(x=name, y=area), colour="red", alpha=0.9, size=3) +
    coord_flip() + theme(axis.title = element_blank()) + ggtitle(paste("Cluster",cid_l,"of labels",sep = " ")) + ylim(0, y_lim)
  g3 <- ggplot(data_labels) +
    geom_bar( aes(x=name, y=all), stat="identity", fill="goldenrod2", alpha=0.5) +
    geom_errorbar( aes(x=name, ymin=pmax(0,all-all_sd), ymax=all+all_sd), width=0.8, colour="tomato4", alpha=0.9, size=1) +
    geom_errorbar( aes(x=name, ymin=pmax(0,all-2*all_sd), ymax=all+2*all_sd), width=0.5, colour="tomato4", alpha=0.9, size=1) +
    geom_errorbar( aes(x=name, ymin=pmax(0,all-3*all_sd), ymax=all+3*all_sd), width=0.2, colour="tomato4", alpha=0.9, size=1) +
    geom_point( aes(x=name, y=area), colour="red", alpha=0.9, size=3) +
    coord_flip() + theme(axis.title = element_blank()) + ggtitle(paste("Cluster",cid_l,"of labels",sep = " "))
  if(maps == F){
    grid.arrange(g1, g2, g3, ncol=3)
  } else{
    col1 <- viridis(nclust)
    col2 <- viridis(nclust2)
    spdf_fortified <- NULL
    if(scale == "MSOA11CD"){
      if(!(file.exists(file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson")))){
        print("Downloading geojson at MSOA scale...")
        download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/MSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson"))
      }
      data_json <- geojson_read(file.path(folderIn,fdl,"MSOA_2011_Pop20.geojson"), what = "sp")
      moments_areas <- moments_all[moments_ids,scale]
      labels_areas <- labels_all[labels_ids,scale]
      if(length(moments_areas) > 0){
        spdf_region <- data_json[!(data_json@data$MSOA11CD %in% moments_areas) & (data_json@data$MSOA11CD %in% moments_all[,scale]),]
        spdf_fortified_temp <- tidy(spdf_region)
        spdf_fortified_temp$col <- "grey"
        spdf_region2 <- data_json[(data_json@data$MSOA11CD %in% moments_areas) & data_json@data$MSOA11CD != name,]
        spdf_fortified_temp2 <- tidy(spdf_region2)
        spdf_fortified_temp2$col <- col1[cid_m]
        spdf_region3 <- data_json[data_json@data$MSOA11CD == name,]
        spdf_fortified_temp3 <- tidy(spdf_region3)
        spdf_fortified_temp3$col <- "red"
        spdf_fortified_moments <- rbind(spdf_fortified,spdf_fortified_temp,spdf_fortified_temp2,spdf_fortified_temp3)
      }
      if(length(labels_areas) > 0){
        spdf_region <- data_json[!(data_json@data$MSOA11CD %in% labels_areas) & (data_json@data$MSOA11CD %in% labels_all[,scale]),]
        spdf_fortified_temp <- tidy(spdf_region)
        spdf_fortified_temp$col <- "grey"
        spdf_region2 <- data_json[(data_json@data$MSOA11CD %in% labels_areas) & data_json@data$MSOA11CD != name,]
        spdf_fortified_temp2 <- tidy(spdf_region2)
        spdf_fortified_temp2$col <- col1[cid_l]
        spdf_region3 <- data_json[data_json@data$MSOA11CD == name,]
        spdf_fortified_temp3 <- tidy(spdf_region3)
        spdf_fortified_temp3$col <- "red"
        spdf_fortified_labels <- rbind(spdf_fortified,spdf_fortified_temp,spdf_fortified_temp2,spdf_fortified_temp3)
      }
    } else if(scale == "LSOA11CD"){
      if(!(file.exists(file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson")))){
        print("Downloading geojson at LSOA scale...")
        download.file("https://ramp0storage.blob.core.windows.net/nationaldata-v2/GIS/LSOA_2011_Pop20.geojson", destfile = file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"))
      }
      data_json <- geojson_read(file.path(folderIn,fdl,"LSOA_2011_Pop20.geojson"), what = "sp")
      non_bad_areas <- flags$area[flags$badness < 1]
      if(length(non_bad_areas) > 0){
        spdf_region = data_json[data_json@data$MSOA11CD %in% non_bad_areas,]
        spdf_fortified_temp <- tidy(spdf_region)
        spdf_fortified_temp$col <- "grey"
        spdf_fortified <- rbind(spdf_fortified,spdf_fortified_temp)
      }
      for(i in 1:length(ref)){
        bad_areas <- flags$area[flags$badness > ref[i]*1000 & flags$badness <= (ref[i] + 1)*1000]
        if(length(bad_areas) > 0){
          spdf_region = data_json[data_json@data$LSOA11CD %in% bad_areas,]
          spdf_fortified_temp <- tidy(spdf_region)
          spdf_fortified_temp$col <- cols[i]
          spdf_fortified <- rbind(spdf_fortified,spdf_fortified_temp)
        }
      }
    } else if(scale == "LAD20CD"){
      stop("Coming soon")
    }
    g4 <- ggplot() +
      geom_polygon(data = spdf_fortified_moments, aes( x = long, y = lat, group = group), fill=spdf_fortified_moments$col, color="white") +
      theme_void() + ggtitle(paste("Cluster",cid_m,"of moments",sep = " ")) +
      coord_map()
    g5 <- NULL
    g6 <- ggplot() +
      geom_polygon(data = spdf_fortified_labels, aes( x = long, y = lat, group = group), fill=spdf_fortified_labels$col, color="white") +
      theme_void() + ggtitle(paste("Cluster",cid_l,"of labels",sep = " ")) +
      coord_map()
    grid.arrange(g1, g2, g3, g4, g5, g6, ncol=3)
  }
  
}

dev.off()

queryArea("E02002189", all_flags, dataWY, labels, "MSOA11CD", "incomeH", 5, 5, colNames)

queryArea("E02002312", all_flags, dataWY, labels, "MSOA11CD", "incomeH", 5, 5, colNames)

###### Toolbox for testing ######
# name = "E02002189"
# flags = all_flags
# data = dataWY
# labels <- loadLabels("west-yorkshire",2020, "MSOA11CD")
# scale = "MSOA11CD"
# variable_name = "incomeH"
# nclust = 5
# nclust2 = 5
# colNames = c("closeness_all","betweenness","distHPD","popDens","medAge","IMD19_ranks")
###### Toolbox for testing ######


nclust = 5
nclust2 = 5


which(data_json@data$MSOA11CD == name)
