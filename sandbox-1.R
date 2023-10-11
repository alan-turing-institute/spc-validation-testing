#devtools::install_github("ld-archer/UKCensusAPI")
#library(UKCensusAPI)
library(fitdistrplus)
library(univariateML)

set.seed(18061815)





#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################



testa <- runSHAP2.3("west-yorkshire", 2020, "MSOA11CD", "incomeH", 20, predNames = columnsPreds)

plot(testa[[1]], index_x_explain = 1:10)
plot(testa[[1]], index_x_explain = 1:10, bar_plot_phi0 = FALSE)

png
plot(testa[[1]], plot_type = "beeswarm")

temp <- testa[[1]]

a <- temp$shapley_values
b <- temp$pred_explain

model <- lm(b ~ a)
#model <- lm(a ~ b)
plot(model)

plot(a,b, pch = 20)
#plot(b,a)
lines(a, fitted(model))
points(a[12],b[12],cex = 2, col = 4)

simpleFunction <- function(a,b){
  model <- lm(b ~ a)
  plot(a,b, pch = 20)
  lines(a, fitted(model))
  return(model$residuals)
}

a <- temp$shapley_values
b <- temp$pred_explain

simpleFunction(a$IMD19_ranks,b)
simpleFunction(a$medAge,b)
simpleFunction(a$popDens,b)
simpleFunction(a$distHPD,b)
simpleFunction(a$closeness_all,b)
simpleFunction(a$betweenness,b)

cor(a$IMD19_ranks,b)
cor(a$medAge,b)
cor(a$popDens,b)
cor(a$distHPD,b)
cor(a$closeness_all,b)
cor(a$betweenness,b)


simpleFunction(a$distHPD[-7],b[-7])


model <- lm(b ~ a$distHPD)


colSums(abs(a))

model$residuals


temp <- testa[[1]]
sum(testa[[1]][1])

temp$shapley_values
temp$pred_explain

sum(temp$shapley_values[1])

shapRes <- testa[[1]]

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
    print(all_res[[i]][[1]])
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


j = 1

shapRes = testa
areas = paste("Dummy",1:20,sep = "_")

test <- flagSHAP(testa, areas)
View(test)
readFlags(test)

all_res[[1]][[3]]

flagSHAPOne(testa[[1]])

flags = test


i = 4

floor(abs(model$residuals) / sd(model$residuals))

as.data.frame(shapValues)





#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################














dataWY <- loadArea("west-yorkshire",2020,folderIn,farea)
moments <- makeMoments(dataWY, "MSOA11CD", "incomeH")
momentsWY <- formatFeat(moments, normalised = TRUE)
#
labels <- loadLabels("west-yorkshire",2020, "MSOA11CD")
labelsWY <- formatFeat(labels, normalised = TRUE, colNames = c("closeness_all","betweenness","distHPD","popDens","medAge","IMD19_ranks"))
#
test <- clustMatching(momentsWY, labelsWY, nclust = 4)

min = 3
max = 10
feat1 = momentsWY
feat2 = labelsWY

nclust = 3
nclust2 = 4

test <- clustMatching(momentsWY, labelsWY, nclust = 4, skip = 14)


clustMatching(feat1, feat2, nclust = i, nclust2 = j, skip = 14)
lustMatching(feat1, feat2, nclust = i, nclust2 = j, skip = 14)


findNumberClusters <- function(feat1, feat2, min, max){
  res <- matrix(NA, ncol = max, nrow = max)
  row <- rep(NA,max)
  col <- rep(NA,max)
  val <- rep(NA,max)
  for(i in min:max){
    for(j in min:max){
      temp <- clustMatching(feat1, feat2, nclust = i, nclust2 = j, skip = 14)
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

dev.off()

test <- findNumberClusters(momentsWY, labelsWY, 3, 10)

test[[4]]


res <- matrix(c(1:11,25,12:24), ncol = 5, nrow = 5)

temp <- which.max(res)

plot(1:3,1:3)

temp %% 5
floor(temp/5) + 1

res <- rep(list(NULL),3)

for(i in 3:10){
  for(j in 3:10){
    test <- clustMatching(momentsWY, labelsWY, nclust = i, nclust2 = j)
    res[[1]] <- c(res[[1]],i)
    res[[2]] <- c(res[[2]],j)
    res[[3]] <- c(res[[3]],print(test$purity))
  }
}

res[[1]][which.max(res[[3]])]
res[[2]][which.max(res[[3]])]
res[[3]][which.max(res[[3]])]

clustMatching(momentsWY, labelsWY, nclust = 4, nclust2 = 4)


install.packages('plotly')
library(plotly)

# Plot
plot_ly(z = volcano, type = "surface")





############################
############################
############################
############################
############################

feat2 = labelsWY
nclust2 = 3
nclust2 = 4

feat2a <- feat2[order(row.names(feat2)),]

dfeat2 <- dist(feat2a)
clust2 <- hclust(dfeat2)
cut2 <- cutree(clust2, k = nclust2)
ccs2 <- list(NULL)
for(i in 1:nclust2){
  ccs2[[i]] <- names(cut2[which(cut2 == i)])
}
# Remove small clusters
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
  }
}
for(i in 1:nclust2){
  if(length(ccs2a[[i]]) <= skip){
    remove2 <- c(remove2,ccs2a[[i]])
    ccs2 <- ccs2[-i]
    clustind2 <- clustind2[-i]
  }
}





############################
############################
############################
############################
############################


clusterCarac(test, folderOut, fplot, title = "test2")

cluster = test

a <- rep(NA,20)
b <- rep(NA,20)
c <- rep(NA,20)
for(n in 1:20){
  a[n] <- floor(sqrt(n))
  b[n] <- ceiling(sqrt(n))
  c[n] <- a[n] * b[n]
}



a*b

install.packages("randquotes")
library(randquotes)

1:20

clusterCaracFeature <- function(cluster, folderOut, fplot, data, variable_name, scale, title = NA, skip = 0, breaks = 10, height = "find", res = 400){
  a <- c(1,1,1,2,2,2,2,2,3,2,3,3,3,3,3,4,4,4,4,4,4,4,4,4,5)
  b <- c(1,2,3,2,3,3,4,4,3,5,4,4,5,5,5,4,5,5,5,5,6,6,6,6,5)
  if(length(clusters) > 25){
    stop("Come on... that's a lot of clusters")
  }
  nrows = a[length(cluster)]
  ncols = b[length(cluster)]
  l <- rep(NA,length(cluster))
  for(i in 1:length(cluster)){
    if(nrow(cluster[[i]]) > (skip + 1) * (skip + 1)){
      l[i] <- floor(nrow(cluster[[i]])/(skip + 1))
    }else{
      l[i] <- nrow(cluster[[i]])
    }
  }
  MX <- matrix(NA, nrow = sum(l), ncol = 2*breaks)
  MY <- matrix(NA, nrow = sum(l), ncol = 2*breaks)
  for(i in 1:length(cluster)){
    for(k in 1:l[i]){
      if(l[i] > (skip + 1) * (skip + 1)){
        ref <- row.names(cluster[[i]])[pmin(1 + (skip + 1) * (k - 1), nrow(cluster[[i]]))]
      }else{
        ref <- row.names(cluster[[i]])
      }
      h <- hist(data[data[,scale] %in% ref,variable_name], breaks = breaks, plot = F)
      index <- k + sum(l[0:(i-1)])
      MY[index,1:length(h$density)] <- h$density
      MX[index,1:length(h$mids)] <- h$mids
    }
  }
  png(file=file.path(folderOut,fplot,paste(title, "_clusters_content_variable_", variable_name, ".png", sep = "")), width= res*ncols, height=res*nrows)
  par(mfrow = c(nrows, ncols))
  for(i in 1:length(cluster)){
    ind <- 1 + sum(l[0:(i-1)])
    if(height == "find"){
      h <- max(MY, na.rm = T) * 1.1
    }else {
      h <- height
    }
    plot(MX[ind,],MY[ind,], ylim = c(0,h), xlim = c(0, max(MX, na.rm = T)), pch = NA, main = paste("Cluster", i, sep = " "), ylab = "frequency", xlab = variable_name)
    for(k in 1:l[i]){
      ind <- k + sum(l[0:(i-1)])
      lines(MX[ind,], MY[ind,], col = alpha(i, 0.5))
    }
  }
  dev.off()
  print(paste("Saved in", file.path(folderOut,fplot,paste(title, "_clusters_content_variable_", variable_name, ".png", sep = "")), sep = " "))
}







i = 4
j = 3

hist(cluster[[i]][,j], breaks, xlim = c(0,max(1,max(cluster[[i]][,j]))), freq = T, main = paste("Cluster ", i, "; ", names(cluster[[i]][j]), sep = ""), xlab = "")

hist(cluster[[i]][,j], breaks, xlim = c(0,max(1,max(cluster[[i]][,j]))), main = paste("Cluster ", i, "; ", names(cluster[[i]][j]), sep = ""), xlab = "")


clusterCarac <- function(cluster, folderOut, fplot, title = NA, breaks = 10){
  nrows = length(cluster)
  ncols = ncol(cluster[[1]])
  png(file=file.path(folderOut,fplot,paste(title,"clusters_content.png",sep = "_")), width=200*ncols, height=200*nrows)
  par(mfrow = c(nrows, ncols))
  for(i in 1:nrows){
    for(j in 1:ncols){
      hist(cluster[[i]][,j], breaks, xlim = c(0,max(1,max(cluster[[i]][,j]))), freq = T, main = paste("Cluster ", i, "; ", names(cluster[[i]][j]), sep = ""), xlab = "")
    }
  }
  dev.off()
  print(paste("Saved in", file.path(folderOut,fplot,paste(title,"clusters_content.png",sep = "_")),sep = " "))
}




clusterCarac(test, folderOut, fplot, title = "test2")

clusterCarac(test, folderOut, fplot, title = "test2a", freqOpt = F)




compare_distrib <- function(distrib1,distrib2){
  test <- FALSE
  model1 <- model_select(distrib1)
  model2 <- model_select(distrib2)
  name1 <- attributes(model1)$model
  name2 <- attributes(model2)$model
  if(name1 == name2){
    test <- TRUE
    res <- c(model1[1] - model2[1],model1[2] - model2[2],(model1[1] - model2[1])/model1[1])
  } else{
    #print("Not comparable")
    res <- c(NA,NA,NA)
  }
  names(res) <- c("mean_diff","sd_diff","rel_diff")
  return(res)
}



test_method <- function(n,num,param1,param2,param3,param4){
  res <- rep(NA,n)
  for(i in 1:n){
    if(i%%(n/20) == 0){
      print(paste(i*100/n,"%",sep = " "))
    }
    distrib1 <- rnorm(num,param1,param2)
    distrib2 <- rnorm(num,param3,param4)
    a <- compare_distrib(distrib1,distrib2)
    res[i] <- a[1]
  }
  return(length(which(is.na(res))) / length(res))
}

x <- seq(500,2000,500)
prop <- rep(NA,length(x))
for(i in 1:length(x)){
  print(paste("Parameter ",i,"/",length(x),": ",x[i],sep = ""))
  prop[i] <- test_method(500,x[i],100,20,50,10)
}
prop


471400 / 1146800

hist(distrib1)
hist(distrib2)
  

fit <- model_select(test)

names(fit)

attributes(fit)$model

dir.create("Data/")
dir.create("Data/dl")
dir.create("Output/")
dir.create("Output/test")

folderIn <- "Data/dl/"
folderOut <- "Output/test"
APIKey <- read_file("nomisAPIKey.txt")
options(timeout=600)



createURL <- function(dataset,geogr,APIKey,date,other){
  url <- paste("https://www.nomisweb.co.uk/api/v01/dataset/",dataset,".data.csv?uid=",APIKey,"&geography=",geogr,"&date=",date,other,sep = "")
  return(url)
}


test <- rnorm(1000,10,2)
hist(test)

x <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

fit <- fitDist(x, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)


descdist(test, discrete = FALSE)

fit <- fitDist(test, k = 1, type = "realplus")





compare_distrib <- function(distrib1,distrib2){
  
}




############################
##### Read protobuffer #####
############################


############################
##### Static variables #####
############################

# Sex
# Age
# Ethinicity
# Nssec8
# Salaries
# BMI


#####################
##### Commuting #####
#####################

# Caveats: Based on 2011 census, so outdated data in terms of total flow volume. Assumption is that the MSOA to LSOA distribution would remain proportional.
# Status of new data: 2021 unreleased, will be mostly irrelevant due to lockdowns.









