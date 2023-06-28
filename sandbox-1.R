#devtools::install_github("ld-archer/UKCensusAPI")
#library(UKCensusAPI)
library(fitdistrplus)
library(univariateML)

set.seed(18061815)


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









