


#### error approach
# 1. Find n most common distributions
# 2. Get error (max deviance when validating from training) compared to each most common distributions
# 3. Plot in n-dimensional plot
# 4. Cluster within that plot
# Bonus: entropy of the plot
####

#### distrib characs approach
# repeat with plot directly form moments


n <- 10000

test <- rnorm(n,200,40)
test <- rweibull(n, shape = 10, scale = 4)
test <- rexGAUS(n, mu = 200, sigma = 40)
hist(test,breaks = 40)

rand <- c(rep(1,floor(n/2)),rep(2,n - floor(n/2)))

a <- fitDist(test, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE) # generalised Akaike information criterion
b <- fitDistPred(test, k = 2, type = "realplus", trace = FALSE, rand = rand, try.gamlss = TRUE) # minimum prediction global deviance.

a$fits
b$fits

a$failed
b$failed



##########
rm(z)
rm(abdom)

plot(abdom$x,abdom$y)

x

##########
#---------------------------------------  
# Example of using the argument extra  
library(gamlss.tr)
data(tensile)
gen.trun(par=1,family="GA", type="right")
gen.trun(par=1,"LOGNO", type="right")
gen.trun(par=c(0,1),"TF", type="both")
ma<-fitDist(str, type="real0to1", trace=T,
            extra=c("GAtr", "LOGNOtr", "TFtr"), 
            data=tensile) 
ma$fits
ma$failed
#-------------------------------------
# selecting model using the prediction global deviance
# Using fitDistPred
# creating training data
y <- rt(1000, df=2)
m1 <- fitDist(y, type="realline")
m1$fits
m1$fails
# create validation data
yn <- rt(1000, df=2)
# choose distribution which fits the new data best
p1 <- fitDistPred(y, type="realline", newdata=yn)
p1$fits
p1$failed
#---------------------------------------
# using the function chooseDist()
# fitting normal distribution model
m1 <- gamlss(y~pb(x), sigma.fo=~pb(x), family=NO, data=abdom)
# choose a distribution on the real line 
# and save GAIC(k=c(2,4,6.4),  i.e. AIC, Chi-square and BIC.   
t1 <- chooseDist(m1, type="realline", parallel="snow", ncpus=4)
# the GAIC's
t1
# the distributions which failed are with NA's 
# ordering according to  BIC
getOrder(t1,3)
fm<-update(m1, family=names(getOrder(t1,3)[1]))

.gamlss.sm.list

pb(abdom$x)

data(abdom)

## End(Not run)

##########

m1 <- gamlss(test ~ pb(x), sigma.fo=~pb(x), family=NO, data=abdom)
t1 <- chooseDist(m1, type="realline", parallel="snow", ncpus=4)

chooseDist(a, type = "realplus")

getOrder()



chooseDist(a, type = "extra", extra=c("GA", "IG", "GG"))

fitDist(test, k = 2, type = "extra", trace = FALSE, try.gamlss = TRUE, extra=c("GA", "IG", "GG"))

extra=c("GA", "IG", "GG")



res <- dstrb_info(test)

typeof(res@family)



res@family[2]

res@family
res@params
res@moments
res@plot



test_gamlss <- function(n,mean,sd,k = 100){
  resF <- rep(NA,k)
  for(i in 1:k){
    if(i %% 10 == 0){
      print(paste(i,"/",k))
    }
    test <- rnorm(n,mean,sd)
    options(warn = -1)
    res <- dstrb_info(test)
    resF[i] <- res@family[2]
  }
  return(resF)
}
test_gamlss_wei <- function(n,shape,scale,k = 100){
  resF <- rep(NA,k)
  for(i in 1:k){
    if(i %% 10 == 0){
      print(paste(i,"/",k))
    }
    test <- rweibull(n, shape = 10, scale = 4)
    options(warn = -1)
    res <- dstrb_info(test)
    resF[i] <- res@family[2]
  }
  return(resF)
}

resF1 <- test_gamlss(1000,100,2)
resF2 <- test_gamlss(1000,100,20)
resF3 <- test_gamlss(10000,100,2)
resF4 <- test_gamlss(10000,100,20)

dev.off()
par(mar=c(17,5,2,1),mfrow=c(2,2))
barplot(table(resF1), las=2, main = "n = 1000, mean = 100, sd = 2")
barplot(table(resF2), las=2, main = "n = 1000, mean = 100, sd = 20")
barplot(table(resF3), las=2, main = "n = 10000, mean = 100, sd = 2")
barplot(table(resF4), las=2, main = "n = 10000, mean = 100, sd = 20")



resF1W <- test_gamlss_wei(1000,10,2)
resF2W <- test_gamlss_wei(1000,10,4)
resF3W <- test_gamlss_wei(10000,10,2)
resF4W <- test_gamlss_wei(10000,10,4)

dev.off()
par(mar=c(17,5,2,1),mfrow=c(2,2))
barplot(table(resF1W), las=2, main = "n = 1000, shape = 10, scale = 2")
barplot(table(resF2W), las=2, main = "n = 1000, shape = 10, scale = 4")
barplot(table(resF3W), las=2, main = "n = 10000, shape = 10, scale = 2")
barplot(table(resF4W), las=2, main = "n = 10000, shape = 10, scale = 4")


################

library(gamlss)

op <- par(mfrow=c(3,1))
z<-seq(-4,4,by=0.01)
plot(dNO(z, mu=0, sigma=1)~z, type="l",col="black",ylab="f_Z1(z)")
plot(2*pNO(z, mu=0, sigma=0.001)~z, type="l",col="blue",ylab="2*F_Z2(z)",lt=1)
lines(2*pNO(z, mu=0, sigma=0.5)~z, col="red",lt=2,lw=2)
lines(2*pNO(z, mu=0, sigma=1)~z, col="green",lt=4,lw=2)
lines(2*pNO(z, mu=0, sigma=10000)~z, col="black",lt=3,lw=2)
plot(dSEP1(z, mu=0, sigma=1, nu=1000, tau=2)~z, type="l",col="blue",
     ylab="f_Z(z)",lt=1)
lines(dSEP1(z, mu=0, sigma=1, nu=2, tau=2)~z, col="red", lt=2, lw=2)
lines(dSEP1(z, mu=0, sigma=1, nu=1, tau=2)~z, col="green",lt=4,lw=2)
lines(dSEP1(z, mu=0, sigma=1, nu=0, tau=2)~z, col="black",lt=3,lw=2)
par(op)


p <- 229
round(p/370*100,0)


#####
##### Outdated
#####


################
##### Data #####
################


x <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)


#################################
##### fitdistrplus analysis #####
#################################


descdist(x, discrete = FALSE)

fit.weibull <- fitdist(x, "weibull")
fit.norm <- fitdist(x, "norm")

plot(fit.norm)
plot(fit.weibull)

fit.weibull$aic
fit.norm$aic


##############################################
##### Kolmogorov-Smirnov test simulation #####
##############################################


n.sims <- 5e4

stats <- replicate(n.sims, {      
  r <- rweibull(n = length(x)
                , shape= fit.weibull$estimate["shape"]
                , scale = fit.weibull$estimate["scale"]
  )
  estfit.weibull <- fitdist(r, "weibull") # added to account for the estimated parameters
  as.numeric(ks.test(r
                     , "pweibull"
                     , shape= estfit.weibull$estimate["shape"]
                     , scale = estfit.weibull$estimate["scale"])$statistic
  )      
})

plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
grid()

# p-value
fit <- logspline(stats)

1 - plogspline(ks.test(x
                       , "pweibull"
                       , shape= fit.weibull$estimate["shape"]
                       , scale = fit.weibull$estimate["scale"])$statistic
               , fit
)

# Pointwise confidence intervals
xs <- seq(10, 65, len=500)

true.weibull <- rweibull(1e6, shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])

boot.pdf <- sapply(1:1000, function(i) {
  xi <- sample(x, size=length(x), replace=TRUE)
  MLE.est <- suppressWarnings(fitdist(xi, distr="weibull"))  
  dweibull(xs, shape=MLE.est$estimate["shape"],  scale = MLE.est$estimate["scale"])
}
)

boot.cdf <- sapply(1:1000, function(i) {
  xi <- sample(x, size=length(x), replace=TRUE)
  MLE.est <- suppressWarnings(fitdist(xi, distr="weibull"))  
  pweibull(xs, shape= MLE.est$estimate["shape"],  scale = MLE.est$estimate["scale"])
}
)   

# Plot PDF
par(bg="white", las=1, cex=1.2)
plot(xs, boot.pdf[, 1], type="l", col=rgb(.6, .6, .6, .1), ylim=range(boot.pdf),
     xlab="x", ylab="Probability density")
for(i in 2:ncol(boot.pdf)) lines(xs, boot.pdf[, i], col=rgb(.6, .6, .6, .1))

# Add pointwise confidence bands
quants <- apply(boot.pdf, 1, quantile, c(0.025, 0.5, 0.975))
min.point <- apply(boot.pdf, 1, min, na.rm=TRUE)
max.point <- apply(boot.pdf, 1, max, na.rm=TRUE)
lines(xs, quants[1, ], col="red", lwd=1.5, lty=2)
lines(xs, quants[3, ], col="red", lwd=1.5, lty=2)
lines(xs, quants[2, ], col="darkred", lwd=2)

# Plot CDF
par(bg="white", las=1, cex=1.2)
plot(xs, boot.cdf[, 1], type="l", col=rgb(.6, .6, .6, .1), ylim=range(boot.cdf),
     xlab="x", ylab="F(x)")
for(i in 2:ncol(boot.cdf)) lines(xs, boot.cdf[, i], col=rgb(.6, .6, .6, .1))

# Add pointwise confidence bands
quants <- apply(boot.cdf, 1, quantile, c(0.025, 0.5, 0.975))
min.point <- apply(boot.cdf, 1, min, na.rm=TRUE)
max.point <- apply(boot.cdf, 1, max, na.rm=TRUE)
lines(xs, quants[1, ], col="red", lwd=1.5, lty=2)
lines(xs, quants[3, ], col="red", lwd=1.5, lty=2)
lines(xs, quants[2, ], col="darkred", lwd=2)
#lines(xs, min.point, col="purple")
#lines(xs, max.point, col="purple")


##################
##### gamlss #####
##################


library(gamlss)
library(gamlss.dist)
library(gamlss.add)

x <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

fit <- fitDist(x, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)

summary(fit)



###########
###########
###########

library("univariateML")

model_select(test)