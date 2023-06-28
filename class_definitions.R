library(gamlss)
library(fitdistrplus)

set.seed(18061815)

# /!\ /!\ /!\ NEEDED: refine family into several families with scores

# Create custom S4 class storing focused info
setClass("distrbinfo", slots=list(family="character", params = "numeric", moments="numeric", plot="recordedplot"))

# Extract and format info for one distribution
dstrb_info <- function(dstrb){
  fit <- invisible(fitDist(dstrb, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE))
  plot.new()
  plot <- descdist(dstrb, discrete = FALSE)
  p <- recordPlot()
  dev.off()
  res <- new("distrbinfo",
             family = fit$family, params = c(fit$mu.coefficients,fit$sigma.coefficients),
             moments = c(plot$mean,plot$sd,plot$skewness,plot$kurtosis), plot = p)
  return(res)
}


dstbr_compare(dstbr1,dstbr2){
  a
}

data("abdom")
abd9 <- gamlss( y~cs(x,df=3), sigma.formula=~cs(x,df=3), family=BCT, data=abdom)
centiles(abd9)
