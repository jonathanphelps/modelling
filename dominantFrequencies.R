library(ggplot2)
library(dplyr)
library(robfilter)

source("fourieranalysis.R")

dominantPeriods <- function(model.data,pvalue=0.01,select.quantile=0.99){
  i.ts <- model.data$Impression
  i.ts <- i.ts - mean(i.ts)
  i.dt <- model.data$Date
  res.test.stationary <- kpss.test(i.ts,null=c("Trend"))
  
  if(res.test.stationary$p.value <= pvalue){
    residual <- i.ts
  } else{
    stationary <- FALSE
    width <- seq(181,361,30)
    w <- 1
    while(stationary==FALSE){
      trialwidth <- width[w]
      y.rr <- hybrid.filter(i.ts,width=trialwidth,method = "CRMMH")
      residual <- (i.ts - y.rr$level)[[1]]
      res.test.stationary <- kpss.test(residual,null=c("Trend"))
      if(res.test.stationary$p.value <= pvalue) {
        stationary <- TRUE;
      } else { 
        w <- w + 1 
      }
    }
  }
  
  # Odd length series
  
  if((length(residual) %% 2) == 1) residual <- residual[1:(length(residual)-1)]
  #dt <- 1
  
  fft.res <- fft(residual)
  fft.res <- fft.res[1:(length(residual)/2)]
  
  f <- data.frame(coef = fft.res, freqindex = seq(1:length(residual)/2))
  #qplot(freqindex, Mod(coef), data = f[1:nrow(f),], geom = "line")
  
  dominant.per <- f[Mod(f$coef) > quantile(Mod(f$coef),select.quantile), "freqindex"] - 1
  
  #return(dominant.per[which(dominant.per<(length(residual)/2))])
  
  fft.df <- convert.fft(fft(residual)) %>% filter(freq>0)
  fft.df <- fft.df %>% arrange(desc(strength))
  fft.df <- fft.df %>% filter(cycle <= (nrow(fft.df)/2))
  
  common.ix <- which(fft.df$cycle %in% dominant.per)
  
  return(fft.df[common.ix,])
  
}
  
dominantPeriodsTimeDomain <- function(model.data,select.quantile=0.99,plot.res=FALSE){
  i.ts <- model.data$Impression
  i.ts <- i.ts - mean(i.ts)
  T <- length(i.ts)
  if((T %% 2)==1) i.ts <- i.ts[1:(T-1)]
  FF = abs(fft(i.ts)/sqrt(T))^2
  P = (4/T)*FF[1:((T/2)+1)] # Only need the first (n/2)+1 values of the FFT result.
  f = (0:(T/2))/T # this creates harmonic frequencies from 0 to .5 in steps of 1/128.
  
  if(plot.res){
    plot((f*T), P,col='red',type='p',xlab='time',ylab='importance') # This plots the periodogram; type = "l" creates a line plot.  Note: l is lowercase L, not number 1.
    abline(h=0)
    lines((f*T), P,col = 'black', lty = 4)
  }
  return((f*T)[which(P>=quantile(P,select.quantile))] )
}
  
#   midindex <- ceiling((length(f$coef)-1)/ 2)
#   lindex <- length(f$coef)
#   peakind <- f[abs(Mod(f$coef)) > quantile(Mod(f$coef),0.99) & f$freqindex > 1 & f$freqindex < midindex, ]
#   
#   lowerind <- 1
#   
#   subsignals <- lapply(c(peakind$freqindex, midindex+1), function(x){
#     upperind <- x
#     fsub <- f
#     notnullind <- ((fsub$freqindex >= lowerind
#                     & fsub$freqindex < upperind)
#                    |
#                      (fsub$freqindex >  (lindex - upperind + 2)
#                       & fsub$freqindex <= (lindex - lowerind + 2)))
#     fsub[!notnullind,"coef"] <- 0
#     lowerind <<- upperind
#     Re(fft(fsub$coef, inverse=TRUE)/length(fsub$coef))
#   })
#   
#   grid.newpage()
#   pushViewport(viewport(layout=grid.layout(3,2)))
#   
#   vplayout <- function(x,y)
#     viewport(layout.pos.row = x, layout.pos.col = y)
#   
#   psig <- function(x, y, z){
#     h <- data.frame(index = c(1:length(subsignals[[x]])),
#                     orders = subsignals[[x]])
#     lab <- paste("Subseries ", as.character(x), sep="")
#     print(qplot(index, orders, data = h, geom = "line", main=lab), vp = vplayout(y,z))
#     TRUE
#   }
#   
#   psig(1,1,1); psig(2,1,2); psig(3,2,1); psig(4,2,2); psig(5,3,1)