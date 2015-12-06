library(forecast)
library(zoo)

# Decomposition of the time series

decomposeTS <- function(x) {
  y <- msts(x, seasonal.periods=365.25)
  fit <- tbats(y)
  return(fit)
}


trendsInSeries<- function(Data,st,ed,all_dates) {
  cts <- Data
  its <- zoo(cts,order.by = all_dates)
  ssits <- window(its,start=st,end=ed)
  decomp.it <- stl(ts(ssits,frequency = 365.25),
                   s.window = "periodic",na.action = na.contiguous)
  return(decomp.it)
  #decomp.it <- decomp.h(ts(ssits,frequency = 365.25))
}
