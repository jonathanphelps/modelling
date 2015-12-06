convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  return(df)
}

get.trajectory <- function(X.k,ts,acq.freq) {
  
  N   <- length(ts)
  i   <- complex(real = 0, imaginary = 1)
  x.n <- rep(0,N)           # create vector to keep the trajectory
  ks  <- 0:(length(X.k)-1)
  
  for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
    x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
  }
  
  return(x.n * acq.freq)
}

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory/10, type="l", col=color)
}


plot.show <- function(trajectory, time=1, harmonics=-1, plot.freq=FALSE) {
  
  if(plot.freq) par(mfrow=c(2,1))
  acq.freq <- length(trajectory)/time      # data acquisition frequency (Hz)
  ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
  
  X.k <- fft(trajectory)
  x.n <- get.trajectory(X.k,ts, acq.freq=acq.freq) / acq.freq
  
  if (plot.freq)
    plot.frequency.spectrum(X.k)
  
  max.y <- ceiling(1.5*max(Mod(x.n)))
  
  if (harmonics[1]==-1) {
    min.y <- floor(min(Mod(x.n)))-1
  } else {
    min.y <- ceiling(-1.5*max(Mod(x.n)))
  }
  
  #plot(ts,x.n, type="l",ylim=c(min.y,max.y))
  #abline(h=min.y:max.y,v=0:time,lty=3)
  plot(ts,trajectory,type='l',col="red",ylim=c(min.y,max.y))  # the data points we know
  
  if (harmonics[1]>-1) {
    for(i in 0:length(harmonics)) {
      plot.harmonic(X.k, harmonics[i], ts, acq.freq, color=i+1)
    }
  }
}
#----------------------------------------------------------------------------------------------------------------

plot.fourier <- function(fourier.series, f.0, ts) {
  w <- 2*pi*f.0
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}