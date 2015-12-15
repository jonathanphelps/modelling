require(dplyr)
require(data.table)
require(lubridate)
require(ggplot2)
require(forecast)

# Get all the config parameters
source("config.R")
source("dfHelper.R")
source("funcQueryDB.R")
source("tsHelper.R")
if(usemaxcpc) source("idsWithAvailableMaxCPCData.R")


if(useweather){
  
  if(!exists("w.df")){
    source("WeatherDecompositionForLocation.R")
    
    w.data <- WeatherDecompositionComplete(selWeatherCol)
    w.df<-cbind.data.frame(w.data$d.comp$all_dates,w.data$d.comp$observed,w.data$d.comp$level_plus_season,
                           w.data$d.comp$remainder)
    
    colnames(w.df) <- c("Date","observed.weather","level_plus_season","remainder")
    lastWeatherDate <- w.df$Date[nrow(w.df)]
  }
}

if(!loadData){
  cat(sprintf("\n Reading Files.... \n"))
  keyword.data.files <- dir(directory, recursive=TRUE, full.names=TRUE, pattern=file.pattern)
  
  temp.df <- data.frame()
  
  r.df <- data.frame()
  for(i in 1:length(keyword.data.files)){
    temp.df <- fread(keyword.data.files[i],stringsAsFactors=FALSE,integer64 = "character")
    r.df <- rbind.data.frame(r.df,temp.df)
  }
  
  r.df <- makeColNamesUserFriendly(r.df)
  
  r.df$LandingDate <- as.Date(r.df$LandingDate,format="%d/%m/%Y")
  data.df <- r.df %>% group_by(DCSListingID,Device) %>% arrange(DCSListingID,Device,LandingDate)
  #data.df <- data.df %>% ungroup()
  
  
  columnsAsNumeric <- function(df){
    allColumns <- colnames(df)
    for(c in 1:length(numericColumnNames)){
      ix <- match(numericColumnNames[c],allColumns)
      if(!is.na(ix)) df[,ix] <- as.numeric(df[,ix])
    }
    return(df)
  }
  
  columnsAsInt <- function(df){
    allColumns <- colnames(df)
    for(c in 1:length(integerColumns)){
      ix <- match(integerColumns[c],allColumns)
      if(!is.na(ix)) df[,ix] <- as.integer(df[,ix])
    }
    return(df)
  }
  
  data.df <- data.frame(data.df)
  
  data.df <- columnsAsNumeric(data.df)
  data.df <- columnsAsInt(data.df)
  data.df <- subset(data.df,Device %in% deviceType)
  
  data.df <- subset(data.df,DCSListingID!="0")
  master.data <- data.df
  data.df <- NULL
  save(master.data,file=masterFileName)
} else{
  if(!exists(masterFileName)) load(masterFileName)
}

#data.df <- subset(data.df,DCSListingID>0)

dcs.Sales <- master.data %>% group_by(DCSListingID) %>% summarise(NSales=sum(UniqueSales))
sale.keywords <- dcs.Sales$DCSListingID[which(dcs.Sales$NSales>0)]
nonsale.keywords <- dcs.Sales$DCSListingID[which(dcs.Sales$NSales==0)]

# Consider only the sale keywords
#master.data <- subset(data.df,DCSListingID %in% sale.keywords)

dcs.counts <- master.data %>% group_by(DCSListingID) %>% tally(sort=T)
# dcs.NImpressions <- master.data %>% group_by(DCSListingID) %>% summarise(No.Impressions=sum(Impressions))
# dcs.NClicks <- master.data %>% group_by(DCSListingID) %>% summarise(No.Clicks=sum(Clicks))
# dcs.NSales <- master.data %>% group_by(DCSListingID) %>% summarise(No.Sales=sum(Sales))
# 
# 
# dcs.stats <- merge(dcs.NClicks,dcs.NImpressions,by="DCSListingID")
# dcs.stats <- merge(dcs.stats,dcs.NSales,by="DCSListingID")


frequently.occuring.dcs <- dcs.counts$DCSListingID[which(dcs.counts$n>quantile(dcs.counts$n,occurence.quantile))]
frequent.counts <- subset(dcs.counts,dcs.counts$DCSListingID %in% frequently.occuring.dcs)
frequent.counts <- frequent.counts %>% arrange(desc(n))

sparsely.occuring.dcs <- dcs.counts$DCSListingID[which(dcs.counts$n<quantile(dcs.counts$n,0.1))]

#################################################################################################

if(usemaxcpc) {
  rand.i <- which(frequent.counts$DCSListingID %in% maxcpc.listing.ids)
} else {
  rand.i <- sample(1:nrow(frequent.counts),Ntotal)
}

if(enableKWMeasures){
  keyword.measures <- matrix(0,nrow=length(rand.i),ncol=13)
  measure.names <- c("frequency","trend","seasonal","autocorrelation","non-linear",     
                     "skewness","kurtosis","Hurst","Lyapunov","dc autocorrelation",
                     "dc non-linear","dc skewness","dc kurtosis")
}

all.kw.names <- c()
dominant.periods <- list()

for(k in 1:length(rand.i)){
  current.dcs <- frequent.counts$DCSListingID[rand.i[k]]
  
  model.data <- subset(master.data,DCSListingID == current.dcs)
  kw.name <- unique(model.data$Keyword)
  cat(sprintf("\n Keyword is: %s",kw.name))
  adgroup.name <- unique(model.data$AdGroup)
  matchtype <- unique(model.data$MatchType)
  
  listing.id <- as.integer(current.dcs)
  
  model.data <- model.data %>% arrange(LandingDate)
  
  
  model.data <- subset(model.data,select=select.master.columns)
  
  if(("Computer" %in% deviceType) & ("Tablet" %in% deviceType)){
    model.data.computer <- subset(model.data,Device == "Computer")
    model.data.tablet <- subset(model.data,Device == "Tablet")
    model.data <- subset(model.data,select=-c(Device,AvgPosition)) 
    model.data.computer <- subset(model.data.computer,select=-c(Device))
    model.data.tablet <- subset(model.data.tablet,select=-c(Device))
    
    model.data <- model.data %>% group_by(LandingDate) %>% 
      summarise(Impression=sum(Impressions),Clicks=sum(Clicks),Sales=sum(UniqueSales))
    
    if((nrow(model.data.tablet) + nrow(model.data.computer)) < 2*nrow(model.data)){
      full <- data.frame(model.data$LandingDate)
      colnames(full)[1] <- "LandingDate"
      model.data.tablet <- merge(full,model.data.tablet,by="LandingDate",all.x = T)
      model.data.computer <- merge(full,model.data.computer,by="LandingDate",all.x = T)
      
      model.data.tablet$Impressions[is.na(model.data.tablet$Impressions)] <- 0
      model.data.tablet$Clicks[is.na(model.data.tablet$Clicks)] <- 0
      model.data.tablet$UniqueSales[is.na(model.data.tablet$UniqueSales)] <- 0
      model.data.tablet$AvgPosition[is.na(model.data.tablet$AvgPosition)] <- 1
      
      model.data.computer$Impressions[is.na(model.data.computer$Impressions)] <- 0
      model.data.computer$Clicks[is.na(model.data.computer$Clicks)] <- 0
      model.data.computer$UniqueSales[is.na(model.data.computer$UniqueSales)] <- 0
      
      model.data.computer$AvgPosition[is.na(model.data.computer$AvgPosition)] <- 1
      
    }
    
    # Aggregating model.data
    
    model.data.computer$pos.imp <- model.data.computer$AvgPosition * model.data.computer$Impressions
    model.data.tablet$pos.imp <- model.data.tablet$AvgPosition * model.data.tablet$Impressions
    
    combined.Impressions <- model.data.computer$Impressions + model.data.tablet$Impressions
    combined.pos.imp <- model.data.computer$pos.imp + model.data.tablet$pos.imp
    
    model.data$AvgPosition <- combined.pos.imp / combined.Impressions
    
  }
  
  model.data$CTR <- model.data$Clicks/model.data$Impression
  #model.data$ConversionRate <- model.data$Sales/model.data$Clicks
  model.data$CTR[is.na(model.data$CTR)] <- 0
  model.data$CTR[is.infinite(model.data$CTR)] <- 0
  #model.data$ConversionRate[is.na(model.data$ConversionRate)] <- 0
  #model.data$ConversionRate[is.infinite(model.data$ConversionRate)] <- 0
  
  #model.data$CPC <- model.data$Cost/model.data$Clicks
  
  colnames(model.data)[1] <- "Date"
  
  #-----------------------------------------------------
  # Padding time series and replace missing data with 0s
  #-----------------------------------------------------
  
  target <- data.frame(Date = seq(from = model.data$Date[1], to = model.data$Date[nrow(model.data)], by = "day"))
  
  model.data <- merge(model.data,target,by="Date",all.y = T)
  
  model.data[which(complete.cases(model.data) == F),2:ncol(model.data)] <- 0
  
  #----------------------------
  # Merging with weather data
  #----------------------------
  
  if(useweather) {
    model.data <- merge(model.data,w.df,by="Date",all.x = T)
    
    model.data <- model.data %>% filter(Date <= lastWeatherDate)
  }
    
  if(useweek) model.data$weekno <- week(model.data$Date)
  if(usedow) model.data$dow <- wday(model.data$Date)
  
  #----------------------------
  # Merging with max cpc
  #----------------------------
  
  if(usemaxcpc) {
    load(paste("./Data/MaxcpcData/maxcpc","-",listing.id,"-",device.type,".RData",sep=""))
    model.data <- merge(model.data,maxcpc.df,by="Date",all.x = T)
    model.data$hour.times.minute <- NULL
    first.ix <- which(!(is.na(model.data$MaxCpc))==T)[1]
    model.data <- model.data[first.ix:nrow(model.data),]
    model.data$MaxCpc <- na.lomf(model.data$MaxCpc)
  }
  
  #----------------------------
  # Plot data if enabled
  #----------------------------
  
  
  if(plotData) {
    par(mfrow = c(4,2))
    
    # Plot impressions
    
    plt.dates <- model.data$Date
    ticks <- seq(plt.dates[1], plt.dates[length(plt.dates)], by = "months")
    data_zoo <- zoo(model.data$Impression,order.by = plt.dates)
    
    plot(data_zoo,main="",xlab="",
         ylab = 'Impressions',type='b',col='blue',xaxt='n')
    title(paste("Median-",as.character(round(median(model.data$Impression),3)),sep=""), line = -2)
    axis(1,at=ticks,labels=ticks,tcl=-0.3)
    
    # Plot from fourier analysis after trend removal
    
    #     trend <- lm(Impression ~ index(Date), data = model.data)
    #     tdata <- trend$residuals
    #     
    #     #lines(plt.dates,trend$fitted.values, col="red",lwd=3)
    #     
    #     f.data <- GeneCycle::periodogram(tdata)
    #     harmonics <- 1:(nrow(model.data)/2) 
    #     plot(f.data$freq[harmonics]*length(tdata), 
    #          f.data$spec[harmonics]/sum(f.data$spec), 
    #          xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")
    #     
    
    plot(zoo(model.data$level_plus_season,order.by = plt.dates),main="",xlab="",
         ylab = 'Seasonal weather',type='b',col='red',xaxt='n')
    title(paste("Median-",as.character(round(median(model.data$AvgPosition),3)),sep=""), line = -2)
    axis(1,at=ticks,labels=ticks,tcl=-0.3)
    
    acf(model.data$Impression,400,main="Impressions")
    
    ccf(model.data$AvgPosition,model.data$Impression,main="Position v Impressions")
    
    plot(zoo(model.data$AvgPosition,order.by = plt.dates),main="",xlab="",
         ylab = 'Position',type='b',col='blue',xaxt='n')
    title(paste("Median-",as.character(round(median(model.data$AvgPosition),3)),sep=""), line = -2)
    axis(1,at=ticks,labels=ticks,tcl=-0.3)
    
    #acf(model.data$Impression,370,main="Impressions")
    
    plot(model.data$Impression~model.data$AvgPosition,type="p",col='green',xlab="Position",ylab="Impressions",main="")
    
    plot(zoo(model.data$CTR,order.by = plt.dates),main="",xlab="",
         ylab = 'CTR',type='b',col='blue',xaxt='n')
    title(paste("Median-",as.character(round(median(model.data$CTR),3)),sep=""), line = -2)
    axis(1,at=ticks,labels=ticks,tcl=-0.3)
    
    if(usemaxcpc){
      plot(zoo(model.data$MaxCpc,order.by = plt.dates),main="",xlab="",
           ylab = 'Max.CPC',type='b',col='blue',xaxt='n')
      title(paste("Median-",as.character(round(median(model.data$CTR),3)),sep=""), line = -2)
      axis(1,at=ticks,labels=ticks,tcl=-0.3)
      
      ccf(model.data$MaxCpc,model.data$Impression,main="MaxCPC v Impression")
    }
    
    #     plot(zoo(model.data$ConversionRate,order.by = plt.dates),main="",xlab="",
    #          ylab = 'Conv.Rate',type='b',col='blue',xaxt='n')
    #     axis(1,at=ticks,labels=ticks,tcl=-0.3)
    #     
    #     plot(zoo(model.data$CPC,order.by = plt.dates),main="",xlab="",
    #          ylab = 'CPC',type='b',col='blue',xaxt='n')
    #     axis(1,at=ticks,labels=ticks,tcl=-0.3)
    
    
    
    title(paste(kw.name,"-",matchtype," (",adgroup.name,") ",sep="",collapse = ""),line = -2, outer = TRUE)
    
    p.f.name <- paste(getwd(),"./plots/",kw.name,".tiff",sep="")
    dev.copy(tiff,p.f.name, width=1100, height=900)
    dev.off()
  }
  
  i.ix <- pmatch("Impression",colnames(model.data))
  if(!is.na(i.ix)) colnames(model.data)[i.ix] <- "Impression"
  
  model.data <- subset(model.data,select=model.columns)
  
  source("trialVariousImpressionModels.R")
  
  if(enableKWMeasures) keyword.measures[k,] <- measures(model.data$Impression)
  
  all.kw.names <- c(all.kw.names,kw.name)
  
  #dominant.periods[[k]] <- dominantPeriods(model.data)
  
}

if(enableKWMeasures){
  keyword.measures <- data.frame(keyword.measures)
  colnames(keyword.measures) <- measure.names
  keyword.measures$keyword <- all.kw.names
}
#keyword.measures <- keyword.measures %>% arrange(keyword)
