library(scales)
library(ggthemes)
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(chron)
library(infotheo)

source("fillSeries.r")
source("WeatherDecompositionForLocation.R")
source("dfHelper.R")


plotData <- FALSE
modelData <- TRUE


selTempcol <- 'MeanTemperature_Celsius'
subsetFlagSet <- TRUE
curr.metric <- "AvgPosition"
N.players <- 5


trimPercentSymbol <- function(x){
  return(gsub('%','',x))
}

topN <- function(a,howmanyentries){
  return(tail(sort(a),howmanyentries))
}

#extractCompetitorInfo <- function(df,curr.metric="Avgposition",N.players=4){
#df <- bm.auction.df
#metrics <- colnames(df)

#curr.metric <- metrics[4]

auction.curr.metric.ix <- match(tolower("AvgPosition"),tolower(colnames(auction.df)))
colnames(auction.df)[auction.curr.metric.ix] <- curr.metric

ser.col <- c("Day","DisplayURLdomain",curr.metric)


if(plotData){
  metric.df <- auction.df[,ser.col]
  
  metric.df$variable <- "observed"
  colnames(metric.df)[2:3] <- c("domain","value")
  # Need to do - cast data frames to split them by year
  
  if(curr.metric != "AvgPosition") {
    metric.df$value <- as.double(sapply(metric.df$value,trimPercentSymbol))
  }else{
    metric.df$value <- as.double(metric.df$value)
  }
  
  metric.df$Day <- as.Date(metric.df$Day,format="%d/%m/%Y")
  t.metric.df <- data.frame(acast(metric.df,Day~domain~variable)[,,1])
  t.metric.df$Day <- rownames(t.metric.df)
  rownames(t.metric.df) <- seq(1,nrow(t.metric.df))
  
  
  # Trends in chosen metric for the most frequently occuring retailer
  
  flags <- (!is.na(t.metric.df))*1
  total.occurences <- apply(flags,2,sum)
  #Remove Day
  total.occurences <- total.occurences[names(total.occurences)!="Day"]
  topNPlayers <- topN(total.occurences,N.players)
  topPlayerNames <- c(names(topNPlayers),"Day")
  
  top.player.df <- t.metric.df[,colnames(t.metric.df) %in% topPlayerNames]
  observed.Dates <- top.player.df$Day
  
  top.player.df <- top.player.df[,colnames(top.player.df)!="Day"]
  topPlayerNames <- colnames(top.player.df)
  
  for(c in 1:ncol(top.player.df)){
    if(c==1) complete.data <- fillSeries(cbind.data.frame(observed.Dates,top.player.df[,c]))
    else {
      ser.Data <- fillSeries(cbind.data.frame(observed.Dates,top.player.df[,c]))
      complete.data <- merge(complete.data,ser.Data,by="time")
    }
  }
  
  colnames(complete.data)[2:ncol(complete.data)] <- topPlayerNames
  
  for(c in 2:ncol(complete.data)){
    #complete.data[,c] <- zoo::na.spline(complete.data[,c])
    complete.data[,c] <- forecast::na.interp(complete.data[,c])
    
  }
  
  # Plot of frequently occuring competitors
  
  complete.data.zoo <- zoo(complete.data[,2:ncol(complete.data)],order.by = as.POSIXct(complete.data$time))
  autoplot(complete.data.zoo,facets = Series ~ .) + ggtitle(curr.metric) + xlab('')+
    geom_line(size=1) +
    geom_point(size=0.5) + scale_x_datetime(breaks = "month", labels=date_format("%Y-%m"))+
    theme_gdocs() + 
    scale_color_gdocs() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #}
  
  # No. competitors per day in 2015
  m.df <- subset(t.metric.df,Day>=as.Date(history.start.date))
  you.df <- subset(m.df, select=c(You))
  m.df <- subset(m.df, select=-c(You))
  avail.dates <- m.df$Day
  m.df <- subset(m.df,select=-c(Day))
  
  no.competitors <- zoo(as.integer(apply((!is.na(m.df))*1,1,sum)),order.by = avail.dates)
  
  position.competitors <- cbind.data.frame(avail.dates,you.df,no.competitors)
  
  # Relatonship between No.Competitors and Impressions
  
  no.competitors.df <- as.data.frame(no.competitors)
  no.competitors.df$Day <- as.Date(rownames(no.competitors.df),format="%Y-%m-%d")
  impressions.competitors <- merge(no.competitors.df,data.df,by='Day')
  
  impressions.competitors$weekno <- as.factor(week(impressions.competitors$Day))
  impressions.competitors$discrete.Impressions <- as.factor(discretize(impressions.competitors$Impressions)[,1])
  #impressions.competitors$disrete.maxcpc <- as.factor(discretize(impressions.competitors$AvgMaxCPC)[,1])
  
  # Relatonship between WeekNo, Position and Impressions
  
  ggplot(impressions.competitors, aes(x=weekno,y=AvgPosition), 
         colour=discrete.Impressions) +  
    geom_point(aes(colour=discrete.Impressions),size = 4)+ 
    xlab('Week') + ylab('Position') +
    scale_color_brewer(palette="PuRd")
  
  # Relatonship between WeekNo, CPC and Impressions
  
  ggplot(impressions.competitors, aes(x=weekno,y=CPC), 
         colour=discrete.Impressions) +  
    geom_point(aes(colour=discrete.Impressions),size = 4)+ 
    xlab('Week') + ylab('CPC') +
    scale_color_brewer(palette="PuRd")
  
  
  # Relatonship between Weather, Demand and Position
  
  serDate <- impressions.competitors$Day
  w.Decomp <- WeatherDecompositionForUK(serDate,selTempcol)
  
  decomp.Data <- w.Decomp$d.comp
  colnames(decomp.Data)[grep("all_dates",colnames(decomp.Data))] <- "Day"
  
  search.weather <- merge(impressions.competitors,decomp.Data,by="Day")
  search.weather$discrete.weather <- as.factor(discretize(search.weather$observed)[,1])
  
  search.weather$discrete.HFweather <- as.factor(discretize(search.weather$remainder)[,1])
  
  
  ggplot(search.weather, aes(x=weekno,y=AvgPosition), 
         colour=discrete.weather) +  
    geom_point(aes(colour=discrete.weather),size = 4)+ 
    xlab('Week') + ylab('Position') +
    scale_color_brewer(palette="PuRd")
  
  f1 <- search.weather$discrete.weather
  f2 <- search.weather$discrete.Impressions
  search.weather$interaction <- as.factor(as.numeric(levels(f1))[f1] * as.numeric(levels(f2))[f2])
  
  ggplot(search.weather, aes(x=weekno,y=AvgPosition), 
         colour=interaction) +  
    geom_point(aes(colour=interaction),size = 4)+ 
    xlab('Week') + ylab('Position')
  
  search.weather$isweekend <- as.factor(chron::is.weekend(search.weather$Day) * 1)
  search.weather$weekday <- wday(search.weather$Day, label=TRUE)
  
  
  search.weather$no.competitors <- as.factor(search.weather$no.competitors)
  
}
if(modelData){
  
  # all.model.variables <- c("Day","Impressions","AvgPosition","CPC","AvgCPCBidGap",
  #                          "weekno","level_plus_season","remainder")
  # model.data <- search.weather[,all.model.variables]
  
  maxcpc.data.all$hour.times.minute <- NULL
  colnames(maxcpc.data.all)[1] <- "Day"
  
  model.data<-merge(data.df,maxcpc.data.all,by="Day",all = T)
  
  serDate <- model.data$Day
  w.Decomp <- WeatherDecompositionForUK(serDate,selTempcol)
  
  decomp.Data <- w.Decomp$d.comp
  colnames(decomp.Data)[grep("all_dates",colnames(decomp.Data))] <- "Day"
  
  
  model.data$weekno <- as.factor(week(model.data$Day))
  model.data <- merge(model.data,decomp.Data,by="Day",all = T)
  model.data <- model.data[complete.cases(model.data),]
}