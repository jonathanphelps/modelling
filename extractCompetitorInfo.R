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


selTempcol <- 'MeanTemperature_Celsius'
subsetFlagSet <- TRUE

trimPercentSymbol <- function(x){
  return(gsub('%','',x))
}

topN <- function(a,howmanyentries){
  return(tail(sort(a),howmanyentries))
}

#extractCompetitorInfo <- function(df,curr.metric="Avg..position",N.players=4){
  #df <- bm.auction.df
  #metrics <- colnames(df)
  
  #curr.metric <- metrics[4]
  
  
  ser.col <- c("Day","Display.URL.domain",curr.metric)
  metric.df <- df[,ser.col]
  
  metric.df$variable <- "observed"
  colnames(metric.df)[2:3] <- c("domain","value")
  # Need to do - cast data frames to split them by year
  
  if(curr.metric != "Avg..position") {
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
    m.df <- subset(t.metric.df,Day>=as.Date("2015-01-01"))
    you.df <- subset(m.df, select=c(You))
    m.df <- subset(m.df, select=-c(You))
    avail.dates <- m.df$Day
    m.df <- subset(m.df,select=-c(Day))
    
    no.competitors <- zoo(as.integer(apply((!is.na(m.df))*1,1,sum)),order.by = avail.dates)
    
    position.competitors <- cbind.data.frame(avail.dates,you.df,no.competitors)
    
  # Relatonship between No.Competitors and Impressions
    
    no.competitors.df <- as.data.frame(no.competitors)
    no.competitors.df$Day <- as.Date(rownames(no.competitors.df),format="%Y-%m-%d")
    data.df$Day <- as.Date(data.df$Day)
    data.df <- subset(data.df,Day>=as.Date("2015-01-01"))
    impressions.competitors <- merge(no.competitors.df,data.df,by='Day')
  
    # Relatonship between WeekNo, Position and Impressions
    impressions.competitors$weekno <- as.factor(week(impressions.competitors$Day))
    impressions.competitors$discrete.Impressions <- as.factor(discretize(impressions.competitors$Impressions)[,1])
    
    ggplot(impressions.competitors, aes(x=weekno,y=Avg..position), 
                 colour=discrete.Impressions) +  
      geom_point(aes(colour=discrete.Impressions),size = 4)+ 
      xlab('Week') + ylab('Position') +
      scale_color_brewer(palette="Spectral")
    
    # Relatonship between Weather, Demand and Position
    
    serDate <- impressions.competitors$Day
    w.Decomp <- WeatherDecompositionForUK(serDate,selTempcol)
    
    decomp.Data <- w.Decomp$d.comp
    colnames(decomp.Data)[grep("all_dates",colnames(decomp.Data))] <- "Day"
    
    search.weather <- merge(impressions.competitors,decomp.Data,by="Day")
    search.weather$discrete.weather <- as.factor(discretize(search.weather$observed)[,1])

    search.weather$discrete.HFweather <- as.factor(discretize(search.weather$remainder)[,1])
    
    
    ggplot(search.weather, aes(x=weekno,y=Avg..position), 
           colour=discrete.weather) +  
      geom_point(aes(colour=discrete.weather),size = 4)+ 
      xlab('Week') + ylab('Position') +
      scale_color_brewer(palette="Spectral")
    
    f1 <- search.weather$discrete.weather
    f2 <- search.weather$discrete.Impressions
    search.weather$interaction <- as.factor(as.numeric(levels(f1))[f1] * as.numeric(levels(f2))[f2])
    
    ggplot(search.weather, aes(x=weekno,y=Avg..position), 
           colour=interaction) +  
      geom_point(aes(colour=interaction),size = 4)+ 
      xlab('Week') + ylab('Position')
    
    search.weather$isweekend <- as.factor(chron::is.weekend(search.weather$Day) * 1)
    search.weather$weekday <- wday(search.weather$Day, label=TRUE)
    
    
    