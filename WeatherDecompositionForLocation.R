library(zoo)
library(dplyr)
library(chron)
source("trimLocation.R")
source("Decomposition.R")

WeatherDecompositionForLocation <- function(requiredDates,location,
                                            weatherColName,search.ser) {
  pathWeather <-  'C:\\Local Forecaster\\Temp-Data\\'
  dataDF <- data.frame(read.delim(paste(pathWeather,'Weather_historical_daily.tsv',
                                        sep = ''),stringsAsFactors=FALSE))
  
  dataDF$WeatherDateTime = chron(dataDF$WeatherDateTime,
                                 format= c(dates="y-m-d"), 
                                 out.format=c(dates='d-m-y'))
  
  dataDF$ScrapeDate <- NULL
  
  dataDF$Location <- sapply(dataDF$Location,trimCity)
  dataDF$Location <- sapply(dataDF$Location,trimWales)
  dataDF$Location <- sapply(dataDF$Location,tolower)
  
  # Pick a few key locations (North,South,East,West,Central)
  
  data_tbl <- tbl_df(dataDF)
  
  # Select locations
  
  data_subset <- data_tbl %>% filter(Location %in% location) %>% 
    group_by(Location) %>% arrange(WeatherDateTime)
  
  temp_data <- data_subset %>% dplyr::select(WeatherDateTime,
                                             Location,base::eval(parse(text=weatherColName)))
  temp_data <- data.frame(temp_data)
  
  melt_df <- melt(temp_data,id.vars=c("WeatherDateTime","Location"))
  seriesdf <- dcast(melt_df,WeatherDateTime~Location+variable,fun.aggregate=mean)
  
  dims <- dim(seriesdf)
  
  colnames(seriesdf)[2:dims[2]] <- location
  
  tsData <- zoo(seriesdf[,2:dims[2]],seriesdf$WeatherDateTime)
  #plot(tsData)
  
  tsData <- as.data.frame(tsData)
  
  # Apply decomposition
  all_dates <- seriesdf$WeatherDateTime
  
  tsData <- cbind.data.frame(all_dates,tsData)
  
  reqDateDF <- data.frame(requiredDates)
  colnames(reqDateDF)[1] <- "all_dates"
  
  complete_tsData <- merge(reqDateDF,tsData,all=T)
  complete_tsData <- subset(complete_tsData,all_dates %in% requiredDates)
  
  complete_tsData$tsData <- na.fill(complete_tsData$tsData, fill='extend')
  
  #norm_weather <- scale(complete_tsData$tsData)
  
  norm_weather <- complete_tsData$tsData
  
  decomp.w <- decomposeTS(norm_weather)
  d.comp <- data.frame(tbats.components(decomp.w))
  d.comp <- cbind.data.frame(requiredDates,d.comp)
  colnames(d.comp)[1] <- 'all_dates'
  
  if(is.null(d.comp$slope)){
    d.comp$level_plus_season <- d.comp$level + d.comp$season
  } else {
    d.comp$level_plus_season <- d.comp$level + d.comp$season + d.comp$slope
  }
  
  d.comp$remainder <- d.comp$observed - d.comp$level_plus_season
  d.comp$season_delta <- d.comp$observed - d.comp$season
  
  #CP <- identifyCP(requiredDates,d.comp$remainder,d.comp$observed,search.ser)
  
  #return(list("d.comp"=d.comp,"CP"=CP))
  return(list("d.comp"=d.comp))
  
}

WeatherDecompositionForUK <- function(requiredDates,
                                      weatherColName) {
  pathWeather <-  'C:\\Local Forecaster\\Temp-Data\\'
  dataDF <- data.frame(read.delim(paste(pathWeather,'Weather_historical_daily.tsv',
                                        sep = ''),stringsAsFactors=FALSE))
  
  dataDF$WeatherDateTime = chron(dataDF$WeatherDateTime,
                                 format= c(dates="y-m-d"), 
                                 out.format=c(dates='d-m-y'))
  
  dataDF$ScrapeDate <- NULL
  
  dataDF$Location <- sapply(dataDF$Location,trimCity)
  dataDF$Location <- sapply(dataDF$Location,trimWales)
  dataDF$Location <- sapply(dataDF$Location,tolower)
  
  # Pick a few key locations (North,South,East,West,Central)
  
  data_tbl <- tbl_df(dataDF)
  
  # Select locations
  
  temp_data <- data_tbl %>% dplyr::select(WeatherDateTime,
                                          Location,base::eval(parse(text=weatherColName))) %>% 
    group_by(WeatherDateTime) 
  colnames(temp_data)[3] <- "weather"
  temp_data <- temp_data %>% dplyr::summarise(observed=median(weather)) %>% data.frame()
  colnames(temp_data)[1] <- "all_dates"
  
  reqDateDF <- data.frame(requiredDates)
  colnames(reqDateDF)[1] <- "all_dates"
  
  # Apply decomposition
  
  if(!subsetFlagSet){
    
    complete_tsData <- merge(reqDateDF,temp_data,all=T)
    complete_tsData <- subset(complete_tsData,all_dates %in% requiredDates)
    
    complete_tsData$observed <- forecast::na.interp(complete_tsData$observed)
    
    norm_weather <- complete_tsData$observed
    
    decomp.w <- decomposeTS(norm_weather)
    d.comp <- data.frame(tbats.components(decomp.w))
    d.comp <- cbind.data.frame(requiredDates,d.comp)
    colnames(d.comp)[1] <- 'all_dates'
    
    if(is.null(d.comp$slope)){
      d.comp$level_plus_season <- d.comp$level + d.comp$season
    } else {
      d.comp$level_plus_season <- d.comp$level + d.comp$season + d.comp$slope
    }
    
    d.comp$remainder <- d.comp$observed - d.comp$level_plus_season
    d.comp$season_delta <- d.comp$observed - d.comp$season
    
  }
  else{
    complete_tsData <- temp_data
    complete_tsData$all_dates <- as.Date(complete_tsData$all_dates)
    complete_tsData <- subset(complete_tsData,all_dates>as.Date("2000-01-01"))
    requiredDates <- data.frame(seq(complete_tsData$all_dates[1],complete_tsData$all_dates[nrow(complete_tsData)],by="1 day"))
    colnames(requiredDates)[1] <- "all_dates"
    
    
    complete_tsData <- merge(requiredDates,complete_tsData,by="all_dates",all.x = T)
    complete_tsData$observed <- na.approx(complete_tsData$observed)
    norm_weather <- complete_tsData$observed
    decomp.w <- decomposeTS(norm_weather)
    d.comp <- data.frame(tbats.components(decomp.w))
    d.comp <- cbind.data.frame(complete_tsData$all_dates,d.comp)
    colnames(d.comp)[1] <- 'all_dates'
    
    if(is.null(d.comp$slope)){
      d.comp$level_plus_season <- d.comp$level + d.comp$season
    } else {
      d.comp$level_plus_season <- d.comp$level + d.comp$season + d.comp$slope
    }
    
    d.comp$remainder <- d.comp$observed - d.comp$level_plus_season
    d.comp$season_delta <- d.comp$observed - d.comp$season
    d.comp <- merge(reqDateDF,d.comp,by="all_dates",all.x = T)
    
  }
  return(list("d.comp"=d.comp))
}


WeatherDecompositionComplete<- function(weatherColName) {
  pathWeather <-  'C:\\Local Forecaster\\Temp-Data\\'
  dataDF <- data.frame(read.delim(paste(pathWeather,'Weather_historical_daily.tsv',
                                        sep = ''),stringsAsFactors=FALSE))
  
#   dataDF$WeatherDateTime = chron(dataDF$WeatherDateTime,
#                                  format= c(dates="y-m-d"), 
#                                  out.format=c(dates='yy-m-d'))
#   
  
  dataDF$WeatherDateTime <- as.Date(dataDF$WeatherDateTime)
  
  dataDF$ScrapeDate <- NULL
  
  dataDF$Location <- sapply(dataDF$Location,trimCity)
  dataDF$Location <- sapply(dataDF$Location,trimWales)
  dataDF$Location <- sapply(dataDF$Location,tolower)
  
  # Pick a few key locations (North,South,East,West,Central)
  
  data_tbl <- tbl_df(dataDF)
  
  # Select locations
  
  temp_data <- data_tbl %>% dplyr::select(WeatherDateTime,
                                          Location,base::eval(parse(text=weatherColName))) %>% 
    group_by(WeatherDateTime) 
  colnames(temp_data)[3] <- "weather"
  temp_data <- temp_data %>% dplyr::summarise(observed=median(weather)) %>% data.frame()
  colnames(temp_data)[1] <- "all_dates"
  temp_data <- temp_data %>% dplyr::arrange(all_dates)
  
  # Fill missing dates
  target <- data.frame(all_dates = seq(from = temp_data$all_dates[2], to = temp_data$all_dates[nrow(temp_data)], by = "day"))
  
  temp_data <- merge(temp_data,target,by="all_dates",all.y = T)
  
  
  complete_tsData <- temp_data
  complete_tsData$all_dates <- as.Date(complete_tsData$all_dates)
  complete_tsData <- subset(complete_tsData,all_dates>as.Date("2000-01-01"))
  
  complete_tsData$observed <- na.approx(complete_tsData$observed)
  norm_weather <- complete_tsData$observed
  decomp.w <- decomposeTS(norm_weather)
  d.comp <- data.frame(tbats.components(decomp.w))
  d.comp <- cbind.data.frame(complete_tsData$all_dates,d.comp)
  colnames(d.comp)[1] <- 'all_dates'
  
  if(is.null(d.comp$slope)){
    d.comp$level_plus_season <- d.comp$level + d.comp$season
  } else {
    d.comp$level_plus_season <- d.comp$level + d.comp$season + d.comp$slope
  }
  
  d.comp$remainder <- d.comp$observed - d.comp$level_plus_season
  d.comp$season_delta <- d.comp$observed - d.comp$season

  return(list("d.comp"=d.comp))
  
  
}