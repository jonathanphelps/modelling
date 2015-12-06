library(xlsx)
library(dplyr)
library(forecast)
library(ggplot2)

metric <- 'Impressions'
select.columns <- c("Day","shown.Impressions","impression.share","Impressions","Max..CPC","Clicks","Avg..CPC","Avg..position","Quality.score")

bm.auction.df <- read.csv('C://Local Forecaster//Experiments//Data//auction-bm.csv',skip=1,stringsAsFactors=FALSE)
playskool.auction.df <- read.csv('C://Local Forecaster//Experiments//Data//auction-playskool.csv',skip=1,stringsAsFactors=FALSE)
playskool.df <- read.xlsx('C://Local Forecaster//Experiments//Data//Keyword report.xlsx',2,stringsAsFactors=FALSE)
bm.df <- read.xlsx('C://Local Forecaster//Experiments//Data//Keyword report.xlsx',3,stringsAsFactors=FALSE)


# Prepare keyword data
playskool.df$Day <- as.POSIXct(playskool.df$Day,format="%Y-%m-%d")
bm.df$Day <- as.POSIXct(as.Date(bm.df$Day),format="%Y-%m-%d")


trimPercentSymbol <- function(x){
  return(gsub('%','',x))
}

prepareColumns <- function(df){
  col.names <- colnames(df)
  ix1 <- pmatch('Search.Impr',col.names,nomatch = NA)
  if(!is.na(ix1)) {
    col.names[ix1] <- 'impression.share'
    colnames(df) <- col.names
    df$impression.share <- sapply(df$impression.share,trimPercentSymbol)
    col.names <- colnames(df)
    col.names[pmatch(metric,col.names)] <- 'shown.Impressions'
    colnames(df) <- col.names
    df[,eval(metric)] <- round(df$shown.Impressions / (as.double(df$impression.share)))
    df[,eval(metric)] <-  forecast::na.interp(df[,eval(metric)])
    
  }
  return(df)
}

subsetDF <- function(df,select.columns){
  return(df[,eval(select.columns)] %>% tbl_df %>% arrange(Day))
}

playskool.df <- prepareColumns(playskool.df)
bm.df <- prepareColumns(bm.df)

playskool.data <- subsetDF(playskool.df ,select.columns)
bm.data <- subsetDF(bm.df,select.columns)


ggplot(aes(x=Avg..position,y=shown.Impressions),data=bm.data) + geom_point()

ggplot(aes(x=Avg..position,y=shown.Impressions),data=playskool.df) + geom_point()



