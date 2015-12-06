library(data.table)
library(xlsx)
library(dplyr)
library(forecast)
library(ggplot2)

source("dfHelper.R")

keyword.name <- "legocity"
dcstorm.fname <- paste("C://Local Forecaster//Experiments//Data//","legocity-1.csv",sep="")

metric <- 'Impressions'
select.columns <- c("Day","shown.Impressions","impression.share","Impressions","MaxCPC","Clicks","AvgCPC","Avgposition","Qualityscore")


r.df <- fread('C://Local Forecaster//Experiments//Data//Keyword report-legocity.csv',stringsAsFactors=FALSE,skip = 1)

r.df <- subset(r.df,Keyword=="[lego city]"&Campaign=="M||S||Lego||Attr")

r.df <- data.frame(r.df)

r.df <- r.df %>% arrange(Day)

r.df <- makeColNamesUserFriendly(r.df)

r.df <- prepareColumns(r.df)

data.df <- subsetDF(r.df,select = select.columns)
data.df$Day <- as.Date(data.df$Day)

ggplot(aes(x=Avgposition,y=shown.Impressions),data=data.df) + geom_point()

dc.df <- fread(dcstorm.fname,stringsAsFactors=FALSE)

dc.df$isgoogle <- sapply(dc.df$`Traffic Source`,isGoogleSource)
dc.df <- subset(dc.df,isgoogle==1)
colnames(dc.df)[1] <- "Day"
dc.df$Day <- as.Date(dc.df$Day,format = "%d/%m/%Y")
dc.df <- data.frame(dc.df)
dc.df <- makeColNamesUserFriendly(dc.df)
bid.gap.cols <- which(str_detect(colnames(dc.df),"BidGap")==T)
dc.df <- dc.df[c(1,bid.gap.cols)]
colnames(dc.df)[3] <- "AvgCPCBidGapPerc"
