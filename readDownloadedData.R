source("dfHelper.R")

library(dplyr)
library(data.table)


history.start.date <- "2014-01-01"

metric <- 'Impressions'
#device.type <- c("Computer","Tablet")
device.type <- c("Computer")
match.type <- "exact"
traffic.source <- c("google")

keyword.name <- "lego city"
#campaign.name <- "M||S||Lego||Attr"
#adgroup.name <- "Lego||Lego City"
listing.id <- 740722407


#campaign.name <- "Manual||Search||Nursery Toys"
#keyword.name <- "playskool"
#listing.id <- 75525958

#campaign.name <- "M||S||Garden Furniture||Gen"
#keyword.name <- "sunbeds"
#adgroup.name <- "Garden Furniture||Sun Loungers"

select.columns <- c("Day","Impressions","Clicks","AvgCPCBidGap","AvgCPCBidGap.","AvgPosition","CPC")

dir.name <- gsub(" ","",keyword.name)
directory <- paste("C://Local Forecaster//Experiments//Data//",dir.name,sep="")

keyword.data.files <- list.files(path=directory,pattern="*LP*")
auction.data.files <- list.files(path=directory,pattern="*auction*")
#dcs.data.files <- list.files(path=directory,pattern="*dcs*")


temp.df <- data.frame()
r.df <- data.frame()
for(i in 1:length(keyword.data.files)){
  temp.df <- fread(paste(directory,"//",keyword.data.files[i],sep=""),
                   stringsAsFactors=FALSE,integer64 = "character")
  r.df <- rbind.data.frame(r.df,temp.df)
}

r.df <- makeColNamesUserFriendly(r.df)

colnames(r.df)[1] <- "Day"

r.df$newDay <- as.Date(r.df$Day,format="%Y-%m-%d")
r.df$newDay[which(is.na(r.df$newDay)==T)] <- as.Date(r.df$Day[which(is.na(r.df$newDay)==T)],format="%d/%m/%Y")
r.df$Day <- r.df$newDay
r.df$newDay <- NULL
r.df <- r.df %>% arrange(Day)

if("google" %in% traffic.source){
  r.df$isgoogle <- sapply(r.df$TrafficSource,isGoogleSource)
  r.df <- subset(r.df,isgoogle==1)
}

if("bing" %in% traffic.source){
  r.df$isbing <- sapply(r.df$TrafficSource,isBingSource)
  r.df <- subset(r.df,isbing==1)
}

if(exists("campaign.name")){
  r.df <- subset(r.df,LowercaseKeyword==keyword.name&Device %in% device.type & MatchType==match.type)
}

if(exists("adgroup.name")){
  r.df <- subset(r.df,LowercaseKeyword==keyword.name&Device %in% device.type & 
                   AdGroup==adgroup.name & MatchType==match.type)
}

if(exists("listing.id")){
  r.df <- subset(r.df,LowercaseKeyword==keyword.name&Device %in% device.type & 
                   DCSListingID==listing.id & MatchType==match.type)
}

data.df <- subset(data.frame(r.df),select = select.columns)
data.df <- data.df %>% arrange(Day)

# df <- data.df
# 
# col.names <- colnames(df)
# ix1 <- pmatch('SearchImprshare',col.names,nomatch = NA)
# if(!is.na(ix1)) {
#   col.names[ix1] <- 'impression.share'
#   colnames(df) <- col.names
#   df$impression.share <- sapply(df$impression.share,trimPercentSymbol)
#   col.names <- colnames(df)
#   df$impression.share <- as.numeric(df$impression.share)
#   NonNAindex <- which(!is.na(df$impression.share))
#   firstNonNA <- min(NonNAindex)
#   df <- df[firstNonNA:nrow(df),]
#   col.names[pmatch(metric,col.names)] <- 'shown.Impressions'
#   colnames(df) <- col.names
#   df[,eval(metric)] <- round(df$shown.Impressions / (as.double(df$impression.share)/100))
#   df[,eval(metric)] <-  forecast::na.interp(df[,eval(metric)])
# }
# 
# data.df <- df
# df <- NULL
# 

data.df <- subset(data.df,Day>=as.Date(history.start.date))


auction.df <- data.frame()
for(i in 1:length(auction.data.files)){
  temp.df <- read.csv(paste(directory,"//",auction.data.files[i],sep=""),skip=1,stringsAsFactors=FALSE)
  auction.df <- rbind.data.frame(auction.df,temp.df)
}

auction.df <- makeColNamesUserFriendly(auction.df)
auction.df <- auction.df %>% arrange(Day)

load(paste("./Data/",gsub(" ","",keyword.name),"/maxcpc.RData",sep=""))

##########################################################################################################

# dcs.df <- data.frame()
# 
# for(i in 1:length(dcs.data.files)){
#   temp.df <- read.csv(paste(directory,"//",dcs.data.files[i],sep=""),stringsAsFactors=FALSE)
#   dcs.df <- rbind.data.frame(dcs.df,temp.df)
# }
# 
# dcs.df <- makeColNamesUserFriendly(dcs.df)
# 
# 
# dcs.df$isgoogle <- sapply(dcs.df$TrafficSource,isGoogleSource)
# 
# dcs.df <- subset(dcs.df,isgoogle==1)
# colnames(dcs.df)[1] <- "Day"
# dcs.df$Day <- as.Date(dcs.df$Day,format = "%d/%m/%Y")
# dcs.df <- data.frame(dcs.df)
# bid.gap.cols <- which(str_detect(colnames(dcs.df),"BidGap")==T)
# dcs.df <- dcs.df[c(1,bid.gap.cols)]
# colnames(dcs.df)[3] <- "AvgCPCBidGapPerc"
# 
# temp.df <- NULL
