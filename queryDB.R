library(RODBC)
library(dplyr)
library(lubridate)
library(zoo)

device.type <- 1
#campaign.name <- "M||S||Lego||Attr"

#keyword.name <- "lego city"
#listing.id <- 740722407

#adgroup.name <- "Lego||Lego City"

#campaign.name <- "Manual||Search||Nursery Toys"
keyword.name <- "playskool"
listing.id <- 75525958

#campaign.name <- "M||S||Garden Furniture||Gen"
#keyword.name <- "sunbeds"

select.columns <- c("Date","MaxCpc","hour.times.minute")

DbConfig <- list(
  RDbConString = paste("DRIVER=ODBC Driver 11 for SQL Server;Server=mssql.forecaster.com;DATABASE=Insights;UID=insights;PWD=9XPDbY3GlggI;", sep=""),
  GDbConString = paste("DRIVER=ODBC Driver 11 for SQL Server;Server=mssql.forecaster.com;DATABASE=Partners;UID=azure;PWD=Pokoliko.0;Connection Timeout=180", sep=""),
  FDbConString = paste("DRIVER=ODBC Driver 11 for SQL Server;Server=mssql.forecaster.com;DATABASE=Forecaster;UID=forecaster-live;PWD=SzmrMem5m8Su;", sep="")
)


string <- "SELECT OA.[ProcessId]
,CAST(P.[StartTime] AS DATE) AS [Date]
,P.[StartTime]
,OA.[TrafficSource]
,A.[TrafficSourceName]
,A.[TrafficSourceId]
,OA.[SeCampaign]
,OA.[SeCampaignId]
,OA.[Category]
,OA.[AdGroup]
,OA.[SeAdGroupId]
,OA.[LowercaseKeyword]
,OA.[SeKeywordId]
,OA.[MatchType]
,OA.[DcStormListingId]
,OA.[OptPosition]
,OA.[MaxCpc]
,OA.[DeviceTypeId]
FROM [dbo].[ProcessOptimumAllocationReports] OA WITH (NOLOCK)
INNER JOIN [dbo].[Processes] P WITH (NOLOCK) ON (P.[ProcessId] = OA.[ProcessId])
INNER JOIN [dbo].[Accounts] A WITH (NOLOCK) ON (A.[AccountId] = P.[AccountId])
WHERE (OA.[LowercaseKeyword] = '''%s''')
AND (OA.[MatchType] = 'exact')
AND (CAST(P.[StartTime] AS DATE) >= '2014-01-01')
ORDER BY OA.[AllocationReportId] ASC;"

query.string <- sprintf(string,keyword.name)

channel <- odbcDriverConnect(connection=DbConfig$FDbConString);

result  <- sqlQuery(channel, query.string, stringsAsFactors = FALSE)
odbcClose(channel)

string2 <- "SELECT OA.[ProcessId]
,CAST(P.[StartTime] AS DATE) AS [Date]
,P.[StartTime]
,OA.[TrafficSource]
,A.[TrafficSourceName]
,A.[TrafficSourceId]
,OA.[SeCampaign]
,OA.[SeCampaignId]
,OA.[Category]
,OA.[AdGroup]
,OA.[SeAdGroupId]
,OA.[LowercaseKeyword]
,OA.[SeKeywordId]
,OA.[MatchType]
,OA.[DcStormListingId]
,OA.[OptPosition]
,OA.[MaxCpc]
,OA.[DeviceTypeId]
FROM [dbo].[ProcessOptimumAllocationReports] OA WITH (NOLOCK)
INNER JOIN [dbo].[Processes] P WITH (NOLOCK) ON (P.[ProcessId] = OA.[ProcessId])
INNER JOIN [dbo].[Accounts] A WITH (NOLOCK) ON (A.[AccountId] = P.[AccountId])
WHERE (OA.[LowercaseKeyword] = '%s')
AND (OA.[MatchType] = 'exact')
AND (CAST(P.[StartTime] AS DATE) >= '%s')
ORDER BY OA.[AllocationReportId] ASC;"

next.start.date <- tail(result$Date,1)
query.string <- sprintf(string2,keyword.name,next.start.date)

channel2 <- odbcDriverConnect(connection=DbConfig$FDbConString);

result2  <- sqlQuery(channel2, query.string, stringsAsFactors = FALSE)
odbcClose(channel2)

############################################################################################################
result <- result %>% arrange(Date)

result <- subset(result,DcStormListingId==listing.id & DeviceTypeId==device.type)

result$hour.time <- hour(result$StartTime)
result$minute.time <- lubridate::minute(result$StartTime)
result$second.time <- lubridate::second(result$StartTime)

result$hour.times.minute <- result$hour.time*result$minute.time*result$second.time

rownames(result) <- NULL

maxcpc.data <- result[,select.columns]
group.data <- maxcpc.data %>% group_by(Date)
maxcpc.df <- dplyr::summarize(group.data, hour.times.minute = max(hour.times.minute))

maxcpc.df <- merge(maxcpc.df, group.data, by=c("Date","hour.times.minute"))

############################################################################################################

result2 <- result2 %>% arrange(Date)

result2 <- subset(result2,DcStormListingId==listing.id & DeviceTypeId==device.type)

result2$hour.time <- hour(result2$StartTime)
result2$minute.time <- lubridate::minute(result2$StartTime)
result2$second.time <- lubridate::second(result2$StartTime)

result2$hour.times.minute <- result2$hour.time*result2$minute.time*result2$second.time

rownames(result2) <- NULL

maxcpc.data2 <- result2[,select.columns]
group.data2 <- maxcpc.data2 %>% group_by(Date)
maxcpc.df2 <- dplyr::summarize(group.data2, hour.times.minute = max(hour.times.minute))

maxcpc.df2 <- merge(maxcpc.df2, group.data2, by=c("Date","hour.times.minute"))

############################################################################################################

maxcpc.data.all <- rbind.data.frame(maxcpc.df,maxcpc.df2)

maxcpc.data.all <- maxcpc.data.all[!duplicated(maxcpc.data.all),]

plot(zoo(maxcpc.data.all$MaxCpc,order.by = maxcpc.data.all$Date),type='b',col='blue',ylab = 'Max CPC')

#######################################################################################################

save(maxcpc.data.all,file=paste("../Experiments/Data/",
                                gsub(" ","",keyword.name),"/maxcpc.RData",sep=""))
