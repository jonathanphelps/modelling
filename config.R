
directory <- "C://Local Forecaster//Modelling//Data//ArgosSports//"

deviceType <- c("Computer","Tablet")
#deviceType <- "Mobile"

device.type <- as.integer(1)

if("Mobile" %in% deviceType) device.type=as.integer(2)

# For initial processing
numericColumnNames <- c("Cost","AvgPosition","Revenue","ConversionRate","CTR","CPC","AvgMaxCPC")
integerColumns <- c("Impressions","Clicks","Sales","UniqueSales","UniqueSalesperClick","QualityScore")

# Before aggregation
select.master.columns <- c("LandingDate","Impressions","Clicks","AvgPosition","Device","UniqueSales","Cost")

maincolumns <- c("Date","Impression","AvgPosition")
weatherColumns <- c("observed.weather","level_plus_season","remainder")

useweather <- TRUE

if(useweather){
  selWeatherCol <- 'MeanTemperature_Celsius'
  model.columns <- c(maincolumns,weatherColumns)
} else{
  model.columns <- maincolumns
}

useweek <- TRUE # Add week variable
if(useweek) model.columns <- c(model.columns,"weekno")
  
usedow <- TRUE # Add day of week variable
if(usedow) model.columns <- c(model.columns,"dow")

usemaxcpc <- TRUE # Add max.cpc variable
if(usemaxcpc) model.columns <- c(model.columns,"MaxCpc")


plotData <- FALSE
enableKWMeasures <- FALSE

loadData <- TRUE
masterFileName <- "master.RData"

if(!loadData) {
  file.pattern <- "\\.csv$"
}

#deviceType <- "Computer"



Ntotal <- 14
occurence.quantile <- 0.99

