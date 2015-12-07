directory <- "C://Local Forecaster//Modelling//Data//ArgosSports//"

deviceType <- c("Computer","Tablet")
#deviceType <- "Mobile"

device.type <- 1

if("Mobile" %in% deviceType) device.type=2

select.master.columns <- c("LandingDate","Impressions","Clicks","AvgPosition","Device","Sales","Cost")

maincolumns <- c("Date","Impression","AvgPosition","weekno")
weatherColumns <- c("observed.weather","level_plus_season","remainder")

useweather <- TRUE

if(useweather){
  selWeatherCol <- 'MeanTemperature_Celsius'
  model.columns <- c(maincolumns,weatherColumns)
} else{
  model.columns <- maincolumns
}

usemaxcpc <- FALSE
plotData <- FALSE
enableKWMeasures <- FALSE

loadData <- TRUE
if(loadData) masterFileName <- "master.RData"

#deviceType <- "Computer"



Ntotal <- 14
occurence.quantile <- 0.99

numericColumnNames <- c("Cost","AvgPosition","Sales","Revenue","ConversionRate","CTR","CPC","AvgMaxCPC")
integerColumns <- c("Impressions","Clicks")