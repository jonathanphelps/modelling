require(edarf)
require(DataCombine)
require(Hmisc)
require(edarf)
require(Matrix)

source("modelConfig.R")
source("dominantFrequencies.R")
source("tsHelper.R")

noMethods <- length(reg.methods) 
newLagResponseColNames <- NULL

###################################################################################################################

tix <- which(otherVariables.transform == "yes")
if(length(tix)>0){
  var.names <- names(otherVariables.transform)
  t.var.names <- var.names[tix]
  var.names <- setdiff(var.names,t.var.names)
  t.var.names <- sapply(t.var.names,function(x) {sprintf("%s(%s)",transformation.string,x)})
  otherVariables <- c(var.names,t.var.names)
} else{
  otherVariables <- names(otherVariables.transform)
}

partial.formula.string <- paste(otherVariables,collapse="+")
partial.formula.string <- paste("~",partial.formula.string,"+",sep="")

###################################################################################################################

mydf <- model.data

if(onlyNumericVars){
  w <- which( sapply( mydf, class ) == 'factor' )
  mydf[w] <- lapply( mydf[w], function(x) as.numeric(as.character(x)) )
  
  w <- which( sapply( mydf, is.ordered) == T )
  mydf[w] <- lapply( mydf[w], function(x) as.numeric(x) )
}

##################################################################################################################

all.response.lags <- list()
if(exists("lagVariables")) {
  for(k in 1:length(lagVariables)){
    all.response.lags[[k]] <- findSignificantCCFLags(mydf,lagVariables[k],curr.metric,no.top.lags.needed[k],
                                                     no.lags,ccf.significance.level)
  }
  newLagResponseColNames <- NULL
  for(v in 1:length(lagVariables)){
    response.lags <- all.response.lags[[v]]
    if(length(response.lags)>0){
      variable <- lagVariables[v]
      newLagResponseColNames <-
        c(newLagResponseColNames,sapply(response.lags,createNewColNamesForLags,variable))
      
      for (l in 1:length(response.lags)) {
        #mydf <- createLaggedVariables(response.lags[l],mydf,variable)
        mydf <- slide(mydf, Var = variable, slideBy = as.integer(-round(response.lags[l])))
        
      }
      colnames(mydf) <- gsub("-", "", colnames(mydf))
    }
  }
}
##################################################################################################################

# Create lags for response variable

imp.periods <- dominantPeriodsTimeDomain(mydf,select.quantile=significance.level)

if(length(which(imp.periods==1))==0) imp.periods <- c(1,imp.periods)
#imp.periods <- c(1)
response.periods <- imp.periods[which(imp.periods<=60)]

significant.lags.response <- sort(response.periods)

if(!exists("newLagResponseColNames")) newLagResponseColNames <- c()
newLagResponseColNames <-
  c(newLagResponseColNames,sapply(significant.lags.response,createNewColNamesForLags,curr.metric))

for (l in 1:length(significant.lags.response)) {
  #mydf <- createLaggedVariables(significant.lags.response[l],mydf,curr.metric)
  mydf <- slide(mydf, Var = curr.metric, slideBy = as.integer(-round(significant.lags.response[l])))
}

colnames(mydf) <- gsub("-", "", colnames(mydf))

##################################################################################################################

# Create design matrix with lagged variable

response.variable <- curr.metric
max.lag <- max(significant.lags.response)
max.lag.colname <- paste(response.variable,max.lag,sep = "")
max.lag.ix <- which(colnames(mydf) == max.lag.colname)
st.ix <- tail(which(is.na(mydf[,max.lag.ix]) == TRUE),1) + 1
mydf <- mydf[st.ix:nrow(mydf),]
mydf <- mydf[complete.cases(mydf),]

##################################################################################################################

# Create a dynamic formula for modelling

if(exists("lagVariables") & applyLagPositionTransformation){
  posn.cols <- newLagResponseColNames[grep("AvgPosition*",newLagResponseColNames)]
  remn.cols <- setdiff(newLagResponseColNames,posn.cols)
  posn.cols <- sapply(posn.cols,function(x) {sprintf("%s(%s)",transformation.string,x)})
  names(posn.cols) <- NULL
  newLagResponseColNames <- c(remn.cols,posn.cols)
}


complete.string <- paste(response.variable,partial.formula.string,
                         paste(newLagResponseColNames,collapse = "+",sep = ""),sep = "")

reg.formula <- as.formula(complete.string)

# Week no to be added as a feature

if(addWeekNo){
  week.string <- gsub("~","~weekno+",complete.string)
  reg.formula.amended <- as.formula(week.string)
} else{
  reg.formula.amended <- reg.formula
}

##################################################################################################################

# Filter position columns for position < 0

filterZeroAvgPosition <- function(x,df){
  df <- df[-which(mydf[,x]==0),]
  return(df)
}

if(exists("lagVariables")) {
  
  if("AvgPosition" %in% lagVariables){
    f.cols <- grep("AvgPosition*",colnames(mydf))
    for(c in 1:length(f.cols)){
      mydf <- filterZeroAvgPosition(f.cols[c],mydf)
    }
  }
}

N <- nrow(mydf)
sind <- sample(N)

#Shuffle the data

if(enableshuffle){
  mydf<-mydf[sind,]
}
rownames(mydf) <- NULL

##################################################################################################################

## Setup 'x' folds of cross-validation

folds <- cut(seq(1,N),breaks = noFolds,labels = FALSE)

nmsemat <- matrix(1e10,noMethods,noFolds)
maemat <- matrix(1e10,noMethods,noFolds)
nmsemat.repeat <- matrix(1e10,1,noFolds)
mae.repeat <- matrix(1e10,1,noFolds)

##################################################################################################################

rf.list <- list()
gbm.list <- list()

rf.predMeans <- list()
rf.predVariances <- list()
actualMeans <- list()

lm.predMeans <- list()
lm.predVariances <- list()

bm.predMeans <- list()
bm.predVariances <- list()

glm.predMeans <- list()

fold.train.dates <- list()
fold.test.dates <- list()


##################################################################################################################

method.order <- c()

#sink(logfile,append = TRUE)

for(fold.cnt in 1:noFolds){
  
  method.cnt <- 0  
  trainIndexes <- which(folds!=fold.cnt,arr.ind=TRUE)
  testIndexes <- which(folds==fold.cnt,arr.ind=TRUE)
  
  train<- mydf[trainIndexes,]
  test <- mydf[testIndexes,]
  fold.train.dates[[fold.cnt]] <- train$Date
  fold.test.dates[[fold.cnt]] <- test$Date
  
  train$Date <- NULL
  test$Date <- NULL
  
  rownames(train) <- NULL
  rownames(test) <- NULL
  
  source("trainAndTestScriptForImpressions.R")
  
  actualMeans[[fold.cnt]] <- test[,eval(response.variable)]
  
  if("AvgPosition1" %in% newLagResponseColNames){
    nmsemat.repeat[1,fold.cnt] = measureRMSE(test[,eval(response.variable)],test$AvgPosition1)
    mae.repeat[1,fold.cnt] = measureMAE(test[,eval(response.variable)],test$AvgPosition1)
  }
}

method.ix <- seq(1,length(reg.methods))
names(method.order) <- method.ix

source("gatherResultStats.R")
#sink()
