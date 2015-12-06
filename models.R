library(caret)
library(randomForest)
library(randomForestSRC)
library(mlr)
library(DataCombine)

all.response.lags <- seq(1,7)

K <- length(all.response.lags)

rf.Fit <- list()
importance <- list()

ntrees <- 200
noFolds <- 5
nmsemat <- matrix(1e10,K,noFolds)
maemat <- matrix(1e10,K,noFolds)

response.variable <- "Avgposition"

createLaggedVariables <- function(lag,df,var.name) {
  return(slide(df, Var = var.name, slideBy = -lag))
}

createNewColNamesForLags <- function(lag,variable) {
  return(paste(variable,lag,sep = ""))
}

#all.response.lags <- c(1,2)

for (run in 1:K) {
  
  df <- search.weather
  
  response.lags <- seq(1,all.response.lags[run])
  newLagResponseColNames <-
    sapply(response.lags,createNewColNamesForLags,response.variable)
  
  for (l in 1:length(response.lags)) {
    df <- createLaggedVariables(response.lags[l],df,response.variable)
  }
  
  colnames(df) <- gsub("-", "", colnames(df))
  
  # Create lags for response variable
  
  max.lag <- max(response.lags)
  max.lag.colname <- paste(response.variable,max.lag,sep = "")
  max.lag.ix <- which(colnames(df) == max.lag.colname)
  st.ix <- tail(which(is.na(df[,max.lag.ix]) == TRUE),1) + 1
  df <- df[st.ix:nrow(df),]
  
  N <- nrow(df)
  folds <- cut(seq(1,N),breaks = noFolds,labels = FALSE)
  
  
  # Create a dynamic formula
  
  reg.formula <-
    as.formula(
      paste(
        response.variable,"~no.competitors+Impressions+weekno+weekday+level_plus_season+remainder+",
        paste(
          newLagResponseColNames,collapse = "+",sep = ""
        ),sep = ""
      )
    )
  
  rfo <- list()
  
  for (f in 1:noFolds) {
    
    testIndexes <- which(folds==f,arr.ind=TRUE)
    
    testing <- df[testIndexes,]
    training <- df[-testIndexes,]
    
    rownames(training) <- NULL
    rownames(testing) <- NULL
    
    ## Train Random Forest
    
    rfo[[f]]  <- rfsrc(reg.formula,
                  training,ntree = ntrees,
                  importance = "permute")
    
    #rfo[[f]]  <- randomForest(reg.formula,data=training,keep.inbag = TRUE,proximity = TRUE)
    
    #  Predict at new test points
    rfPred <- predict(rfo[[f]],newdata = testing)
    
    nmsemat[run,f] = measureRMSE(testing[,eval(response.variable)],rfPred$predicted)
    maemat[run,f] = measureMAE(testing[,eval(response.variable)],rfPred$predicted)
    
  }
  
  c.ix <- which(nmsemat[run,] == min(nmsemat[run,]))
  rfselect <- rfo[[c.ix]]
  importance[[run]] <- rfselect$importance
  rf.Fit[[run]] <- rfselect
}

apply(nmsemat,1,mean)