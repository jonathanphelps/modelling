library(randomForestSRC)
library(edarf)
library(DataCombine)
library(mlr)
library(Hmisc)
library(edarf)
library(bartMachine)
library(Matrix)
library(mlr)

reg.methods <- c("bartmachine","RF","lm","glm")

lagVariables <- c("MaxCpc")
#lagVariables <- c("MaxCpc")
no.top.lags.needed <- c(7)
#no.top.lags.needed <- c(1)
no.lags <- 30

otherVariables <- c("weekno","level_plus_season","remainder")

#otherVariables <- c("level_plus_season","remainder")

enableHyperparameterLearning <- FALSE

significance.level <- 0.95

#onlyNumericVars <- FALSE
onlyNumericVars <- TRUE

ntrees <- 150
noFolds <- 10

rf.hparam.mtry <- 7
rf.hparam.ntree <- ntrees

noMethods <- length(reg.methods) 

partial.formula.string <- paste(otherVariables,collapse="+")
partial.formula.string <- paste("~",partial.formula.string,"+",sep="")

###################################################################################################################

findSignificantCCFLags <- function(df,pred.variable,response.variable,no.top.lags.needed,
                                   no.lags=30,significance.level=0.95)
{
  res.ccf <- ccf(df[,pred.variable],df[,response.variable],no.lags,plot=F,na.action = na.pass)
  ci.limit <- qnorm((1 + significance.level)/2)/sqrt(res.ccf$n.used)
  extract <- cbind.data.frame(res.ccf$lag[which(res.ccf$lag < 0)],res.ccf$acf[which(res.ccf$lag < 0)])
  colnames(extract) <- c("lag","ccf")
  extract <- extract %>% dplyr::arrange(desc(abs(ccf)))
  extract <- extract[1:no.top.lags.needed,]
  significant.lags.response <- abs(extract[which(abs(extract[,2]) >= ci.limit),1])
  return(significant.lags.response)
}

createLaggedVariables <- function(lag,df,var.name) {
  return(slide(df, Var = var.name, slideBy = -lag))
}

createNewColNamesForLags <- function(lag,variable) {
  return(paste(variable,lag,sep = ""))
}

###################################################################################################################

mydf <- model.data
#mydf$Day <- NULL

if(onlyNumericVars){
  w <- which( sapply( mydf, class ) == 'factor' )
  mydf[w] <- lapply( mydf[w], function(x) as.numeric(as.character(x)) )
  
  w <- which( sapply( mydf, is.ordered) == T )
  mydf[w] <- lapply( mydf[w], function(x) as.numeric(x) )
}


##################################################################################################################

all.response.lags <- list()

for(k in 1:length(lagVariables)){
  all.response.lags[[k]] <- findSignificantCCFLags(mydf,lagVariables[k],curr.metric,no.top.lags.needed[k],
                                                   no.lags,significance.level)
}

newLagResponseColNames <- NULL

for(v in 1:length(lagVariables)){
  response.lags <- all.response.lags[[v]]
  variable <- lagVariables[v]
  
  newLagResponseColNames <-
    c(newLagResponseColNames,sapply(response.lags,createNewColNamesForLags,variable))
  
  for (l in 1:length(response.lags)) {
    mydf <- createLaggedVariables(response.lags[l],mydf,variable)
  }
  
  colnames(mydf) <- gsub("-", "", colnames(mydf))
}

##################################################################################################################

# Create lags for response variable

res.pacf <- pacf(mydf[,curr.metric],no.lags,plot = F,na.action = na.pass)
ci.limit <- qnorm((1 + significance.level)/2)/sqrt(res.pacf$n.used)
extract <- cbind.data.frame(res.pacf$lag,res.pacf$acf)
colnames(extract) <- c("lag","acf")
extract <- extract %>% dplyr::arrange(desc(abs(acf)))
extract <- extract[1:no.lags,]
significant.lags.response <- sort(abs(extract[which(abs(extract[,2]) >= ci.limit),1]))


newLagResponseColNames <-
  c(newLagResponseColNames,sapply(significant.lags.response,createNewColNamesForLags,curr.metric))

for (l in 1:length(significant.lags.response)) {
  mydf <- createLaggedVariables(significant.lags.response[l],mydf,curr.metric)
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

N <- nrow(mydf)
sind <- sample(N)

#Shuffle the data

mydf<-mydf[sind,]
rownames(mydf) <- NULL

##################################################################################################################

# Create a dynamic formula

reg.formula <-
  as.formula(
    paste(
      response.variable,partial.formula.string,
      paste(
        newLagResponseColNames,collapse = "+",sep = ""
      ),sep = ""
    )
  )

##################################################################################################################

## Setup 'x' folds of cross-validation

folds <- cut(seq(1,N),breaks = noFolds,labels = FALSE)

nmsemat <- matrix(1e10,noMethods,noFolds)
maemat <- matrix(1e10,noMethods,noFolds)
nmsemat.repeat <- matrix(1e10,1,noFolds)


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
##################################################################################################################

for(fold.cnt in 1:noFolds){
  
  method.cnt <- 0  
  trainIndexes <- which(folds!=fold.cnt,arr.ind=TRUE)
  testIndexes <- which(folds==fold.cnt,arr.ind=TRUE)
  
  train<- mydf[trainIndexes,]
  test <- mydf[testIndexes,]
  train$Day <- NULL
  test$Day <- NULL
  
  rownames(train) <- NULL
  rownames(test) <- NULL
  
  actualMeans[[fold.cnt]] <- test[,eval(response.variable)]
  
  if("RF" %in% reg.methods){
    method.cnt <- method.cnt + 1
  
    #Attempt to fit a RandomForest
    
    
    if(fold.cnt==1 && enableHyperparameterLearning) {
      pos.task = makeRegrTask(id = "pos", data = train, target = response.variable)
      
      ps = makeParamSet(
        makeIntegerParam(id="mtry",lower = 6L, upper = 9L),
        makeIntegerParam(id="ntree",lower = 50L, upper = 250L)
      )
      
      ctrl = makeTuneControlGrid(resolution = 3L)
      rdesc = makeResampleDesc("CV", iters = 20)
      
      rf.lrn = tuneParams("regr.randomForest", task = pos.task, resampling = rdesc, par.set = ps,
                       control = ctrl)
      rf.hparam.mtry <- rf.lrn$x$mtry
      rf.hparam.ntree <- rf.lrn$x$ntree
      rf.hparam.maxnodes <- rf.lrn$x$maxnodes
    }
    
    rf.fit  <- rfsrc(reg.formula,
                     train,
                     mtry=rf.hparam.mtry,
                     ntree = rf.hparam.ntree,
                     split.depth = "all.trees",
                     importance = "permute")
    
    #  Predict at new test points
    rf.pred <- predict(rf.fit,newdata = test)
    jkest <- edarf::var_est(rf.fit,test)
    rf.predMeans[[fold.cnt]] <- jkest[,1]
    rf.predVariances[[fold.cnt]] <- jkest[,2]
    nmsemat[method.cnt,fold.cnt] = measureRMSE(test[,eval(response.variable)],rf.pred$predicted)
    maemat[method.cnt,fold.cnt] = measureMAE(test[,eval(response.variable)],rf.pred$predicted)
    
    rf.list[[fold.cnt]] <- rf.fit
    
  }
  
  
  if("lm" %in% reg.methods){
    method.cnt <- method.cnt + 1
    lm.fit <- lm(reg.formula,train)
    lm.pred <- predict(lm.fit,test,se.fit = T,interval="prediction")
    lm.predMeans[[fold.cnt]] <- lm.pred$fit[,1]
    lm.predVariances[[fold.cnt]] <- lm.pred$fit[,2:3]
    nmsemat[method.cnt,fold.cnt] = measureRMSE(test[,eval(response.variable)],lm.pred$fit[,1])
    maemat[method.cnt,fold.cnt] = measureMAE(test[,eval(response.variable)],lm.pred$fit[,1])
  }
  
  if("bartmachine" %in% reg.methods){
    method.cnt <- method.cnt + 1
    X <- subset(train,select=-c(eval(parse(text=curr.metric)),
                                Impressions,Clicks,AvgCPCBidGap,AvgCPCBidGap.,CPC,MaxCpc,observed,
                                level,slope,season,season_delta))
    Y <- subset(train,select=c(eval(parse(text=curr.metric))))
    
    bm.fit <- bartMachine(X,as.numeric(Y$AvgPosition),num_burn_in = 300,num_trees=50,
                          q=0.8,k=7,nu=2,alpha=0.5)
    
    Xtest <- subset(test,select=-c(eval(parse(text=curr.metric)),
                                   Impressions,Clicks,AvgCPCBidGap,AvgCPCBidGap.,CPC,MaxCpc,observed,
                                   level,slope,season,season_delta))
    
    Ytest <- predict(bm.fit,Xtest)
    bm.predMeans[[fold.cnt]] <- Ytest
    bm.predVariances[[fold.cnt]] <- calc_prediction_intervals(bm.fit,Xtest)
    nmsemat[method.cnt,fold.cnt] = measureRMSE(test[,eval(response.variable)],Ytest)
    maemat[method.cnt,fold.cnt] = measureMAE(test[,eval(response.variable)],Ytest)
  }
  
  if("glm" %in% reg.methods){
    method.cnt <- method.cnt + 1
    zero.indices <- which(train[,response.variable]==0)
    if(length(zero.indices) > 0) train <- train[-zero.indices,]
    glm.fit <- bayesglm(reg.formula,train,family = gaussian("log"))
    glm.pred <- predict(glm.fit,test,type="response",se.fit = T,interval='prediction')
    glm.predMeans[[fold.cnt]] <- glm.pred$fit
    #glm.predVariances[[fold.cnt]] <- glm.pred$fit[,2:3]
    nmsemat[method.cnt,fold.cnt] = measureRMSE(test[,eval(response.variable)],glm.pred$fit)
    maemat[method.cnt,fold.cnt] = measureMAE(test[,eval(response.variable)],glm.pred$fit)    
  }
  
  nmsemat.repeat[1,fold.cnt] = measureRMSE(test[,eval(response.variable)],test$AvgPosition1)
}

if("RF" %in% reg.methods){
  sel.ix <- which(nmsemat[1,] == min(nmsemat[1,]))
  rfo <- rf.list[[sel.ix]]
  
  rf.means <- rf.predMeans[[sel.ix]]
  rf.std <- sqrt(rf.predVariances[[sel.ix]])
  rf.upperlimit <- rf.means + (2*rf.std)
  rf.lowerlimit <- rf.means - (2*rf.std)
  
  x <- seq(1,length(rf.means))
  df2 <- cbind.data.frame(x,rf.means,rf.upperlimit,rf.lowerlimit)
  
  with(df2,errbar(x,rf.means,rf.upperlimit,rf.lowerlimit))
  points(x,actualMeans[[sel.ix]],col='red',pch=19)
  
  resid <- rf.means - actualMeans[[sel.ix]]
  
  better.outcomes <- length(which((nmsemat[1,]/nmsemat.repeat) < 1.0))
  worse.outcomes <- length(which((nmsemat[1,]/nmsemat.repeat) >= 1.0))
  
  cat(better.outcomes)
  cat('\n')
  cat(worse.outcomes)
  cat('\n')
  cat(median(nmsemat[1,]/nmsemat.repeat))
}

if(dim(nmsemat)[1] > 1){
  cat('\n')
  better.outcomes <- length(which((nmsemat[2,]/nmsemat.repeat) < 1.0))
  worse.outcomes <- length(which((nmsemat[2,]/nmsemat.repeat) >= 1.0))
  
  cat(better.outcomes)
  cat('\n')
  cat(worse.outcomes)
  cat('\n')
  cat(median(nmsemat[2,]/nmsemat.repeat))
  
}

if(dim(nmsemat)[1] > 2){
  
  sel.ix <- which(nmsemat[3,] == min(nmsemat[3,]))
  
  bm.means <- bm.predMeans[[sel.ix]]
  bm.upperlimit <- bm.predVariances[[sel.ix]][,2]
  bm.lowerlimit <- bm.predVariances[[sel.ix]][,1]
  
  x <- seq(1,length(bm.means))
  df2 <- cbind.data.frame(x,bm.means,bm.upperlimit,bm.lowerlimit)
  
  with(df2,errbar(x,bm.means,bm.upperlimit,bm.lowerlimit))
  points(x,actualMeans[[sel.ix]],col='red',pch=19)
  
  
  cat('\n')
  better.outcomes <- length(which((nmsemat[3,]/nmsemat.repeat) < 1.0))
  worse.outcomes <- length(which((nmsemat[3,]/nmsemat.repeat) >= 1.0))
  
  cat(better.outcomes)
  cat('\n')
  cat(worse.outcomes)
  cat('\n')
  cat(median(nmsemat[3,]/nmsemat.repeat))
  
}

if(dim(nmsemat)[1] > 3){
  
  sel.ix <- which(nmsemat[4,] == min(nmsemat[4,]))
  
  #bm.means <- bm.predMeans[[sel.ix]]
  #bm.upperlimit <- bm.predVariances[[sel.ix]][,2]
  #bm.lowerlimit <- bm.predVariances[[sel.ix]][,1]
  
  #x <- seq(1,length(bm.means))
  #df2 <- cbind.data.frame(x,bm.means,bm.upperlimit,bm.lowerlimit)
  
  #with(df2,errbar(x,bm.means,bm.upperlimit,bm.lowerlimit))
  #points(x,actualMeans[[sel.ix]],col='red',pch=19)
  
  
  cat('\n')
  better.outcomes <- length(which((nmsemat[4,]/nmsemat.repeat) < 1.0))
  worse.outcomes <- length(which((nmsemat[4,]/nmsemat.repeat) >= 1.0))
  
  cat(better.outcomes)
  cat('\n')
  cat(worse.outcomes)
  cat('\n')
  cat(median(nmsemat[4,]/nmsemat.repeat))
  
}

if(dim(nmsemat)[1] > 4){
  
  sel.ix <- which(nmsemat[5,] == min(nmsemat[5,]))
  
  #bm.means <- bm.predMeans[[sel.ix]]
  #bm.upperlimit <- bm.predVariances[[sel.ix]][,2]
  #bm.lowerlimit <- bm.predVariances[[sel.ix]][,1]
  
  #x <- seq(1,length(bm.means))
  #df2 <- cbind.data.frame(x,bm.means,bm.upperlimit,bm.lowerlimit)
  
  #with(df2,errbar(x,bm.means,bm.upperlimit,bm.lowerlimit))
  #points(x,actualMeans[[sel.ix]],col='red',pch=19)
  
  
  cat('\n')
  better.outcomes <- length(which((nmsemat[5,]/nmsemat.repeat) < 1.0))
  worse.outcomes <- length(which((nmsemat[5,]/nmsemat.repeat) >= 1.0))
  
  cat(better.outcomes)
  cat('\n')
  cat(worse.outcomes)
  cat('\n')
  cat(median(nmsemat[5,]/nmsemat.repeat))
  
}
