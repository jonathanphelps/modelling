library(mlr)
library(tgp)
library(lubridate)
library(chron)
library(dplyr)
library(randomForestSRC)
enableHyperparameterLearning <- FALSE
includeDOW <- FALSE
#includeDOW <- TRUE

ddf <- read.csv('./Data/bid_Data.csv')
chooseN <- 9
noFolds <- 5
list.keywords <- unique(ddf$kw)
response.variable <- "Max.Historical.Bid"

#subset.keywords <- sample(list.keywords,chooseN)

subset.keywords <- c(893694522,780532879,893330163,885453069,886344531,890444642,
                     762688498,878064696,888980074)

par(mfrow=c(3,3))

rmse.btgp.keywords <- list()
rmse.rf.keywords <- list()

for(i in 1:chooseN){
  data <- subset(ddf,ddf$kw == subset.keywords[i])
  #plot(Max.Historical.Bid~CPC,data,type='p',col='blue')
  Z <- data$Max.Historical.Bid
  t <- as.Date(data$Landing.Date,format="%d/%m/%Y")
  data$weekend <- 1*(chron::is.weekend(t))
  data$weekday <- wday(t)
  #boxplot(Max.Historical.Bid~weekday,data)
  
  N <- nrow(data)
  sind <- sample(N)
  
  #Shuffle the data
  
  data<-data[sind,]
  data <- subset(data,eval(response.variable)>=CPC)
  #data <- data[complete.cases(data),]
  rownames(data) <- NULL
  
  N <- nrow(data)
  
  rmse.btgp.folds <- c()
  rmse.rf.folds <- c()
  prediction.frame <- data.frame()
  
  folds <- cut(seq(1,N),breaks = noFolds,labels = FALSE)
  
  # Cross-validation
  
  for(fold.cnt in 1:noFolds){
    
    trainIndexes <- which(folds!=fold.cnt,arr.ind=TRUE)
    testIndexes <- which(folds==fold.cnt,arr.ind=TRUE)
    
    train<- data[trainIndexes,]
    test <- data[testIndexes,]
    train$Day <- NULL
    test$Day <- NULL
    
    rownames(train) <- NULL
    rownames(test) <- NULL
    
    #actualMeans[[fold.cnt]] <- test[,"Max.Historical.Bid"]
    
    ifelse(includeDOW,X <- cbind.data.frame(train$CPC,train$weekday),X <- as.data.frame(train$CPC))
    Z <- train$Max.Historical.Bid
    
    ifelse(includeDOW,Xt <- cbind.data.frame(test$CPC,test$weekday),Xt <- as.data.frame(test$CPC))
    
    #-------------------------------------
    # 
    #-------------------------------------
    
    
    #-------------------------------------
    # Bayesian-treed GP regression
    #-------------------------------------
    
#      fit <- btgpllm(X,Z,tree = c(0.95, 2),corr = 'matern',bprior = 'b0not')
#      Zt <- predict(fit,Xt)
#      rmse.btgp.folds <- c(rmse.btgp.folds,measureRMSE(test[,eval(response.variable)],Zt$ZZ.mean))
#  
    #-------------------------------------
    # RandomForest
    #-------------------------------------
    #rf.hparam.mtry <- 4
    rf.hparam.ntree <- 250
    
    model.train.df <- cbind.data.frame(X,Z)
    model.test.df <- cbind.data.frame(Xt,test[,eval(response.variable)])
    
    if(includeDOW){
      colnames(model.train.df) <- c("CPC","dow",response.variable)
      colnames(model.test.df) <- c("CPC","dow",response.variable)
      formula <- as.formula(paste(response.variable,"~CPC+dow",sep=""))
    } else{
      colnames(model.train.df) <- c("CPC",response.variable)
      colnames(model.test.df) <- c("CPC",response.variable)
      formula <- as.formula(paste(response.variable,"~CPC",sep=""))
    }
    
    if(fold.cnt==1 && enableHyperparameterLearning) {
      pos.task = makeRegrTask(id = "pos", data = model.train.df , target = response.variable)
      
      ps = makeParamSet(
        makeIntegerParam(id="ntree",lower = 250L, upper = 500L)
      )
      
      ctrl = makeTuneControlGrid(resolution = 3L)
      rdesc = makeResampleDesc("CV", iters = noFolds)
      
      rf.lrn = tuneParams("regr.randomForest", task = pos.task, resampling = rdesc, par.set = ps,
                          control = ctrl)
      rf.hparam.ntree <- rf.lrn$x$ntree
    }
    
    rf.fit  <- rfsrc(formula,model.train.df,
                     ntree = rf.hparam.ntree,
                     nodesize=2,
                     split.depth = "all.trees",
                     na.action="na.impute",
                     statistics=TRUE,
                     importance = "permute")
    
    #  Predict at new test points
    rf.pred <- predict(rf.fit,newdata = model.test.df)
    rmse.rf.folds <- c(rmse.rf.folds,measureRMSE(test[,eval(response.variable)],rf.pred$predicted))
    
    prediction.frame <- rbind.data.frame(prediction.frame,cbind.data.frame(test[,eval(response.variable)],rf.pred$predicted))
    
  }
  
  #plot(prediction.frame[,1],prediction.frame[,2],type='p',col='blue',xlab = 'observed',ylab = 'predicted')
  #abline(lm(prediction.frame[,2]~prediction.frame[,1]), col="red")
  
  hist(prediction.frame[,1]-prediction.frame[,2],xlab = 'residual', 
       main=paste(subset.keywords[i],"(",nrow(model.train.df),")",sep=""),
       breaks = "FD")
  
  #rmse.btgp.keywords[[i]] <- median(rmse.btgp.folds)
  rmse.rf.keywords[[i]] <- median(rmse.rf.folds)
  
}

#print(median(unlist(rmse.btgp.keywords)))
print(median(unlist(rmse.rf.keywords)))
