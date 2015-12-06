rm(list= ls())

### install if not there
pkg_check <- function(x){
    if (!require(x,character.only = TRUE)){ #-- TRUE if not in our repository
        install.packages(x,dep=TRUE)  #-- install with dependent packages
        if(!require(x,character.only = TRUE)) stop("Package not found") 
    }
    require(x, character.only = T)
}

pkg_check('mlr')
pkg_check('tgp')
pkg_check('lubridate')
pkg_check('chron')
pkg_check('dplyr')
pkg_check('randomForestSRC')
pkg_check('arm')

path <- 'C:/CODES/Insight SVN2/Releases/PLA/15-11-14 (max cpc)/maxcpc dev scripts/'
setwd(path)

enableHyperparameterLearning <- FALSE
includeDOW <- FALSE
#includeDOW <- TRUE

ddf <- read.csv('./Data/bid_Data.csv')
ddf$Landing.Date <- chron(as.character(ddf$Landing.Date), format = c('y-m-d'))
### Jonny - setup. No kw loop so need to do differently



chooseN <- 9
noFolds <- 5
list.keywords <- unique(ddf$kw)
response.variable <- "Max.Historical.Bid"

subset.keywords <- sample(list.keywords,chooseN)

par(mfrow=c(3,3))

rmse.btgp.keywords <- list()
rmse.rf.keywords <- list()
rmse.glm.keywords <- list()


pred.btgp.keywords <- list()
pred.rf.keywords <- list()
pred.glm.keywords <- list()

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
  data <- data[complete.cases(data),]
  rownames(data) <- NULL
  
  N <- nrow(data)
  
  rmse.btgp.folds <- c()
  rmse.rf.folds <- c()
  rmse.glm.folds <- c()
  prediction.frame <- data.frame()
  prediction.frame.glm <- data.frame()
  
  folds <- cut(seq(1,N),breaks = noFolds,labels = FALSE)
  
  ### need for glm
  fdates <- data$Landing.Date
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
        makeIntegerParam(id="mtry",lower = 4L, upper = 8L),
        makeIntegerParam(id="ntree",lower = 250L, upper = 500L)
      )
      
      ctrl = makeTuneControlGrid(resolution = 3L)
      rdesc = makeResampleDesc("CV", iters = noFolds)
      
      rf.lrn = tuneParams("regr.randomForest", task = pos.task, resampling = rdesc, par.set = ps,
                          control = ctrl)
      rf.hparam.mtry <- rf.lrn$x$mtry
      rf.hparam.ntree <- rf.lrn$x$ntree
    }
    
    rf.fit  <- rfsrc(formula,model.train.df,
                     ntree = rf.hparam.ntree,
                     split.depth = "all.trees",
                     importance = "permute")
    
    #  Predict at new test points
    rf.pred <- predict(rf.fit,newdata = model.test.df)
    rmse.rf.folds <- c(rmse.rf.folds, measureRMSE(test[,eval(response.variable)], rf.pred$predicted))
    
    prediction.frame <- rbind.data.frame(prediction.frame,cbind.data.frame(test[,eval(response.variable)],rf.pred$predicted))
    
    
    #-------------------------------------
    # Log Bayes GLM
    #-------------------------------------
    # need to drop the dates from ddf instead
    d0 <- ddf
    d0 <- d0[d0$Clicks > 1,]
    d0$kw <- factor(d0$kw)
    d0  <- d0[!is.na(d0$CPC),]
    d0  <- d0[d0$Max.Historical.Bid > d0$CPC,]
    d0$bid.gap <- d0$Max.Historical.Bid - d0$CPC
    qc <- quantile(d0$bid.gap, 0.975)
    d0 <- d0[d0$bid.gap < qc,]
    drop.dates <- fdates[folds == fold.cnt]
    
    train0  <- d0[!d0$Landing.Date %in% drop.dates | d0$kw != subset.keywords[i],]

    bg.mod <- bayesglm(bid.gap ~ CPC + kw, data = train0, family = Gamma("log"))
    
    train0$bid.gap.m <- predict(bg.mod, newdata = train0, 'response')
    train0$Max.CPC.m <- train0$CPC + train0$bid.gap.m
    
    test0   <- ddf[ddf$Landing.Date %in% drop.dates & ddf$kw == subset.keywords[i],]
    test0$kw <- factor(test0$kw)
    test0    <- test0[!is.na(test0$CPC),]
    test0$bid.gap.m <- predict(bg.mod, newdata = test0, 'response')
    test0$Max.CPC.m <- test0$CPC + test0$bid.gap.m
    
    glm.out <- measureRMSE(test0[,eval(response.variable)],test0$Max.CPC.m)
    
    rmse.glm.folds <- c(rmse.glm.folds, glm.out)
    prediction.frame.glm <- rbind.data.frame(prediction.frame.glm,
                                             cbind.data.frame(test0$Max.Historical.Bid, test0$Max.CPC.m))
  }
  
  #plot(prediction.frame[,1],prediction.frame[,2],type='p',col='blue',xlab = 'observed',ylab = 'predicted')
  #abline(lm(prediction.frame[,2]~prediction.frame[,1]), col="red")
  
  ### residual
  pred.frame.out <- prediction.frame[,1]-prediction.frame[,2]
  pred.frame.glm.out <- prediction.frame.glm[,1]-prediction.frame.glm[,2]
  
  
  
  ### rf
  hist(pred.frame.out ,xlab = 'residual', 
       main=paste(subset.keywords[i],"(",nrow(model.train.df),") - RF",sep=""),
       breaks = "FD", xlim = range(pred.frame.out, pred.frame.glm.out))

  
  ### GLM
  hist(pred.frame.glm.out,xlab = 'residual', 
       main=paste(subset.keywords[i],"(",nrow(model.train.df),") - GLM",sep=""),
       breaks = "FD", xlim = range(pred.frame.out, pred.frame.glm.out))
  
  pred.rf.keywords[[i]]  <- prediction.frame
  pred.glm.keywords[[i]] <- prediction.frame.glm
  
  #rmse.btgp.keywords[[i]] <- median(rmse.btgp.folds)
  rmse.rf.keywords[[i]]  <- median(rmse.rf.folds)
  rmse.glm.keywords[[i]] <- median(rmse.glm.folds)
  
}

#print(median(unlist(rmse.btgp.keywords)))
print(median(unlist(rmse.rf.keywords)))
print(median(unlist(rmse.glm.keywords)))



pred.rf <- do.call(rbind, pred.rf.keywords)
pred.glm <- do.call(rbind, pred.glm.keywords)

pred.rf.out <- pred.rf[,1] - pred.rf[,2]
pred.glm.out <- pred.glm[,1] - pred.glm[,2]

print(measureRMSE(pred.rf[,1], pred.rf[,2]))
print(measureRMSE(pred.glm[,1], pred.glm[,2]))

### rf
par(mfrow= c(2,1))
hist(pred.rf.out ,xlab = 'residual', 
     main=paste(subset.keywords[i],"(",nrow(model.train.df),") - RF",sep=""),
     breaks = "FD", xlim = range(pred.rf.out, pred.glm.out),
     freq = F)


### GLM
hist(pred.glm.out,xlab = 'residual', 
     main=paste(subset.keywords[i],"(",nrow(model.train.df),") - GLM",sep=""),
     breaks = "FD", xlim = range(pred.rf.out, pred.glm.out), freq = F)
lines(density(pred.rf.out))

