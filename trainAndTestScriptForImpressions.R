require(randomForestSRC)
require(mlr)
require(bartMachine)
require(arm)

if("RF" %in% reg.methods){
  method.cnt <- method.cnt + 1
  
  if(fold.cnt==1) {
    method.order <- c(method.order,"RF")
  }
  #Attempt to fit a RandomForest
  
  
  if(fold.cnt==1 && enableHyperparameterLearning) {
    pos.task = makeRegrTask(id = "pos", data = train, target = response.variable)
    
    ps = makeParamSet(
      makeIntegerParam(id="mtry",lower = 6L, upper = 10L),
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
  
  rf.fit  <- rfsrc(reg.formula.amended,
                   train,
                   mtry=rf.hparam.mtry,
                   ntree = rf.hparam.ntree,
                   nodesize = rf.hparam.maxnodes,
                   split.depth = "all.trees",
                   distribution="gaussian",
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
  
  if(fold.cnt==1) {
    method.order <- c(method.order,"lm")
  }
  lm.fit <- lm(reg.formula,train)
  lm.pred <- predict(lm.fit,test,se.fit = T,interval="prediction")
  lm.predMeans[[fold.cnt]] <- lm.pred$fit[,1]
  lm.predVariances[[fold.cnt]] <- lm.pred$fit[,2:3]
  nmsemat[method.cnt,fold.cnt] = measureRMSE(test[,eval(response.variable)],lm.pred$fit[,1])
  maemat[method.cnt,fold.cnt] = measureMAE(test[,eval(response.variable)],lm.pred$fit[,1])
}

if("bartmachine" %in% reg.methods){
  method.cnt <- method.cnt + 1
  
  if(fold.cnt==1) {
    method.order <- c(method.order,"bartMachine")
  }
  
  if("observed.weather" %in% otherVariables) {
    X <- subset(train,select=-c(eval(parse(text=curr.metric)),AvgPosition,level_plus_season,remainder))
    Xtest <- subset(test,select=-c(eval(parse(text=curr.metric)),AvgPosition,level_plus_season,remainder))
    
  } else{
    X <- subset(train,select=-c(eval(parse(text=curr.metric)),AvgPosition,observed.weather))
    Xtest <- subset(test,select=-c(eval(parse(text=curr.metric)),AvgPosition,observed.weather))
  }
  
  Y <- subset(train,select=c(eval(parse(text=curr.metric))))
  
  bm.fit <- bartMachine(X,as.numeric(Y[,curr.metric]),num_burn_in = 750,num_trees=50,
                        q=0.8,k=7,nu=2,alpha=0.5,verbose = FALSE)
  
  Ytest <- predict(bm.fit,Xtest)
  bm.predMeans[[fold.cnt]] <- Ytest
  bm.predVariances[[fold.cnt]] <- calc_prediction_intervals(bm.fit,Xtest)
  nmsemat[method.cnt,fold.cnt] = measureRMSE(test[,eval(response.variable)],Ytest)
  maemat[method.cnt,fold.cnt] = measureMAE(test[,eval(response.variable)],Ytest)
}

if("glm" %in% reg.methods){
  method.cnt <- method.cnt + 1
  
  if(fold.cnt==1) {
    method.order <- c(method.order,"glm")
  }
  zero.indices <- which(train[,response.variable]==0)
  if(length(zero.indices) > 0) train <- train[-zero.indices,]
  glm.fit <- bayesglm(reg.formula,train,family = gaussian("log"))
  glm.pred <- predict(glm.fit,test,type="response",se.fit = T,interval='prediction')
  glm.predMeans[[fold.cnt]] <- glm.pred$fit
  #glm.predVariances[[fold.cnt]] <- glm.pred$fit[,2:3]
  nmsemat[method.cnt,fold.cnt] = measureRMSE(test[,eval(response.variable)],glm.pred$fit)
  maemat[method.cnt,fold.cnt] = measureMAE(test[,eval(response.variable)],glm.pred$fit)    
}
