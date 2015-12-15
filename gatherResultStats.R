
if("RF" %in% reg.methods){
  
  methodIndex <- which(method.order == 'RF')
  if(!exists("sel.ix")) {
    sel.ix <- which(maemat[methodIndex,] == min(maemat[methodIndex,]))
    forecaster.query.dates <- fold.test.dates[[sel.ix]]
  }
  rfo <- rf.list[[sel.ix]]
  
  rf.means <- rf.predMeans[[sel.ix]]
  rf.std <- sqrt(rf.predVariances[[sel.ix]])
  rf.upperlimit <- rf.means + (2*rf.std)
  rf.lowerlimit <- rf.means - (2*rf.std)
  
  x <- seq(1,length(rf.means))
  df2 <- cbind.data.frame(x,rf.means,rf.upperlimit,rf.lowerlimit)
  if(dev.cur() == 1) dev.new()
  with(df2,errbar(x,rf.means,rf.upperlimit,rf.lowerlimit))
  points(x,actualMeans[[sel.ix]],col='red',pch=19)
  title(paste(kw.name,"-",matchtype," (",adgroup.name,") ",sep="",collapse = ""))
  
  p.f.name <- paste(getwd(),"./plots/",kw.name,"_rf.tiff",sep="")
  dev.copy(tiff,p.f.name,width=1200, height=600)
  dev.off()
  
  resid <- rf.means - actualMeans[[sel.ix]]
  
  if("AvgPosition1" %in% newLagResponseColNames){
    
    better.outcomes <- length(which((maemat[methodIndex,]/mae.repeat) < 1.0))
    worse.outcomes <- length(which((maemat[methodIndex,]/mae.repeat) >= 1.0))
    
    cat("Stats for RF v One-day repeat (better,worse,rmse)")
    cat('\n')
    cat(better.outcomes)
    cat('\n')
    cat(worse.outcomes)
    cat('\n')
    cat(median(maemat[methodIndex,]/mae.repeat))
  }
  cat(sprintf("\n Overall Average MAE for RF: %3f",mean(maemat[methodIndex,])))
  
}

if("lm" %in% reg.methods){
  methodIndex <- which(method.order == 'lm')
  if(!exists("sel.ix")) sel.ix <- which(maemat[methodIndex,] == min(maemat[methodIndex,]))
  if("AvgPosition1" %in% newLagResponseColNames){
    
    cat('\n')
    better.outcomes <- length(which((maemat[methodIndex,]/mae.repeat) < 1.0))
    worse.outcomes <- length(which((maemat[methodIndex,]/mae.repeat) >= 1.0))
    cat("Stats for Linear Model v One-day repeat (better,worse,rmse)")
    cat('\n')
    cat(better.outcomes)
    cat('\n')
    cat(worse.outcomes)
    cat('\n')
    cat(median(maemat[methodIndex,]/mae.repeat))
  }
  cat(sprintf("\n Overall Average MAE for LM: %3f",mean(maemat[methodIndex,])))
  
}

if("bartmachine" %in% reg.methods){
  
  methodIndex <- which(method.order == 'bartMachine')
  if(!exists("sel.ix")) sel.ix <- which(maemat[methodIndex,] == min(maemat[methodIndex,]))
  
  bm.means <- bm.predMeans[[sel.ix]]
  bm.upperlimit <- bm.predVariances[[sel.ix]][,2]
  bm.lowerlimit <- bm.predVariances[[sel.ix]][,1]
  
  x <- seq(1,length(bm.means))
  df2 <- cbind.data.frame(x,bm.means,bm.upperlimit,bm.lowerlimit)
  if(dev.cur() == 1) dev.new()
  with(df2,errbar(x,bm.means,bm.upperlimit,bm.lowerlimit))
  points(x,actualMeans[[sel.ix]],col='red',pch=19)
  title(paste(kw.name,"-",matchtype," (",adgroup.name,") ",sep="",collapse = ""))
  p.f.name <- paste(getwd(),"./plots/",kw.name,"_bart.tiff",sep="")
  dev.copy(tiff,p.f.name,width=1200, height=600)
  dev.off()
  
  if("AvgPosition1" %in% newLagResponseColNames){
    
    better.outcomes <- length(which((maemat[methodIndex,]/mae.repeat) < 1.0))
    worse.outcomes <- length(which((maemat[methodIndex,]/mae.repeat) >= 1.0))
    cat("Stats for BART v One-day repeat (better,worse,rmse)")
    cat('\n')
    cat(better.outcomes)
    cat('\n')
    cat(worse.outcomes)
    cat('\n')
    cat(median(maemat[3,]/mae.repeat))
  }
  cat(sprintf("\n Overall Average MAE for BART: %3f",mean(maemat[methodIndex,])))
  
}

if("glm" %in% reg.methods){
  
  methodIndex <- which(method.order == 'glm')
  if(!exists("sel.ix")) sel.ix <- which(maemat[methodIndex,] == min(maemat[methodIndex,]))
  if("AvgPosition1" %in% newLagResponseColNames){
    
    cat('\n')
    #bm.means <- bm.predMeans[[sel.ix]]
    #bm.upperlimit <- bm.predVariances[[sel.ix]][,2]
    #bm.lowerlimit <- bm.predVariances[[sel.ix]][,1]
    
    #x <- seq(1,length(bm.means))
    #df2 <- cbind.data.frame(x,bm.means,bm.upperlimit,bm.lowerlimit)
    
    #with(df2,errbar(x,bm.means,bm.upperlimit,bm.lowerlimit))
    #points(x,actualMeans[[sel.ix]],col='red',pch=19)
    
    cat("Stats for Generalised Linear Model v One-day repeat (better,worse,rmse)")
    cat('\n')
    better.outcomes <- length(which((maemat[methodIndex,]/mae.repeat) < 1.0))
    worse.outcomes <- length(which((maemat[methodIndex,]/mae.repeat) >= 1.0))
    
    cat(better.outcomes)
    cat('\n')
    cat(worse.outcomes)
    cat('\n')
    cat(median(maemat[methodIndex,]/mae.repeat))
  }
  cat(sprintf("\n Overall Average MAE for GLM: %3f",mean(maemat[methodIndex,])))
  
}

if(compareWithExistingForecaster){
  t.ix <- sample(seq(1,noFolds),2)
  select.dates <- as.Date(unlist(fold.test.dates[t.ix]))
  source("funcQueryDB.R")
  result.df <- functionQueryResults(kw.name,listing.id,select.dates,device.type)
  
  forecast.result <- cbind.data.frame(select.dates,unlist(actualMeans[t.ix]),unlist(rf.predMeans[t.ix]), 
                                      unlist(bm.predMeans[t.ix]))
  
  colnames(forecast.result) <- c("Date","observed","rf.means","bm.means")
  forecast.result <- merge(forecast.result,result.df,by="Date")
  forecast.result$hour.times.minute <- NULL
  
  if(nrow(forecast.result) > 0){
    cat(sprintf("\n Found %d test points in database",nrow(forecast.result)))
    
    res <- with(forecast.result,measureMAE(observed,rf.means))
    cat(sprintf("\n Overall MAE from RF for select forecaster dates is %.2f",res))
    res <- with(forecast.result,measureMAE(observed,bm.means))
    cat(sprintf("\n Overall MAE from BART for select forecaster dates is %.2f",res))
    res <- with(forecast.result,measureMAE(observed,ImpM))
    cat(sprintf("\n Overall MAE from Forecaster for same dates is %.2f \n",res))
  }
}


