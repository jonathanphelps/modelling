all.positions <- sample(unique(mydf$AvgPosition),120,replace = F)
#all.positions <- all.positions[-which(all.positions==0)]

#sel.row <- sample.int(nrow(test),1)

bootstrapPosition <- function(sim.data,rfo,all.positions){
  sim.data <- as.data.frame(t(sim.data))
  simulatePrediction <- function(x,rfo){
    sim.data$AvgPosition <- x
    rf.predMeans <- predict(rfo,sim.data)
    return(rf.predMeans$predicted)
  }
  
  result <- sapply(all.positions,simulatePrediction,rfo)
  res.df <- cbind.data.frame(all.positions,result)
  return(res.df)
}

b.results <- apply(test,1,bootstrapPosition,rfo,all.positions)
