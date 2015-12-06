reg.methods <- c("bartmachine","RF","lm","glm")

curr.metric <- "Impression"

#lagVariables <- c("AvgPosition")
applyLagPositionTransformation <- FALSE
transformation.string <- "log"

#lagVariables <- c("MaxCpc")
no.top.lags.needed <- c(7)
no.lags <- 30

otherVariables.transform <- c("no","no")
names(otherVariables.transform) <- c("observed.weather","AvgPosition")
#otherVariables <- c("level_plus_season","remainder")
enableshuffle <- TRUE
enableHyperparameterLearning <- FALSE
compareWithExistingForecaster <- TRUE
addWeekNo <- TRUE
onlyNumericVars <- FALSE


significance.level <- 0.99
ccf.significance.level <- 0.95

#onlyNumericVars <- TRUE

# Cross-validation
noFolds <- 10

# No. trees for ensemble methods
ntrees <- 150

# RF hyperparameters
rf.hparam.mtry <- 8
rf.hparam.ntree <- ntrees
rf.hparam.maxnodes <- 2

logfile <- "modelling-log.txt"