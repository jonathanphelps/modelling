path = getwd() 
filename = "demInterpolation"
png = FALSE
gif = FALSE
noPoints=100

options = gpOptions()
kStructure <- list("rbf", "white")
lengthScale = c(0.05, 0.1, 0.25, 0.5, 1, 2, 4, 8, 16)

options$kern$comp = kStructure


xTrain = as.matrix(seq(-2,2,length = noPoints))
trueKern = kernCreate(xTrain, list(type = "cmpnd", comp = list("rbf", "white")))

K = kernCompute(trueKern, xTrain)
yTrain = t(gaussSamp(matrix(0, noPoints, 1), K, 1))


#kern = kernCreate(x, "rbf")
#kern$inverseWidth = 5
xTest = as.matrix(seq(-1.3, 1.4, length = 20))

model = gpCreate(dim(xTrain)[2], dim(yTrain)[2], xTrain, 
                 yTrain, options)

inithypers = log(c(1/(lengthScale[1]), 1, model$kern$comp[[2]]$variance))
model = gpExpandParam(model, inithypers)
model = gpOptimise(model, display = TRUE, iters = 400)
opthypers = gpExtractParam(model, only.values = FALSE)
opthypers = exp(opthypers)
ll_opt = gpLogLikelihood(model)


yPred=gpPosteriorMeanVar(model, xTest, varsigma.return=TRUE)
yMu = yPred$mu
yVar = yPred$varsigma

gpPlot(model, xTest, yMu, yVar, ylim = c(-3, 3), col = "black")
