library(mlr)
library(tgp)
data(BostonHousing, package = "mlbench")
bh.task = makeRegrTask(id = "bh", data = BostonHousing, target = "medv")

n = getTaskSize(bh.task)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)


lrn = makeLearner("regr.btlm")
mod = train(lrn, bh.task, subset = train.set)

task.pred = predict(mod, task = bh.task, subset = test.set)


regr.lrn.rf = makeLearner("regr.randomForest",predict.type = 'se',
                       par.vals = list(mtry=5,ntree=150))

mod2 = train(regr.lrn.rf, bh.task, subset = train.set)

task.pred.rf = predict(mod2, task=bh.task,subset = test.set)


ps = makeParamSet(
  makeIntegerParam(id="mtry",lower = 2L, upper = 7L),
  makeIntegerParam(id="ntree",lower = 50L, upper = 500L)
)

ctrl = makeTuneControlGrid(resolution = 4L)
rdesc = makeResampleDesc("CV", iters = 5L)

res = tuneParams("regr.randomForest", task = bh.task, resampling = rdesc, par.set = ps,
                 control = ctrl)

rf.lrn = setHyperPars(makeLearner("regr.randomForest",
                                  predict.type = 'se',par.vals = res$x ))

mod3 = train(rf.lrn, bh.task, subset = train.set)

task.pred.rf.lrn = predict(mod3, task=bh.task,subset = test.set)
performance(task.pred.rf.lrn)

