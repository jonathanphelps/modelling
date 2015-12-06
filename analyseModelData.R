require(caret)
require(corrplot)
require(Rtsne)
require(xgboost)
require(stats)
require(knitr)
require(ggplot2)
knitr::opts_chunk$set(cache=TRUE)

list.variables <- all.vars(reg.formula)

ex.df <- mydf[,list.variables]

w <- which( sapply( ex.df, class ) == 'factor' )
ex.df[w] <- lapply( ex.df[w], function(x) as.numeric(as.character(x)) )

zero.var = nearZeroVar(ex.df, saveMetrics=TRUE)
zero.var

corrplot.mixed(cor(ex.df), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")