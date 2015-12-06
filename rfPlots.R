require("ggplot2") 
require("RColorBrewer") 
require("plot3D") 
require("dplyr") 
require("parallel") 
require("ggRandomForests") 
require(randomForestSRC)

gg_e <- gg_error(rfo)

plot(gg_rfsrc(rfo), alpha=.5)
plot(gg_vimp(rfo))


varsel <- var.select(rfo) 
gg_md <- gg_minimal_depth(varsel)
plot(gg_md)

plot(gg_minimal_vimp(gg_md))

gg_v <- gg_variable(rfo)
xvar <- gg_md$topvars
plot(gg_v, xvar=xvar, panel=TRUE,alpha=.4)
