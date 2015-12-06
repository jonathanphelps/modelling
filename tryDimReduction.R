library(Rtsne)

dim.df <- model.data
dim.df$weekno <- as.numeric(dim.df$weekno)

dim.formula <- as.formula("AvgPosition~Impressions+Clicks+CPC+level_plus_season+remainder")
dim.data <- model.frame(dim.formula,dim.df)
if("MaxCpc" %in% colnames(dim.data)) dim.data <- subset(dim.data,MaxCpc > CPC)
dim.data <- subset(dim.data,!(AvgPosition<1))

#dim.formula <- as.formula(reg.formula)
#dim.data <- model.frame(dim.formula,mydf)

data_matrix <- as.matrix(dim.data)

#data_matrix <- subset(data_matrix,select=-c(Day,AvgCPCBidGap.,weekno,level,slope,season,season_delta))

set.seed(42) # Set a seed if you want reproducible results
tsne_out <- Rtsne(data_matrix) # Run TSNE

# Show the objects in the 2D tsne representation
points2D(tsne_out$Y[,1],tsne_out$Y[,2],colvar = data_matrix[,2])
text2D(tsne_out$Y[,1],tsne_out$Y[,2],labels=seq(1:nrow(data_matrix)),cex= 0.5, pos=3)
