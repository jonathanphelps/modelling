library(arules)
R <- length(maxcpc.change)
trans <- cbind(maxcpc.change[1:(R-1)],pos.change[1:(R-1)],impression.change[2:R])
trans
inspect(trans)
?inspect
?write.csv
s.trans = as(trans, "transactions");
s.trans
inspect(s.trans)
trans[1:2,]
trans
colnames(trans) <- c("max.cpc","position","impressions")
?table
table(trans)
trans <- cbind(as.factor(maxcpc.change[1:(R-1)]),as.factor(pos.change[1:(R-1)]),as.factor(impression.change[2:R]))
trans
colnames(trans) <- c("max.cpc.state","position.state","impressions.state")
table(trans[,1],trans[,3])
?"prop.table"
prop.table(trans)
table(trans[,1],trans[,2],trans[,3])
s.trans = as(trans, "transactions");
inspect(s.trans)
trans <- cbind.data.frame(as.factor(maxcpc.change[1:(R-1)]),as.factor(pos.change[1:(R-1)]),as.factor(impression.change[2:R]))
colnames(trans) <- c("max.cpc.state","position.state","impressions.state")
trans$pattern <- as.character(transform(df,xyz=interaction(max.cpc.state,position.state,impressions.state,sep='')))
trans$pattern <- as.character(transform(trans,xyz=paste0(max.cpc.state,position.state,impressions.state))
)
transform(trans,xyz=paste0(max.cpc.state,position.state,impressions.state))
transform(trans,xyz=paste(max.cpc.state,position.state,impressions.state,sep="-",collapse=""))
str(trans)
trans <- as.character(trans)
str(trans)
trans <- cbind(maxcpc.change[1:(R-1)],pos.change[1:(R-1)],impression.change[2:R])
trans
str(trans)
colnames(trans) <- c("max.cpc.state","position.state","impressions.state")
transform(trans,xyz=paste(max.cpc.state,position.state,impressions.state,sep="-",collapse=""))
colnames(trans) <- c("x","y","z")
trans <- cbind.data.frame(maxcpc.change[1:(R-1)],pos.change[1:(R-1)],impression.change[2:R])
colnames(trans) <- c("x","y","z")
trans
head(trans)
?gsub
head(transform(trans,xyz=paste0(x,y,z)))
trans$xyz <- transform(trans,xyz=paste0(x,y,z))
head(trans)
trans <- cbind.data.frame(maxcpc.change[1:(R-1)],pos.change[1:(R-1)],impression.change[2:R])
colnames(trans) <- c("x","y","z")
trans <- transform(trans,xyz=paste0(x,y,z))
head(trans)
table(trans$xyz)
trans <- cbind.data.frame(maxcpc.change,pos.change)
colnames(trans) <- c("x","y")
trans <- transform(trans,xy=paste0(x,y))
trans
table(trans$xy)
trans <- cbind.data.frame(maxcpc.change[1:(R-1)],pos.change[1:(R-1)],impression.change[2:R])
colnames(trans) <- c("x","y","z")
trans <- transform(trans,xyz=paste0(x,y,z))
View(trans)
