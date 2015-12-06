data("mtcars")
library(xgboost)
require(Matrix)
require(data.table)
library(vcd) 

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mtcars))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

train <- mtcars[train_ind, ]

test <- mtcars[-train_ind, ]

sparse_matrix <- sparse.model.matrix(mpg~.-1, data = train)
output_vector <- train[,"mpg"]
sparse_test_matrix <- sparse.model.matrix(mpg~.-1, data = test)


bst <- xgboost(data = sparse_matrix, label = output_vector,max.depth = 4,
               eta = 0.9, nthread = 2, nround = 8)

bst.pred <- predict(bst, sparse_test_matrix)

# train.X <- subset(train,select=-c(mpg))
# train.Y <- subset(train,select=c(mpg))
# test.X <- subset(test,select=-c(mpg))
# test.Y <- subset(test,select=c(mpg))

print(measureMSE(test$mpg,bst.pred))

rf <- rfsrc(mpg~.,train,ntree = 1500)
rf.pred <- predict(rf,test)
print(measureMSE(test$mpg,rf.pred$predicted))
