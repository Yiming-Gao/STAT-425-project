library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
# magrittr and data.table are to make the code cleaner and more rapid

# transform dummy variables into numeric form
train.reg$waterfront <- as.integer(train.reg$waterfront)
test.reg$waterfront <- as.integer(test.reg$waterfront)

# transform the dataset into sparse matrix
train.tmp <- as.matrix(train.reg, rownames.force=NA)
test.tmp <- as.matrix(test.reg, rownames.force=NA)
train.xgb <- as(train.tmp, "sparseMatrix")
test.xgb <- as(test.tmp, "sparseMatrix")

train.xgb <- xgb.DMatrix(data = train.xgb[,2:15], label = train.xgb[,"price"])

g.xgb <- xgboost(data = train.xgb, max.depth = 4, eta = 1, nthread = 2,
                 nround = 10, objective = "reg:linear")

importance<-xgb.importance(feature_names = names(train.reg)[2:15], 
                             model = g.xgb)

# Plot the feature importance matrix
library(Ckmeans.1d.dp)

xgb.plot.importance(importance[1:10,])
# In the feature importance above, we can see the first 10 most important features
# gives a color to each bar, k-means clustering is applied to group each feature
# by importance


# prediction
pred.xgb = predict(g.xgb, data.matrix(test.xgb[,-1]))
rmse(test.reg$price, pred.xgb) # 0.02160, the smallest