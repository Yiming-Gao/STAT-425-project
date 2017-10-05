library(choroplethr)
library(data.table)
library(testthat)
library(gridExtra)
library(corrplot)
library(GGally)
library(ggplot2)
library(e1071)
library(dplyr) # data manipulation
library(MASS)
library(lmtest)
library(leaps)
library(Metrics)
library(lars)

house <- read.csv("kc_house_data.csv")
# remove unnecessary columns
house.reg <- house[, -c(1:2, 16:19)]
house.reg$waterfront <- as.factor(house.reg$waterfront)

# Split the data set into train and test set, with 70% in a training set
set.seed(1234)
sub <- sample(nrow(house.reg), floor(nrow(house.reg) * 0.7))
train.reg <- house.reg[sub, ]
test.reg <- house.reg[-sub, ]

# get the data frame of price and log(price + 1) for plotting
df <- rbind(data.frame(version = "log(price+1)", x = log(house.reg$price)),
            data.frame(version = "price", x = house.reg$price))
ggplot(data = df) + facet_wrap(~version, ncol = 2, scales = "free_x")+
  geom_histogram(aes(x = x))
# We can see that the original data is highly skewed, we reduce skewness by a
# log transformation


# for numeric feature with excessive skewness, perform log transformation
# first get data type for each feature
numeric_feats <-c(names(house.reg)[1],names(house.reg)[4],names(house.reg)[5],
                  names(house.reg)[11],names(house.reg)[12],names(house.reg)[14],
                  names(house.reg)[15])

# determine skew for each numeric feature
skewed_feats <- sapply(numeric_feats,function(x){skewness(house.reg[[x]],na.rm=TRUE)})

# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[skewed_feats > 0.75]

# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  train.reg[[x]] <- log(train.reg[[x]] + 1)
  test.reg[[x]] <- log(test.reg[[x]] + 1)
}


# randomForest
library(caret)
library(party)
library(randomForest)
set.seed(1)

rfModel = randomForest(price~., data = train.reg)
te.tree = predict(rfModel, newdata=test.reg)
mean((te.tree-test.reg$price)^2)
obj = randomForest(price~., data = train.reg,importance=TRUE)
## Gini importance
obj$importance 
importance(obj, type=2)
## permutation importance (not necessary)
importance(obj, type=1)
obj1 <- cforest(price~.,data=train.reg)#if your predictor variables are highly correlated:
varimp(obj1)
VI_C = varimp(obj1)
### over all trees:raw importance (not necessary)
importance(obj, type=1, scale=FALSE)
### over all trees:scaled importance (z-score)
importance(obj, type=1, scale=TRUE)

VI_F=importance(obj,type=1)
varImp(obj)
varImpPlot(obj,type=2)
barplot(t(VI_F/sum(VI_F)))
barplot(t(VI_C/sum(VI_C)))#if your predictor variables are highly correlated