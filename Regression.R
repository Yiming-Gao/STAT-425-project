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


# Our response is a continuous numeric variable, we want a linear model
g.full <- lm(price ~ ., data = train.reg)
summary(g.full) # R^2 = 0.6563

layout(matrix(c(1,2,3,4),2,2))
plot(g.full)

# prediction with the full model
pred.full = predict(g.full, newdata = test.reg)
rmse(test.reg$price, pred.full) # 0.3043


# Model Selection by AIC & BIC (with leaps package)
b = regsubsets(price~., data = train.reg)
rs = summary(b)
m = dim(train.reg)[1]
msize = 2:14
AIC = m*log(rs$rss/m) + 2*msize
BIC = m*log(rs$rss/m) + msize*log(m)


# AIC model
var.AIC = colnames(rs$which)[rs$which[which.min(AIC),]]
var.AIC

# BIC model
var.BIC = colnames(rs$which)[rs$which[which.min(BIC),]]
var.BIC

# AIC and BIC give same model: price~bathrooms + sqft_living + waterfront
# + view + grade + yr_built + sqft_living15 + sqft_lot15
g.lm = lm(price ~ bathrooms + sqft_living + waterfront
         + view + grade + yr_built + sqft_living15 + sqft_lot15,data = train.reg)
summary(g.lm) # R^2 = 0.649, doesn't decrease too much compared to full model
layout(matrix(c(1,2,3,4),2,2))
plot(g.lm)

# prediction with the model chosen by AIC&BIC
pred.lm = predict(g.lm, newdata = test.reg)
rmse(test.reg$price, pred.lm) # 0.3084, dosen't increase too much




  
  
  
  
  
