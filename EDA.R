library(data.table)
library(testthat)
library(gridExtra)
library(corrplot)
library(GGally)
library(ggplot2)
library(e1071)
library(dplyr)

train<-fread('train.csv', colClasses = c('MiscFeature' = "character",
                                         'PoolQC' = 'character',
                                         'Alley' = 'character'))
test<-fread('test.csv', colClasses = c('MiscFeature' = "character",
                                         'PoolQC' = 'character',
                                         'Alley' = 'character'))

cat_var<-names(train)[which(sapply(train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]


# Structure of the data (1460 rows and 81 features, target feature Sale Price)
dim(train)

str(train)



# Summarize the missing values in the data
# calculate the total number of missing values for each variable
colSums(sapply(train, is.na))

# calculate the total number of missing values of categorical variables
colSums(sapply(train[,.SD, .SDcols = cat_var], is.na))

# calculate the total number of missing values of numeric variables
colSums(sapply(train[,.SD, .SDcols = numeric_var], is.na))



# Summarize the numeric values and the structure of the data
summary(train[,.SD, .SDcols = numeric_var])
cat('Train has', dim(train)[1], 'rows and', dim(train)[2], 'columns.')
cat('Test has', dim(test)[1], 'rows and', dim(test)[2], ' columns.')

# The percentage of data missing in train.
sum(is.na(train)) / (nrow(train) *ncol(train))

# The percentage of data missing in test.
sum(is.na(test)) / (nrow(test) * ncol(test))

# Check for duplicated rows.
cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))

# Convert character to factors
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]

train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}



# Barplots for the categorical features
doPlots(train_cat, fun = plotHist, ii = 1:4, ncol = 2)

doPlots(train_cat, fun = plotHist, ii  = 4:8, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 8:12, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 13:18, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 18:22, ncol = 2)


# LandSlope
train %>% select(LandSlope, Neighborhood, SalePrice) %>% filter(LandSlope == c('Sev', 'Mod')) %>% arrange(Neighborhood) %>% group_by(Neighborhood, LandSlope) %>% summarize(Count = n()) %>% ggplot(aes(Neighborhood, Count)) + geom_bar(aes(fill = LandSlope), position = 'dodge', stat = 'identity') + theme_light() +theme(axis.text.x = element_text(angle = 90, hjust =1))


# Plot a boxplot between the neighboorhoods
train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')

# Density plots for numeric variables
doPlots(train_cont, fun = plotDen, ii = 2:6, ncol = 2)



# Explore the correlation
correlations <- cor(na.omit(train_cont[,-1, with = FALSE]))

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

# Plot scatter plot for variables that have high correlation
train %>% select(OverallCond, YearBuilt) %>% ggplot(aes(factor(OverallCond),YearBuilt)) + geom_boxplot() + xlab('Overall Condition')

plotCorr <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}


highcorr <- c(names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] > 0.5)], names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])

data_corr <- train[,highcorr, with = FALSE]


doPlots(data_corr, fun = plotCorr, ii = 1:6)





# an example for overfitting
# https://www.kaggle.com/ozagordi/house-prices-advanced-regression-techniques/a-clear-example-of-overfitting/comments