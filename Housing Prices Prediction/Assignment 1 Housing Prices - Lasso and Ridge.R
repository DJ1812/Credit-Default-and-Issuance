library(readxl)
library(dplyr)
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("visdat")
library(visdat)
#install.packages("mice")
library(mice)
#install.packages("sjmisc")
library(sjmisc)
#install.packages("corrplot")
library(corrplot)
library(caret)
#install.packages("glmnet")
library(glmnet)

train_test <- read.csv(file.choose())
train_new <- read.csv(file.choose())
test_new <- read.csv(file.choose())


colSums(is.na(train_test))
md.pattern(train_test)


y_train<-subset(train_new, Id<=1000) #redefine the training data
y_test<-subset(train_new, Id>1000 & Id<=1460) #redefine the validation data

set.seed(2)
y <- y_train$SalePrice
x <- model.matrix(Id~ MSZoning + log(LotArea) + LotConfig + 
                    LandSlope + Neighborhood + Condition1 + OverallQual + OverallCond + 
                   + YearRemodAdd + Exterior1st + MasVnrType + ExterQual + 
                    ExterCond + Foundation + BsmtExposure +  RoofMatl +
                    HeatingQC + CentralAir + KitchenAbvGr + KitchenQual + Functional + 
                    Fireplaces + GarageCars +  TotalSF + TotalRms + age+
                    ScreenPorch + PoolArea + PoolQC + SaleCondition + GrLivArea*Neighborhood+OverallQual*Neighborhood*LotArea +
                    OverallQual*LotArea+ log(GrLivArea)*X1stFlrSF*TotRmsAbvGrd + 
                    YearBuilt*Neighborhood, train_test)[,-1] 
x<-cbind(train_test$Id,x)

# split X into testing, trainig/holdout and prediction as before
x.training<-subset(x,x[,1]<=1000)
x.testing<-subset(x, (x[,1]>=1001 & x[,1]<=1460))
x.prediction<-subset(x,x[,1]>=1461)

#LASSO (alpha=1)
nFolds <- 1
foldid <- sample(rep(seq(nFolds), length.out = nrow(x.training)))
lasso.fit<-glmnet(x = x.training, y = y, alpha = 1, nfolds = nFolds,
                  foldid = foldid,
                  standardize = FALSE)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = x.training, y = y, alpha = 1) #create cross-validation data
#cross validation will split the data set in to train and test and do it multiple time, 10 times, 10 slightly different testing sets and generate errors.
#the dot is the average error and the bars are the confidence intervals, average plus minus one standard deviation

plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-6,-4),ylim=c(0.00,0.005)) # lets zoom-in
lasso.opt.fit <-glmnet(x = x.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
library(caret)
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =x.testing))
mean(abs(lasso.testing-y_test$SalePrice)/y_test$SalePrice*100) #calculate and display MAPE
RMSE(lasso.testing, exp(y_test$SalePrice))
RMSE(log(lasso.testing), y_test$SalePrice) #0.1206665

predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =x.prediction))
predicted.prices.log.i.lasso <- data.frame("SalePrice" = predicted.prices.log.i.lasso)
names(predicted.prices.log.i.lasso)[1] <- "SalePrice" 
head(predicted.prices.log.i.lasso)

predicted.prices.log.i.lasso <- data.frame("Id" = test_new$Id, "SalePrice" = predicted.prices.log.i.lasso$SalePrice) 
write.csv(predicted.prices.log.i.lasso, file = 
            "C:\\Users\\divya\\Documents\\QUEEN'S SMITH MMA\\Predictive Modelling - MMA 867\\Assignment 867\\Assignment 1\\house-prices-advanced-regression-techniques\\submission_house_lasso.csv", row.names=FALSE) # export the predicted prices into a CSV file



#ridge (alpha=0)
ridge.fit<-glmnet(x = x.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = x.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = x.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =x.testing))
mean(abs(ridge.testing-y_test$SalePrice)/y_test$SalePrice*100) 
RMSE(log(ridge.testing), (y_test$SalePrice)) #0.1195857

predicted.prices.log.i.ridge <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =x.prediction))
predicted.prices.log.i.ridge <- data.frame("SalePrice" = predicted.prices.log.i.ridge)
names(predicted.prices.log.i.ridge)[1] <- "SalePrice" 
head(predicted.prices.log.i.ridge)

predicted.prices.log.i.ridge <- data.frame("Id" = test_new$Id, "SalePrice" = predicted.prices.log.i.ridge$SalePrice) 
write.csv(predicted.prices.log.i.ridge, file = 
            "C:\\Users\\divya\\Documents\\QUEEN'S SMITH MMA\\Predictive Modelling - MMA 867\\Assignment 867\\Assignment 1\\house-prices-advanced-regression-techniques\\submission_house_ridge.csv", row.names=FALSE) # export the predicted prices into a CSV file

