source("~/GitHub/Kaggle_homeprices/cleaning/eda.R")
library(xgboost)
library(glmnet)
library(caret)
set.seed(10)

# Setting feature names
feature.names <- names(train)[ - which(names(train) == "SalePrice")]
remove <- c("SalePrice.BC")
feature.names <- setdiff(feature.names, remove)
rm(remove)

# Converting characters to integers 
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}
rm(levels)
rm(f)

# Create Training and Validation Sets
bound <- floor((nrow(train)/4)*3)         #define % of training and test set
train <- train[sample(nrow(train)), ]           #sample rows 
test.val <- train[(bound+1):nrow(train), ]    #get test set
train.val <- train[1:bound, ]
rm(bound)

##############################################################################################
# Training an XGBoost model
clf <- xgboost(data        = data.matrix(train.val[,feature.names]),
               label       = train.val$SalePrice.BC,
               nrounds     = 25,
               objective   = "reg:linear")
errorCheck <- test.val[,c("Id", "SalePrice")]
errorCheck$XGBSalePrice <- predict(clf, data.matrix(test.val[,feature.names]))
errorCheck$XGBSalePrice <- InvBoxCox(errorCheck$XGBSalePrice, BC.Lambda)
##############################################################################################
# Train Lasso Regression Model
x <- data.matrix(train.val[,feature.names])
y <- train.val$SalePrice # 
lassoReg <- cv.glmnet(x,y,alpha = 1)
minLambda <- lassoReg$lambda.min
nx <- data.matrix(test.val[,feature.names])
lassoPred <- predict(lassoReg, s = minLambda, newx = nx)
errorCheck$LassoSalePrice <- lassoPred
##############################################################################################
# Errors
errorCheck$Average <- rowMeans(errorCheck[,c("XGBSalePrice", "LassoSalePrice")])
errorCheck$XGBerror <- abs(errorCheck$XGBSalePrice - errorCheck$SalePrice)
errorCheck$Lassoerror <- abs(errorCheck$LassoSalePrice - errorCheck$SalePrice)
errorCheck$AvgError <- abs(errorCheck$Average- errorCheck$SalePrice)
RMSE(errorCheck$XGBSalePrice, errorCheck$SalePrice)
RMSE(errorCheck$LassoSalePrice, errorCheck$SalePrice)
RMSE(errorCheck$Average, errorCheck$SalePrice)
