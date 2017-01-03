library(readr)
# library(caret)
# library(xgboost)
# Set Seed
set.seed(621)
BC.Lambda <- .08

setwd("~/GitHub/Kaggle_homeprices/")
train <-  read_csv("data/train.csv")
test <-  read_csv("data/test.csv")
homeVal <- train$SalePrice
df <- rbind(train[,-81],test)
attach(train)

# Clean up NA values
for (v in names(df)) {
  vals <- is.na(df[[v]])
  if (length(unique(vals)) > 1) {
    print(v)
    if (table(vals)[[2]] < 500){
      modeVal <- NULL
      medianVal <- NULL
      if (class(df[[v]]) == "integer") {
        medianVal <- median(df[[v]], na.rm = TRUE)
        # Convert missing values to medianVal
        df[[v]] <- ifelse(is.na(df[[v]]),medianVal,df[[v]])
      }
      if (class(df[[v]]) == "character") {
        modeVal <- names(sort(table(df[[v]]),decreasing=TRUE)[1])
        if (modeVal == "" || is.na(modeVal)) {
          # If most frequent is blank/missing, use second most frequent
          modeVal <- names(sort(table(df[[v]]),decreasing=TRUE)[2])
        }
        df[[v]] <- ifelse(is.na(df[[v]]),modeVal,df[[v]] )
      }
    } else {
      df[[v]] <- ifelse(is.na(df[[v]]),"NONE",df[[v]] )
    }
  }
}
rm(medianVal)
rm(modeVal)
rm(v)
rm(vals)
train <- df[1:1460,]
train$SalePrice <- homeVal
test <- df[1461:2919,]
rm(homeVal)

train$Utilities <- NULL
test$Utilities <- NULL

# Garage
## large finished garage
train$FE_Garage1 <- ifelse(train$GarageArea > mean(train$GarageArea), 1, 0)
test$FE_Garage1 <- ifelse(test$GarageArea > mean(test$GarageArea), 1, 0)

 
###
###
###  I want to do more here
###
###

# Amount of living space
train$GrLivArea <- ifelse(train$GrLivArea > 4000, 4000, train$GrLivArea)
test$GrLivArea <- ifelse(test$GrLivArea > 4000, 4000, test$GrLivArea)
train$FE_BsmtFin <- ifelse(train$BsmtFinType1 == "Unf", 0, 1)
test$FE_BsmtFin <- ifelse(test$BsmtFinType1 == "Unf", 0, 1)
train$FE_BsmtFin2 <- ifelse(train$FE_BsmtFin == 1, train$GrLivArea + train$BsmtFinSF1, train$GrLivArea)
test$FE_BsmtFin2 <- ifelse(test$FE_BsmtFin == 1, test$GrLivArea + test$BsmtFinSF1, test$GrLivArea)
train$FE_BsmtFin3 <- log(train$FE_BsmtFin2)
test$FE_BsmtFin3 <- log(test$FE_BsmtFin2)
train$FE_QualLiv <- -(train$FE_BsmtFin2 * log(train$OverallQual*.01))
test$FE_QualLiv <- -(test$FE_BsmtFin2 * log(test$OverallQual*.01))

# Neighborhood
train$FE_Neighborhood <- NA
test$FE_Neighborhood <- NA
nhWeight <- .8
lowNH <- c("MeadowV", "IDOTRR", "BrDale", "BrkSide", "Edwards", "OldTown")
mdlowNH <- c("Sawyer", "Blueste", "SWISU", "NPkVill", "NAmes", "Mitchel")
mdNH <- c("SawyerW", "NWAmes", "Gilbert", "Blmngtn", "CollgCr")
mdhighNH <- c("Crawfor", "ClearCr", "Somerst", "Veenker", "Timber")
highNH <- c("StoneBr", "NridgHt", "NoRidge")
nhVars <- c("lowNH", "mdlowNH", "mdNH", "mdhighNH","highNH")
for (i in nhVars){
  v <- get(i)
  train$FE_Neighborhood <- ifelse(train$Neighborhood %in% v, train$FE_BsmtFin2 * nhWeight, train$FE_Neighborhood)
  test$FE_Neighborhood <- ifelse(test$Neighborhood %in% v, test$FE_BsmtFin2 * nhWeight, test$FE_Neighborhood)
  nhWeight <- nhWeight + .1
}
rm(lowNH)
rm(mdlowNH)
rm(mdNH)
rm(mdhighNH)
rm(highNH)
rm(nhVars)
rm(nhWeight)

train$FE_LandContour <- ifelse(train$LandContour == "Lvl", train$FE_Neighborhood, train$FE_Neighborhood * .7)
test$FE_LandContour <- ifelse(test$LandContour == "Lvl", test$FE_Neighborhood, test$FE_Neighborhood * .7)
#
#
#
# LotShape, Condition1,2 
#
#
#

# Date
lowMonth = c(2,4:6)
train$FE_MoSold <- ifelse(train$MoSold %in% lowMonth, 0,1)
test$FE_MoSold <- ifelse(test$MoSold %in% lowMonth, 0,1)
rm(lowMonth)

# Manipulating MSSubClass
old <- c(30,70)
train$MSSubClassOld <- ifelse(train$MSSubClass %in% old, 1,0)
test$MSSubClassOld <- ifelse(test$MSSubClass %in% old, 1,0)
new <- c(20, 60, 120, 160)
train$MSSubClassNew <- ifelse(train$MSSubClass %in% new, 1,0)
test$MSSubClassNew <- ifelse(test$MSSubClass %in% new, 1,0)
rm(old)
rm(new)

# Manipulating MSZoning
res <- c("RH", "RL", "RP", "RM", "FV")
train$MSSubClassRes <- ifelse(train$MSZoning %in% res, 1,0)
test$MSSubClassRes <- ifelse(test$MSZoning %in% res, 1,0)
rm(res)

# Binary Variables
# Interaction Variables 
# binaryV  <- list()
# for (a in names(train)) {
#   if (length(unique(train[[a]])) == 2) {
#     binaryV <- c(a, binaryV)
#   }
# }
# rm(a)
# rm(binaryV)

# # Categorical Variables 
v <- c("OverallQual", "Alley", "LotShape", "LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtFinType1","BsmtFinType2","Heating","HeatingQC","CentralAir","Electrical","KitchenQual","Functional","FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PavedDrive","PoolQC","Fence","MiscFeature","SaleType","SaleCondition")
for(i in names(train)){
  if (is.element(i, v)){
    for(level in unique(train[[i]])){
      train[paste0(i, "_",sub("-","Neg", level))] <- ifelse(train[[i]] == level, 1, 0)
      test[paste0(i, "_",sub("-","Neg", level))] <- ifelse(test[[i]] == level, 1, 0)
    }
  }
}
rm(i)
rm(v)
rm(level)


# Years Since Remodel
train$YearRemodSince <- train$YearRemodAdd - train$YearBuilt
test$YearRemodSince <- test$YearRemodAdd - test$YearBuilt
train$YearRemodSinceLog <- log(train$YearRemodSince + 1)
test$YearRemodSinceLog <- log(test$YearRemodSince + 1)
train$YearRemodSale <- train$YrSold - train$YearRemodAdd
test$YearRemodSale <- test$YrSold - test$YearRemodAdd
rec <- c(1:4)
train$RecentRemod <- ifelse(train$YearRemodSale %in% rec, 1, 0)
test$RecentRemod <- ifelse(test$YearRemodSale %in% rec, 1, 0)
rm(rec)

library(forecast)
train$SalePrice.BC <- BoxCox(SalePrice, BC.Lambda)






# c <- c("Id", "SalePrice", "FE_Neighborhood", "RecentRemod", "YearRemodSale")
# View(train[,c])
# 
# r <- c(1299, 524, 1183,692)
# View(train[r,])
# 
# v <- Condition1
# aggregate(SalePrice ~ v, train, mean)
# table(v)


