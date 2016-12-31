source("~/GitHub/Kaggle_homeprices/cleaning/eda.R")
library(xgboost)

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

# Training an XGBoost model
clf <- xgboost(data        = data.matrix(train[,feature.names]),
               label       = train$SalePrice.BC,
               nrounds     = 25,
               objective   = "reg:linear")
submission <- read_csv("data/sample_submission.csv")
submission$SalePrice <- predict(clf, data.matrix(test[,feature.names]))

if (mean(submission$SalePrice) < 50) {
  submission$SalePrice <- InvBoxCox(submission$SalePrice, BC.Lambda)
}

write.csv(submission, paste0("submissions/",format(Sys.time(), "%m-%d-%y_%H:%M"),".csv"), row.names = FALSE)

########################################################################################
########################################################################################
library(caret)
set.seed(10)

cat("Create Train and Test set")
bound <- floor((nrow(train)/4)*3)         #define % of training and test set
train <- train[sample(nrow(train)), ]           #sample rows 
test.val <- train[(bound+1):nrow(train), ]    #get test set
train.val <- train[1:bound, ]
rm(bound)

flds <- createFolds(1:nrow(train.val), k = 10, list = TRUE, returnTrain = FALSE)
names(flds) <- 1:10

## Setting XGBoost parameters
nrounds <- c(50,60,70)
max.depth <-  c(8:14)
eta <- c(.001,.01,.05,.1,.2)
gamma <- c(.01,.1,1,10)

# Variables to store scores
error <-  list()
n <-  list()
m <-  list()
e <- list()
g <- list()

# Housekeeping
response <- train$SalePrice.BC
flds.num <- 1

for (nr in nrounds){
  for (md in max.depth){
    for (et in eta){
      for (ga in gamma) {
        # Creating index of values for cross validation
        subset <- flds[[flds.num]]
        # Iterate on data
        clf <- xgboost(
          data        = data.matrix(train.val[subset,feature.names]),
          label       = response[subset],
          objective   = "reg:linear",
          nrounds     = nr,
          gamma       = ga,
          max.depth   = md,
          eta         = et)
        xgb.predict <- predict(clf, data.matrix(test.val[,feature.names]))
        er <- RMSE(test.val$SalePrice.BC, xgb.predict) 
        error[paste0("nrounds:",nr," depth:",md," eta:",et," gam: ",ga)] <- er
        ##(tab[1,2] + tab[2,1])/sum(tab)
        n[paste0("nrounds:",nr," depth:",md," eta:",et," gam: ",ga)] <- nr
        m[paste0("nrounds:",nr," depth:",md," eta:",et," gam: ",ga)] <- md
        e[paste0("nrounds:",nr," depth:",md," eta:",et," gam: ",ga)] <- et
        g[paste0("nrounds:",nr," depth:",md," eta:",et," gam: ",ga)] <- ga
        
        # Housekeeping for the cross validation folds
        flds.num <- flds.num + 1
        if (flds.num > 10) {
          flds.num = 1
        }
      }
    }
  }
}
length(error)
# Finding the model with the lowest error
a.max <- which.min(error)
a.max

clf <- xgboost(
  data        = data.matrix(train[subset,feature.names]),
  label       = response[subset],
  objective   = "reg:linear",
  nrounds     = n[[a.max]],
  gamma       = g[[a.max]],
  max.depth   = m[[a.max]],
  eta         = e[[a.max]])
xgb.predict <- predict(clf, data.matrix(test.val[,feature.names]))
sorted <- sort(xgb.predict, decreasing = TRUE)
cut.index <- round(length(sorted)*.1,0)
xgb.predict.cut <- ifelse(xgb.predict > sorted[cut.index], 1,0)
table(xgb.predict.cut, test$RESP)

important <- xgb.importance(feature.names, model = clf)
head(important,20)

