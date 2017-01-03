library(glmnet)

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

x <- data.matrix(train[,feature.names])
y <- train$SalePrice

lassoReg <- cv.glmnet(x,y,alpha = 1)
minLambda <- lassoReg$lambda.min
nx <- data.matrix(test[,feature.names])
lassoPred <- predict(lassoReg, s = minLambda, newx = nx)
submission <- read_csv("data/sample_submission.csv")
submission$SalePrice <- lassoPred

write.csv(submission, paste0("submissions/",format(Sys.time(), "%m-%d-%y_%H:%M"),"_LASSO.csv"), row.names = FALSE)
