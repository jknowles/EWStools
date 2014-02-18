# Regression tests and mlbenchmarks

set.seed(442)
library(caret)
train <- twoClassSim(n = 500, intercept = -8, linearVars = 5, 
                        noiseVars = 4, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 5, 
                       noiseVars = 4, corrVars = 4, corrValue = 0.6)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, classProbs = TRUE, 
                     summaryFunction = twoClassSummary)


fullModel <- train(Class ~ ., data = train, 
                   method = "svmRadial", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "ROC", 
                   trControl = ctrl)

fullModel


# Tests
# First correct ROC on the current data
# ROC on current data with custom threshold preserved
# ROC on new data
# ROC on new data with custom threshold
# Inspect all ROC objects

# fullTest <- roc(test$Class, 
#                 predict(fullModel, test, type = "prob")[,1], 
#                 levels = rev(levels(test$Class)))
# fullTest

res1 <- ROCtest.train(fullModel)

res2 <- ROCtest.train(fullModel, best.method="closest.topleft", 
                      best.weights=c(10, .33))

res3 <- ROCtest.train(fullModel, best.method="closest.topleft", 
                      best.weights=c(100, .66))

roc_plot.train(fullModel, 100, newdata = train)

# Test summary is accurate
summary(res1)

# Test if confusion matrix function is accurate

confuse_mat.train(fullModel, thresh=0.99, prop=TRUE)
confuse_mat.train(fullModel, thresh=0.99, prop=FALSE)
confuse_mat.train(fullModel, thresh=0.01, prop=FALSE)
confuse_mat.train(fullModel, thresh=0.01, prop=FALSE)

# Test if confusion matrix produces predictions on new data well

confuse_mat.train(fullModel, thresh = 0.99, prop=FALSE, 
                  newdata = list(preds = test[, -19], y = test[, 19]))

confuse_mat.train(fullModel, thresh = 0.5, prop=TRUE, 
                  newdata = list(preds = test[, -19], y = test[, 19]))


# Create a plot function

roc_plot.train(fullModel, s = 20, 
               newdata = list(preds = test[, -19], y = test[, 19]))

