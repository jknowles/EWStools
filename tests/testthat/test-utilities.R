# # Test utilities
# set.seed(442)
# library(caret)
# train <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
#                      noiseVars = 10, corrVars = 2, corrValue = 0.6)
# test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
#                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
# 
# ctrl <- trainControl(method = "cv", classProbs = TRUE, 
#                      summaryFunction = twoClassSummary)
# 
# 
# fullModel <- train(Class ~ ., data = train, 
#                    method = "knn", 
#                    preProc = c("center", "scale"), 
#                    tuneLength = 8, 
#                    metric = "ROC", 
#                    trControl = ctrl)
# 
# 
# context("Confusion matrices")
# 
# confuse_mat.train(fullModel)
# confuse_mat.train(fullModel, testdata = list(class = test[, 19], preds=test[, -19]))
# 
# confuse_mat.train(fullModel, thresh = 0.5)
# confuse_mat.train(fullModel, thresh = 0.5, newdata=test)
# 
# confuse_mat.train2(fullModel, thresh = 0.5, newdata = test)
# 
# 
# # ROCplot of fullModel
# 
# 
# 
# ROCplot.train(fullModel)
# ROCplot.train(fullModel, datatype = "test", 
#               testdata =list(class = test[, 19], preds=test[, -19]))



