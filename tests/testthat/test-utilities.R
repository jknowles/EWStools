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


# context("Test confidence interval")

set.seed(2356)

x <- rnorm(10000)

ci(x, scale = 1)
ci(x, scale = 2)

x <- runif(1000, min = -3, max = 3)

ci(x, scale = 1)
ci(x, scale = 2)


# trainStrip
# 
# library(caret)
# train1 <- twoClassSim(n = 50, intercept = -8, linearVars = 1, 
#                      noiseVars = 10, corrVars = 2, corrValue = 0.6)
# train2 <- twoClassSim(n = 500, intercept = -7, linearVars = 1, 
#                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
# train3 <- twoClassSim(n = 25000, intercept = -7, linearVars = 1, 
#                       noiseVars = 10, corrVars = 2, corrValue = 0.6)
# 
# 
# ctrl <- trainControl(method = "cv", 
#                      number = 3, classProbs = TRUE, 
#                      summaryFunction = twoClassSummary)
# 
# myFit <- train(Class ~ ., data = train3, 
#                method = "knn", 
#                tuneLength = 8, 
#                metric = "ROC", maximize = TRUE,
#                trControl = ctrl)
# 
# pryr::object_size(myFit)
# lapply(myFit, pryr::object_size)
# lapply(myFit$finalModel, pryr::object_size)
# 
# myFit$control <- NULL
# myFit$trainingData <- NULL
# 
# 
# 
# out <- predict(myFit, type = "prob")
# out <- predict(myFit, type = "prob", newdata = train3)
# 

