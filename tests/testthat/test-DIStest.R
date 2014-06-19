
set.seed(442)
library(caret)
train <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
                    noiseVars = 10, corrVars = 2, corrValue = 0.6)


ctrl <- trainControl(method = "cv", 
                     number = 3, classProbs = TRUE, 
                     summaryFunction = fourStatsSummary)

fullModel <- train(Class ~ ., data = train, 
                   method = "knn", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "Dist", maximize = FALSE,
                   trControl = ctrl)



DIStest(fullModel)@confusematrix
dist(DIStest(fullModel)@coords)

fullModel <- train(x = train[, 1:18], y = train[, 19], 
                   method = "lda2", 
                  # preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "Dist", maximize = FALSE,
                   trControl = ctrl)

DIStest(fullModel, testdata = list(preds = test[, 1:18], class = test[, 19]))

dfExtract(modAcc(fullModel, datatype = c("test", "train"), 
       testdata = list(preds = test[, 1:18], class = test[, 19])))

EWStools:::probExtract(fullModel, testdata = list(preds = test[, 1:18], class = test[, 19]))
yhats <- predict(fullModel, newdata = test, type = "prob")

fullModel <- train(Class ~ ., data = train, 
                   method = glmDist, 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "Dist", maximize = FALSE,
                   trControl = ctrl)




test1 <- modTest(method = "svmRadial", datatype = c("train", "test"), 
                 traindata = list(preds = train[, -19], class = train[, 19]), 
                 testdata = list(preds = test[, -19], class = test[, 19]), 
                 modelKeep = FALSE, length = 6, fitControl = ctrl, 
                 metric = "Dist")


dfExtract(test1)

resultSet <- modSearch(methods = c("knn", "glm", "svmRadial"), 
                       datatype = c("train", "test"), 
                       traindata = list(preds = train[, -19], class = train[, 19]), 
                       testdata = list(preds = test[, -19], class = test[, 19]), 
                       modelKeep = FALSE, length = 6, fitControl = ctrl, 
                       metric = "Dist")

########################
# 
library(microbenchmark)

train <- twoClassSim(n = 200000, intercept = -8, linearVars = 1, 
                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
                    noiseVars = 10, corrVars = 2, corrValue = 0.6)


ctrl <- trainControl(method = "cv", 
                     number = 3, classProbs = TRUE, 
                     summaryFunction = fourStatsSummary)

fullModel <- train(x = train[, -19], y = train[, 19],
                   method = "glm", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "Dist", maximize = FALSE,
                   trControl = ctrl)

yhats <- EWStools:::probExtract(fullModel)

mroc <- roc(yhats$.outcome, yhats$yhat, percent=TRUE, algorithm=0)


mroc <- roc(yhats$.outcome, round(yhats$yhat, digits = 2), percent=TRUE, algorithm=0)



