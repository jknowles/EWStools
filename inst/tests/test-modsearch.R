# Test modsearch

# For train objects
# For glm objects
# others?!?


context("Test train objects")

set.seed(442)
library(caret)
train <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
                    noiseVars = 10, corrVars = 2, corrValue = 0.6)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, classProbs = TRUE, 
                     summaryFunction = twoClassSummary)


fullModel <- train(Class ~ ., data = train, 
                   method = "svmRadial", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "ROC", 
                   trControl = ctrl)

modobj <- modAcc(fullModel, datatype = c("train", "test"), modelKeep = FALSE, 
                 testdata = list(preds = test[, -19], class = test[, 19]))

dfExtract(modobj)

modobj <- modAcc(fullModel, datatype = c("test"), modelKeep = FALSE, 
                 testdata = list(preds = test[, -19], class = test[, 19]))

dfExtract(modobj)
