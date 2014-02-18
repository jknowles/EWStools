# Simulate student data

library(devtools)
#install_github("jknowles/datasynthR")
library(datasynthR)
library(eeptools)
library(arm)



set.seed(442)
library(caret)
train <- twoClassSim(n = 1000, intercept = -8, linearVars = 15, 
                     noiseVars = 4, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -8, linearVars = 15, 
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
