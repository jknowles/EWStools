# Set up and run an analysis
library(caret)
library(EWStools)

# Generate sample data ---------------------------------------------------------
set.seed(442)
train <- twoClassSim(n = 1000, intercept = -8, linearVars = 3, 
                     noiseVars = 10, corrVars = 4, corrValue = 0.6)
test <- twoClassSim(n = 1500, intercept = -7, linearVars = 3, 
                    noiseVars = 10, corrVars = 4, corrValue = 0.6)

ctrl <- trainControl(method = "cv", classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

# Generate sample model ---------------------------------------------------------
fullModel <- train(Class ~ ., data = train, 
                   method = "knn", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "ROC", 
                   trControl = ctrl)

# Build ROCit objects ---------------------------------------------------------

examp1 <- ROCtest(fullModel)

print(examp1)
