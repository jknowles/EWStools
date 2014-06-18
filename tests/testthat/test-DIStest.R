
set.seed(442)
library(caret)
train <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
                    noiseVars = 10, corrVars = 2, corrValue = 0.6)


fourStats <- function (data, lev = levels(data$obs), model = NULL) {
  out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
  coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), 
                   ncol = 2, 
                   byrow = TRUE)
  colnames(coords) <- c("Spec", "Sens")
  rownames(coords) <- c("Best", "Current")
  c(out, Dist = dist(coords)[1])
}

ctrl <- trainControl(method = "cv", 
                     number = 3, classProbs = TRUE, 
                     summaryFunction = fourStats)

fullModel <- train(Class ~ ., data = train, 
                   method = "knn", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "Dist", maximize = FALSE,
                   trControl = ctrl)







fullModel <- train(Class ~ ., data = train, 
                   method = glmDist, 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "Dist", maximize = FALSE,
                   trControl = ctrl)
