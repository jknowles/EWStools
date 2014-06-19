# # Test pROC algorithm speeds
# 
# ########################
# # 
# library(microbenchmark)
# 
# train <- twoClassSim(n = 200000, intercept = -8, linearVars = 1, 
#                      noiseVars = 10, corrVars = 2, corrValue = 0.6)
# test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
#                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
# 
# 
# ctrl <- trainControl(method = "cv", 
#                      number = 3, classProbs = TRUE, 
#                      summaryFunction = fourStatsSummary)
# 
# fullModel <- train(x = train[, -19], y = train[, 19],
#                    method = "glm", 
#                    preProc = c("center", "scale"), 
#                    tuneLength = 8, 
#                    metric = "Dist", maximize = FALSE,
#                    trControl = ctrl)
# 
# yhats <- EWStools:::probExtract(fullModel)
# 
# mroc <- roc(yhats$.outcome, yhats$yhat, percent=TRUE, algorithm=0)
# mroc <- roc(.outcome ~ yhat, data = yhats, percent=TRUE, algorithm=0)
# 
# mroc <- roc(yhats$.outcome, round(yhats$yhat, digits = 2), percent=TRUE, algorithm=0)
# yhats$zed <- round(yhats$yhat, digits = 4)
# mroc <- roc(.outcome ~ zed, data = yhats, percent=TRUE, algorithm=0)
# 
# 
