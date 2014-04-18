# # 
# # context("Test building of data")
# # 
# set.seed(442)
# library(caret)
# full <- twoClassSim(n = 1500, intercept = -8, linearVars = 1, 
#                      noiseVars = 1, corrVars = 1, corrValue = 0.6)
# test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
#                     noiseVars = 1, corrVars = 1, corrValue = 0.6)
# 
# idx <- createDataPartition(full[, "Class"], 1, p = .25)
# train <- full[idx[[1]], ]
# test <- full[-idx[[1]], ]
# 
# 
# split1 <- splitData(full, class = "Class", p = 0.25)
# split2 <- splitData(full, class = "Class", p = 0.5)
# 
# splitVAL <- splitData(full, class = "Class", p = 0.5, pvalid = 0.75, type = "three")
# 
# # 
# full$Factor1 <- sample(letters[1:5], nrow(full), replace=TRUE)
# full$Factor2 <- sample(letters[9:14], nrow(full), replace=TRUE)
# 
# prednames <- c("TwoFactor1", "TwoFactor2", "Linear1", "Nonlinear1", "Nonlinear3", 
#               "Corr1", "Factor1", "Factor2")
# 
# 
# 
# head(buildModelMatrix(data = full, predvars = prednames, na.omit=TRUE))
# 
# context("Test building of data on matrices")
# make a matrix
# test it
# 
# 
# table(split1$test$Class)
# table(split1$train$Class)
# 
# zed <- assembleData(full, class = "Class", p = 0.25, predvars = prednames)
# zed <- assembleData(full, class = "Class", p = 0.1,  pvalid = .75)
# zed <- assembleData(full[full$Factor1 == "e" | 
#                            full$Factor1 == "d",], 
#                     class = "Class", p = 0.25, predvars = prednames)
# 
# context("Test linear combos omission")
# # 
# testData1 <- matrix(0, nrow=20, ncol=8)
# testData1[,1] <- 1
# testData1[,2] <- round(rnorm(20), 1)
# testData1[,3] <- round(rnorm(20), 1)
# testData1[,4] <- round(rnorm(20), 1)
# testData1[,5] <- 0.5 * testData1[,2] - 0.25 * testData1[,3] - 0.25 * testData1[,4]
# testData1[1:4,6] <- 1
# testData1[5:10,7] <- 1
# testData1[11:20,8] <- 1
# # 
# #colnames(testData1) <- letters[1:8]\
# splits <- splitData(testData1, class = "g", p =.2)
# 
# traindata <- list(preds = splits$train[, names(splits$train) != class], 
#                   class = splits$train[, class])
# testdata <- list(preds = splits$test[, names(splits$test) != class], 
#                  class = splits$test[, class])
# 

# findLinearCombos(testData1)
# 
# omitLinearCombos(testData1)
