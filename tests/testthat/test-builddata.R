
context("Test building of data")

set.seed(442)
library(caret)
full <- twoClassSim(n = 1500, intercept = -8, linearVars = 1, 
                     noiseVars = 1, corrVars = 1, corrValue = 0.6)
# test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
#                     noiseVars = 1, corrVars = 1, corrValue = 0.6)

idx <- createDataPartition(full[, "Class"], 1, p = .25)
train <- full[idx[[1]], ]
test <- full[-idx[[1]], ]


split1 <- splitData(full, class = "Class", p = 0.25)
split2 <- splitData(full, class = "Class", p = 0.5)


full$Factor1 <- sample(letters[1:5], nrow(full), replace=TRUE)
full$Factor2 <- sample(letters[9:14], nrow(full), replace=TRUE)

prednames <- c("TwoFactor1", "TwoFactor2", "Linear1", "Nonlinear1", "Nonlinear3", 
              "Corr1", "Factor1", "Factor2")



head(buildModelMatrix(data = full, predvars = prednames, na.omit=TRUE))




table(split1$test$Class)
table(split1$train$Class)

zed <- assembleData(full, class = "Class", p = 0.25, predvars = prednames)

zed <- assembleData(full[full$Factor1 == "e" | 
                           full$Factor1 == "d",], 
                    class = "Class", p = 0.25, predvars = prednames)
