# 
# context("Test building of data")
# 

## Test splitData
## Test buildModelMatrix
## Test assembleData
## Test omitLinearCombos

set.seed(442)
library(caret)
full <- twoClassSim(n = 1500, intercept = -8, linearVars = 1, 
                     noiseVars = 1, corrVars = 1, corrValue = 0.6)


idx <- createDataPartition(full[, "Class"], 1, p = .25)
train <- full[idx[[1]], ]
test <- full[-idx[[1]], ]

splits <- list(train = train, test = test, 
            indexes = idx)

splits.tmp <- splitData(full, class = "Class", p = .25)

context("Test that splitData produces correct objects")

test_that("Two way split dimensions are correct", {
  expect_equal(nrow(splits$train), nrow(splits.tmp$train))
  expect_equal(ncol(splits$train), ncol(splits.tmp$train))
  expect_equal(nrow(splits$test), nrow(splits.tmp$test))
  expect_equal(ncol(splits$test), ncol(splits.tmp$test))
  expect_identical(class(splits$indexes), class(splits.tmp$indexes))
  expect_equal(length(splits$indexes), length(splits.tmp$indexes))

})

# test three way split

idx <- createDataPartition(full[, "Class"], 1, p = .25)
train <- full[idx[[1]], ]
test <- full[-idx[[1]], ]

idx2 <- createDataPartition(test[, "Class"], 1, p = 0.1)
valid <- test[idx2[[1]], ]
test <- test[-idx2[[1]], ]


splits <- list(train = train, test = test, valid = valid, 
               indexes = list(idx, idx2))

splits.tmp <- splitData(full, class = "Class", p = .25, pvalid = 0.1)

test_that("Two way split dimensions are correct", {
  expect_equal(nrow(splits$train), nrow(splits.tmp$train), tolerance = 0.01)
  expect_equal(ncol(splits$train), ncol(splits.tmp$train))
  expect_equal(nrow(splits$test), nrow(splits.tmp$test), tolerance = 0.01)
  expect_equal(ncol(splits$test), ncol(splits.tmp$test))
  expect_equal(nrow(splits$valid), nrow(splits.tmp$valid), tolerance = 0.01)
  expect_equal(ncol(splits$valid), ncol(splits.tmp$valid))
  expect_identical(class(splits$indexes), class(splits.tmp$indexes))
  expect_equal(length(splits$indexes), length(splits.tmp$indexes))
})
 
# splits1 <- splitData(full, class = "Class", p = 0)
# splits2 <- splitData(full, class = "Class", p = 1)
# splits3 <- splitData(full, class = "Class", p = 0, pvalid = 0.1)
# splits4 <- splitData(full, class = "Class", p = .5, pvalid = 0)
# splits5 <- splitData(full, class = "Class", p = .5, pvalid = 1)

test_that("splitData handles extreme values", {
  expect_warning(splitData(full, class = "Class", p = 1))
  expect_error(splitData(full, class = "Class", p = 0))
  expect_error(splitData(full, class = "Class", p = 0, pvalid = 0.1))
  expect_error(splitData(full, class = "Class", p = .5, pvalid = 0))
  expect_warning(splitData(full, class = "Class", p = .5, pvalid = 1))
})

split1 <- splitData(full, class = "Class", p = 0.1)
split2 <- splitData(full, class = "Class", p = 0.9)

splitVAL1 <- splitData(full, class = "Class", p = 0.5, pvalid = 0.75)
splitVAL2 <- splitData(full, class = "Class", p = 0.25, pvalid = 0.2)
splitVAL3 <- splitData(full, class = "Class", p = 0.1, pvalid = 0.1)


test_that("splitData gets proportions right",{
  expect_equal(nrow(split1$train), nrow(full) * 0.1, tolerance = 0.01)
  expect_equal(nrow(split2$train), nrow(full) * 0.9, tolerance = 0.01)
  expect_equal(nrow(splitVAL1$train), nrow(full) * 0.5, tolerance = 0.01)
  expect_equal(nrow(splitVAL2$train), nrow(full) * 0.25, tolerance = 0.01)
  expect_equal(nrow(splitVAL3$train), nrow(full) * 0.1, tolerance = 0.01)
  expect_equal(nrow(split1$test), nrow(full) * 0.9, tolerance = 0.01)
  expect_equal(nrow(split2$test), nrow(full) * 0.1, tolerance = 0.01)
  expect_equal(nrow(splitVAL1$test), nrow(full) * 0.125, tolerance = 0.01)
  expect_equal(nrow(splitVAL2$test), nrow(full) * 0.6, tolerance = 0.01)
  expect_equal(nrow(splitVAL3$test), nrow(full) * 0.81, tolerance = 0.01)
  expect_equal(nrow(splitVAL1$valid), nrow(full) * 0.375, tolerance = 0.01)
  expect_equal(nrow(splitVAL2$valid), nrow(full) * 0.15, tolerance = 0.01)
  expect_equal(nrow(splitVAL3$valid), nrow(full) * 0.09, tolerance = 0.01)
})

rm(split1, split2, splitVAL1, splitVAL2, test, train, valid, splitVAL3, 
   idx, idx2, splits, splits.tmp)
######################
## Test modelMatrix
######################

context("Test building of data on matrices")

full$Factor1 <- sample(letters[1:5], nrow(full), replace=TRUE)
full$Factor2 <- sample(letters[9:14], nrow(full), replace=TRUE)

mat1 <- buildModelMatrix(data = full[, -9])

prednames <- c("TwoFactor1", "TwoFactor2", "Linear1", "Nonlinear1", "Nonlinear3", 
               "Corr1", "Factor1", "Factor2")
 
mat2 <- buildModelMatrix(data = full, predvars = prednames)
mat3 <- buildModelMatrix(data = full, predvars = prednames, keepNA = TRUE)

test_that("buildModelMatrix is correct dimensions", {
  expect_identical(mat2, mat3)
  expect_equal(ncol(mat1), ncol(full) -1 + 9)
  expect_equal(nrow(mat1), nrow(mat2))
  expect_equal(nrow(mat2), nrow(mat3))
  expect_equal(nrow(mat2), nrow(full))
  expect_is(mat1, "matrix")
  expect_is(mat2, "matrix")
  expect_is(mat3, "matrix")
})

MCARx <- function(x, p){
  class <- class(x) 
  z <- rbinom(length(x), 1, prob=p)
  x[z==1] <- NA
  return(x)
}

MCAR.df <- function(df, p){
  if(length(p) == 1){
    df <- apply(df, 2, MCARx, p)
  } else if(length(p) > 1) {
    df <- apply(df, 2, MCARx, sample(p, 1))
  }
  df <- as.data.frame(df)
  return(df)
}

full2 <- MCAR.df(full, p = 0.14)
full2[, 1:8] <- lapply(full2[, 1:8], as.numeric)
prednames <- c("TwoFactor1", "TwoFactor2", "Linear1", "Nonlinear1", "Nonlinear3", 
               "Corr1", "Factor1", "Factor2")

mat1 <- buildModelMatrix(data = full2[, -9], keepNA = TRUE)
mat2 <- buildModelMatrix(data = full2, predvars = prednames, keepNA = TRUE)
mat3 <- buildModelMatrix(data = full2, predvars = prednames)


mat1a <- buildModelMatrix(data = full2[, -9], keepNA = FALSE)
mat2a <- buildModelMatrix(data = full2, predvars = prednames, keepNA = FALSE)
mat3a <- buildModelMatrix(data = full2, predvars = prednames)


test_that("buildModelMatrix handles NA correctly", {
  expect_identical(mat2a, mat3a)
  expect_equal(nrow(mat1), nrow(full2))
  expect_equal(nrow(mat2), nrow(full2))
  expect_equal(nrow(mat3), nrow(mat2a))
  expect_less_than(nrow(mat1a), nrow(mat1))
  expect_less_than(nrow(mat2a), nrow(mat2))
  expect_less_than(nrow(mat1a), nrow(mat2a))
  expect_equal(ncol(mat1), ncol(mat1a))
  expect_equal(ncol(mat2), ncol(mat2a))
})

rm(mat1, mat1a, mat2, mat2a)

################
# Test assembleData
###############
context("Test that assembleData functions as expected")

zed1 <- assembleData(full, class = "Class", p = 0.25, predvars = prednames)
zed2 <- assembleData(full, class = "Class", p = 0.1,  pvalid = .75)
zed3 <- assembleData(full[full$Factor1 == "e" | 
                           full$Factor1 == "d",], 
                    class = "Class", p = 0.25, predvars = prednames)


test_that("assembleData objects are correct class and dimensions", {
  expect_equal(length(zed1), 2)
  expect_equal(length(zed2), 3)
  expect_equal(length(zed3), 2)
  expect_is(zed1[[1]], "list")
  expect_is(zed1[[2]], "list")
  expect_is(zed2[[1]], "list")
  expect_is(zed2[[2]], "list")
  expect_is(zed2[[3]], "list")
  expect_is(zed3[[1]], "list")
  expect_is(zed3[[2]], "list")
  expect_equal(names(zed1), names(zed3))
  expect_equal(names(zed2), c("traindata", "testdata", "validdata"))
  expect_equal(names(zed1[[1]]), c("preds", "class"))
  expect_equal(names(zed1[[1]]), names(zed1[[2]]))
  expect_equal(names(zed2[[1]]), c("preds", "class"))
  expect_equal(names(zed2[[1]]), names(zed2[[2]]))
  expect_equal(names(zed2[[1]]), names(zed2[[3]]))
  expect_equal(names(zed3[[1]]), c("preds", "class"))
  expect_equal(names(zed3[[1]]), names(zed3[[2]]))
  expect_is(zed1$traindata$preds, "data.frame")
  expect_is(zed2$traindata$preds, "data.frame")
  expect_is(zed3$traindata$preds, "data.frame")
  expect_is(zed1$testdata$preds, "data.frame")
  expect_is(zed2$testdata$preds, "data.frame")
  expect_is(zed3$testdata$preds, "data.frame")
  expect_is(zed1$traindata$class, "factor")
  expect_is(zed2$traindata$class, "factor")
  expect_is(zed3$traindata$class, "factor")
  expect_is(zed1$testdata$class, "factor")
  expect_is(zed2$testdata$class, "factor")
  expect_is(zed3$testdata$class, "factor")
})


test_that("assembleData objects are correct size", {
  expect_less_than(nrow(zed1$traindata$preds), nrow(zed1$testdata$preds))
  expect_equal(nrow(zed1$traindata$preds), length(zed1$traindata$class))
  expect_equal(nrow(zed1$testdata$preds), length(zed1$testdata$class))
  expect_less_than(nrow(zed2$traindata$preds), nrow(zed2$testdata$preds))
  expect_equal(nrow(zed2$traindata$preds), length(zed2$traindata$class))
  expect_equal(nrow(zed2$testdata$preds), length(zed2$testdata$class))
  expect_less_than(nrow(zed2$traindata$preds), nrow(zed2$validdata$preds))
  expect_equal(nrow(zed2$validdata$preds), length(zed2$validdata$class))
  expect_less_than(nrow(zed3$traindata$preds), nrow(zed3$testdata$preds))
  expect_equal(nrow(zed3$traindata$preds), length(zed3$traindata$class))
  expect_equal(nrow(zed3$testdata$preds), length(zed3$testdata$class))
  expect_more_than(ncol(zed1$traindata$preds), ncol(zed3$traindata$preds))
  expect_equal(ncol(zed1$traindata$preds), ncol(zed1$testdata$preds))
  expect_more_than(ncol(zed1$traindata$preds), ncol(zed2$traindata$preds))
  expect_equal(ncol(zed3$traindata$preds), ncol(zed3$testdata$preds))
})


zed1 <- assembleData(full2, class = "Class", p = 0.25, predvars = prednames)
zed1a <- assembleData(full2, class = "Class", p = 0.25, predvars = prednames, 
                      keepNA = TRUE)

zed2 <- assembleData(full2, class = "Class", p = 0.8)
zed2a <- assembleData(full2, class = "Class", p = 0.8, keepNA = TRUE)

zed3 <- assembleData(full2[full2$Factor1 == "e" | 
                            full2$Factor1 == "d",], 
                     class = "Class", p = 0.25, pvalid = 0.1, predvars = prednames)

zed3a <- assembleData(full2[full2$Factor1 == "e" | 
                            full2$Factor1 == "d",], 
                     class = "Class", p = 0.25, pvalid = 0.1, 
                     predvars = prednames, keepNA = TRUE)

test_that("assembleData handles NA correctly", {
  expect_less_than(nrow(zed1$traindata$preds), nrow(zed1$testdata$preds))
  expect_less_than(nrow(zed1$traindata$preds), nrow(zed1a$traindata$preds))
  expect_equal(nrow(zed1$traindata$preds), length(zed1$traindata$class))
  expect_equal(nrow(zed1$testdata$preds), length(zed1$testdata$class))
  expect_less_than(nrow(zed2$testdata$preds), nrow(zed2$traindata$preds))
  expect_less_than(nrow(zed2$traindata$preds), nrow(zed2a$traindata$preds))
  expect_equal(nrow(zed2$traindata$preds), length(zed2$traindata$class))
  expect_equal(nrow(zed2$testdata$preds), length(zed2$testdata$class))
  expect_less_than(nrow(zed3$validdata$preds), nrow(zed3$traindata$preds))
  expect_less_than(nrow(zed3$validdata$preds), nrow(zed3a$validdata$preds))
  expect_equal(nrow(zed3$validdata$preds), length(zed3$validdata$class))
  expect_less_than(nrow(zed3$traindata$preds), nrow(zed3$testdata$preds))
  expect_equal(nrow(zed3$traindata$preds), length(zed3$traindata$class))
  expect_equal(nrow(zed3$testdata$preds), length(zed3$testdata$class))
  expect_equal(ncol(zed1$traindata$preds), ncol(zed1$testdata$preds))
  expect_less_than(ncol(zed2$traindata$preds), ncol(zed1$traindata$preds))
  expect_equal(ncol(zed3$traindata$preds), ncol(zed3$testdata$preds))
  expect_less_than(nrow(zed3$traindata$preds), nrow(zed3a$traindata$preds))
})


context("Test that classification data and regression data works")

full$Class <- as.character(full$Class)

zed1 <- assembleData(full, class = "Class", p = 0.25, predvars = prednames)
zed1a <- assembleData(full, class = "Class", p = 0.25, predvars = prednames)

zed2 <- assembleData(full, class = "Class", p = 0.25, pvalid = 0.25, 
                     predvars = prednames)
zed2a <- assembleData(full, class = "Class", p = 0.25, pvalid = 0.25, 
                      predvars = prednames)



zed3 <- assembleData(full, class = "Class", p = 0.25, predvars = prednames, 
                     classification = FALSE)
zed3a <- assembleData(full, class = "Class", p = 0.25, predvars = prednames, 
                      classification = FALSE)

##
test_that("All class elements are factors", {
  expect_is(zed1$traindata$class, "factor")
  expect_is(zed1a$traindata$class, "factor")
  expect_is(zed2$traindata$class, "factor")
  expect_is(zed2a$traindata$class, "factor")
  expect_is(zed3$traindata$class, "factor")
  expect_is(zed3a$traindata$class, "factor")
  expect_is(zed1$testdata$class, "factor")
  expect_is(zed1a$testdata$class, "factor")
  expect_is(zed2$testdata$class, "factor")
  expect_is(zed2a$testdata$class, "factor")
  expect_is(zed3$testdata$class, "factor")
  expect_is(zed3a$testdata$class, "factor")
  expect_is(zed2$validdata$class, "factor")
  expect_is(zed2a$validdata$class, "factor")
})



##

zed1 <- assembleData(full, class = "Linear1", p = 0.25, predvars = prednames, 
                     classification = FALSE)
zed1a <- assembleData(full, class = "Linear1", p = 0.25, predvars = prednames, 
                      classification = FALSE)

zed2 <- assembleData(full, class = "Linear1", p = 0.25, predvars = prednames, 
                     classification = TRUE)
zed2a <- assembleData(full, class = "Linear1", p = 0.25, predvars = prednames, 
                      classification = TRUE)

zed3 <- assembleData(full, class = "Linear1", p = 0.25, pvalid = 0.2, 
                     predvars = prednames, classification = TRUE)
zed3a <- assembleData(full, class = "Linear1", p = 0.25, 
                      pvalid = 0.2, predvars = prednames, 
                      classification = FALSE)

test_that("All class elements are factors", {
  expect_is(zed1$traindata$class, "numeric")
  expect_is(zed1a$traindata$class, "numeric")
  expect_is(zed2$traindata$class, "factor")
  expect_is(zed2a$traindata$class, "factor")
  expect_is(zed3$traindata$class, "factor")
  expect_is(zed3a$traindata$class, "numeric")
})

# table(split1$test$Class)
# table(split1$train$Class)
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
# 
# findLinearCombos(testData1)
# 
# omitLinearCombos(testData1)
# 
# 
# 
# library(mlbench); library(rpart)
# data(BostonHousing)
# 
# BostonHousing <- rbind(BostonHousing, BostonHousing)
# 
# dat <- assembleData(BostonHousing, class = "medv", p = 0.8)
# 
# context("Test assembling data works in the continuous case")
# 
# # Give tests wide tolerance, looking more for equivalence
# test_that("Data can be assembled correctly", {
#   expect_equal(length(dat), 2)
#   expect_identical(names(dat), c("traindata", "testdata"))
#   expect_equal(round(mean(dat$traindata$class), 1), 
#                round(mean(dat$testdata$class),1), tolerance = 0.5)
#   expect_equal(round(median(dat$traindata$class),1), 
#                round(median(dat$testdata$class), 1), tolerance = 0.5)
#   expect_equal(min(dat$traindata$class), 
#                min(dat$testdata$class), tolerance = 2)
#   expect_equal(max(dat$traindata$class), 
#                max(dat$testdata$class), tolerance = 2)
# })
# 
# context("Test 3 way split")
# dat <- assembleData(BostonHousing, class = "medv", p = 0.8, pvalid = 0.3)
# 
# test_that("Data is split and balanced", {
#   expect_equal(length(dat), 3)
#   expect_identical(names(dat), c("traindata", "testdata", "validdata"))
#   expect_equal(mean(dat$traindata$class), mean(dat$testdata$class), tolerance = 0.5)
#   expect_equal(median(dat$traindata$class), median(dat$testdata$class), tolerance = 0.5)
#   expect_equal(min(dat$traindata$class), min(dat$testdata$class), tolerance = 2)
#   expect_equal(max(dat$traindata$class), max(dat$testdata$class), tolerance = 2)
#   expect_equal(mean(dat$traindata$class), mean(dat$validdata$class), tolerance = 0.5)
#   expect_equal(median(dat$traindata$class), median(dat$validdata$class), tolerance = 0.5)
#   expect_equal(min(dat$traindata$class), min(dat$validdata$class), tolerance = 2)
#   expect_equal(max(dat$traindata$class), max(dat$validdata$class), tolerance = 2)
# })
# 
