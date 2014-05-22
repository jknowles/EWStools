# Tests for EWStools for regression
# Build data

library(mlbench)
data(BostonHousing)

BostonHousing <- rbind(BostonHousing, BostonHousing)

#
dat <- assembleData(BostonHousing, class = "medv", p = 0.8)

context("Test assembling data works in the continuous case")

# Give tests wide tolerance, looking more for equivalence
test_that("Data can be assembled correctly", {
  expect_equal(length(dat), 2)
  expect_identical(names(dat), c("traindata", "testdata"))
  expect_equal(round(mean(dat$traindata$class), 1), 
               round(mean(dat$testdata$class),1), tolerance = 0.5)
  expect_equal(round(median(dat$traindata$class),1), 
               round(median(dat$testdata$class), 1), tolerance = 0.5)
  expect_equal(min(dat$traindata$class), 
               min(dat$testdata$class), tolerance = 2)
  expect_equal(max(dat$traindata$class), 
               max(dat$testdata$class), tolerance = 2)
})

context("Test 3 way split")
dat <- assembleData(BostonHousing, class = "medv", p = 0.8, pvalid = 0.3)

test_that("Data is split and balanced", {
  expect_equal(length(dat), 3)
  expect_identical(names(dat), c("traindata", "testdata", "validdata"))
  expect_equal(mean(dat$traindata$class), mean(dat$testdata$class), tolerance = 0.5)
  expect_equal(median(dat$traindata$class), median(dat$testdata$class), tolerance = 0.5)
  expect_equal(min(dat$traindata$class), min(dat$testdata$class), tolerance = 2)
  expect_equal(max(dat$traindata$class), max(dat$testdata$class), tolerance = 2)
  expect_equal(mean(dat$traindata$class), mean(dat$validdata$class), tolerance = 0.5)
  expect_equal(median(dat$traindata$class), median(dat$validdata$class), tolerance = 0.5)
  expect_equal(min(dat$traindata$class), min(dat$validdata$class), tolerance = 2)
  expect_equal(max(dat$traindata$class), max(dat$validdata$class), tolerance = 2)
})


dat <- assembleData(BostonHousing, class = "medv", p = 0.7)

context("Extract accuracy from a regular caret object")

ctrl <- trainControl(method = "cv", repeats = 5)
myFit <- train(x = dat$traindata$preds, y = dat$traindata$class,
                  "rpart",
                  tuneLength = 9, trControl = ctrl)


test_that("RMSEtest.train works as expected", {
  expect_error(RMSEtest.train(myFit))
  expect_is(EWStools:::RMSEtest.train(myFit), "RMSEit")
  expect_identical(EWStools:::RMSEtest.train(myFit), RMSEtest(myFit))
})

test_that("RMSEtest accepts testdata", {
  expect_is(RMSEtest(myFit, testdata = dat$testdata), "RMSEit")
  
})


context("Testing modAcc")

test_that("modAcc accepts datatype values", {
  expect_null(modAcc(myFit)$summaryTe)
  expect_is(modAcc(myFit, datatype = c("train", "test"), 
                   testdata = dat$testdata)$summaryTe, "RMSEit")
  expect_message(modAcc(myFit))
  expect_error(modAcc(myFit, datatype = "test"))
})

context("Testing modTest")

test_that("modTest throws errors with ROC metric misspecified", {
  expect_error(modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
                       modelKeep = FALSE, length = 12, fitControl = ctrl, metric = "ROC"))
  expect_is(modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
                    modelKeep = FALSE, length = 12, fitControl = ctrl, metric = "RMSE"), 
            "list")
})


mod <- modAcc(myFit, datatype = c("train", "test"), testdata = dat$testdata)
mod2 <- modAcc(myFit, datatype = c("train"), testdata = dat$testdata)
mod3 <- modAcc(myFit, datatype = c("test"), testdata = dat$testdata)

out <- buildRMSEFrame(methods = "rpart")


# buildFrame test

context("Test that buildRMSEframe works")

test_that("corect frame is built", {
  expect_equal(nrow(buildRMSEFrame(methods = "rpart")), 1)
  expect_equal(nrow(buildRMSEFrame(methods = c("rpart", "knn", "glm"))), 3)
  expect_equal(length(buildRMSEFrame(methods = "car")), 7)
})

# dfExtract functions

context("Test that RMSE frames are extracted from RMSE modAccs")

test_that("Correct variables and dimensions present", {
  expect_identical(dfExtract(mod2)$grp, "train")
  expect_identical(dfExtract(mod3)$grp, "test")
  expect_identical(dfExtract(mod)$grp, c("train", "test"))
  expect_equal(nrow(dfExtract(mod)), 2)
  expect_equal(nrow(dfExtract(mod2)), 1)
  expect_equal(nrow(dfExtract(mod3)), 1)
  expect_equal(length(dfExtract(mod)), 7)
  expect_equal(length(dfExtract(mod2)), 7)
  expect_equal(length(dfExtract(mod3)), 7)
})

test_that("Train and test metrics not identical", {
  expect_false(dfExtract(mod)[1, 1] == dfExtract(mod)[2, 1])
  expect_true(dfExtract(mod)[1, 2] == dfExtract(mod)[2, 2])
  expect_true(dfExtract(mod)[1, 3] == dfExtract(mod)[2, 3])
  expect_true(dfExtract(mod)[1, 4] == dfExtract(mod)[2, 4])
  expect_true(dfExtract(mod)[1, 5] == dfExtract(mod)[2, 5])
  expect_true(dfExtract(mod)[1, 6] != dfExtract(mod)[2, 6])
  expect_true(dfExtract(mod)[1, 7] == dfExtract(mod)[2, 7])
})

# modSearch passes
context("Test that modSearch functions for metric RMSE")

resultSet1 <- modSearch(methods = c("knn", "glm", "rpart", "lm"), 
                        datatype = c("train", "test"), 
                        traindata = dat$traindata, 
                        testdata = dat$testdata, 
                        modelKeep = FALSE, length = 6, fitControl = ctrl, 
                        metric = "RMSE")


resultSet1a <- modSearch(methods = c("knn", "glm", "rpart", "lm"), 
                       datatype = c("train"), 
                       traindata = dat$traindata, 
                       testdata = dat$testdata, 
                       modelKeep = FALSE, length = 6, fitControl = ctrl, 
                       metric = "RMSE")


resultSet1b <- modSearch(methods = c("knn", "glm", "rpart", "lm"), 
                       datatype = c("test"), 
                       traindata = dat$traindata, 
                       testdata = dat$testdata, 
                       modelKeep = FALSE, length = 6, fitControl = ctrl, 
                       metric = "RMSE")

test_that("Results are correctly formatted", {
  expect_equal(nrow(resultSet1), 8)
  expect_equal(nrow(resultSet1a), 4)
  expect_equal(nrow(resultSet1b), 4)
  expect_equal(length(resultSet1b), 7)
  expect_false(identical(resultSet1a, resultSet1b))
  expect_false(identical(resultSet1, resultSet1b))
  expect_false(identical(resultSet1, resultSet1a))
  expect_false(identical(resultSet1[1, ], resultSet1[5, ]))
  expect_false(identical(resultSet1[2, ], resultSet1[6, ]))
  expect_false(identical(resultSet1[3, ], resultSet1[7, ]))
  expect_false(identical(resultSet1[4, ], resultSet1[8, ]))
})


test_that("Errors are thrown when objects are misspecified", {
  expect_error(modSearch(methods = c("knn", "glm", "rpart", "lm"), 
                          datatype = c("test"), 
                          traindata = dat$traindata, 
                          testdata = dat$testdata, 
                          modelKeep = FALSE, length = 6, fitControl = ctrl, 
                           metric = "MAD"))
  expect_warning(modSearch(methods = c("knn", "glm", "rpart", "lm"), 
                         datatype = c("test"), 
                         traindata = dat$traindata, 
                         testdata = dat$testdata, 
                         modelKeep = FALSE, length = 6, fitControl = ctrl, 
                         metric = "ROC"))
})


 
# modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
#         modelKeep = FALSE, length = 12, fitControl = ctrl, metric = "RMSE")
# 
# modTest(method = "rpart", datatype = c("train", "test"), traindata = dat$traindata, 
#         testdata = dat$testdata, modelKeep = FALSE, 
#         length = 12, fitControl = ctrl, metric = "RMSE")
# 
