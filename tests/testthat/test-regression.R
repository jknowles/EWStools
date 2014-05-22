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
})

modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
         modelKeep = FALSE, length = 12, fitControl = ctrl, metric = "RMSE")

modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
        modelKeep = FALSE, length = 12, fitControl = ctrl, metric = "ROC")


mod <- modAcc(myFit, datatype = c("train", "test"), testdata = dat$testdata)
mod2 <- modAcc(myFit, datatype = c("train"), testdata = dat$testdata)

out <- buildRMSEFrame(methods = "rpart")


dfExtract(mod)
dfExtract(mod2)

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
# 
# resultSet1b <- modSearch(methods = c("knn", "glm", "rpart", "lm"), 
#                          datatype = c("test"), 
#                          traindata = dat$traindata, 
#                          testdata = dat$testdata, 
#                          modelKeep = FALSE, length = 6, fitControl = ctrl, 
#                          metric = "MAD")

# consider warning to modAcc when datatype is not explicit
# 
# modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
#         modelKeep = FALSE, length = 12, fitControl = ctrl, metric = "RMSE")
# 
# modTest(method = "rpart", datatype = c("train", "test"), traindata = dat$traindata, 
#         testdata = dat$testdata, modelKeep = FALSE, 
#         length = 12, fitControl = ctrl, metric = "RMSE")
# 
