

context("Extract accuracy from a regular caret object")
set.seed(442)
library(caret)
train <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
                    noiseVars = 10, corrVars = 2, corrValue = 0.6)


ctrl <- trainControl(method = "cv", 
                     number = 3, classProbs = TRUE, 
                     summaryFunction = fourStatsSummary)

myFit <- train(Class ~ ., data = train, 
                   method = "knn", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "Dist", maximize = FALSE,
                   trControl = ctrl)


dat <- assembleData(rbind(train,test), class = "Class", p = 0.8)


test_that("DIStest.train works as expected", {
  expect_is(EWStools:::DIStest.train(myFit), "DISit")
  expect_identical(EWStools:::DIStest.train(myFit), DIStest(myFit))
  expect_is(DIStest(myFit, testdata = dat$testdata), "DISit")
})


context("Testing modAcc")

test_that("modAcc accepts datatype values", {
  expect_null(modAcc(myFit)$summaryTe)
  expect_is(modAcc(myFit, datatype = c("train", "test"), 
                   testdata = dat$testdata)$summaryTe, "DISit")
  expect_message(modAcc(myFit))
  expect_error(modAcc(myFit, datatype = "test"))
})

context("Testing modTest")

test_that("modTest throws errors with RMSE metric misspecified", {
  expect_error(modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
                       modelKeep = FALSE, length = 12, fitControl = ctrl, metric = "RMSE"))
  expect_is(modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
                    modelKeep = FALSE, length = 12, fitControl = ctrl, 
                    metric = "Dist", maximize = FALSE), 
            "list")
})


test_that("modTest accepts maximize as an argument", {
  expect_is(modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
                    modelKeep = FALSE, length = 12, fitControl = ctrl, 
                    metric = "Dist", maximize = FALSE), 
            "list")
  expect_is(modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
                    modelKeep = FALSE, length = 12, fitControl = ctrl, 
                    metric = "Dist", maximize = TRUE), 
            "list")
})

# buildFrame test

context("Test that buildDISframe works")

test_that("corect frame is built", {
  expect_equal(nrow(EWStools:::buildDISFrame(methods = "rpart")), 1)
  expect_equal(nrow(EWStools:::buildDISFrame(methods = c("rpart", "knn", "glm"))), 3)
  expect_equal(length(EWStools:::buildDISFrame(methods = "car")), 7)
})

# dfExtract functions

context("Test that DIS frames are extracted from DIS modAccs")


mod1 <- modAcc(myFit, datatype = c("train", "test"), testdata = dat$testdata)
mod2 <- modAcc(myFit, datatype = c("train"), testdata = dat$testdata)
mod3 <- modAcc(myFit, datatype = c("test"), testdata = dat$testdata)

test_that("Correct variables and dimensions present", {
  expect_identical(dfExtract(mod2)$grp, "train")
  expect_identical(dfExtract(mod3)$grp, "test")
  expect_identical(dfExtract(mod1)$grp, c("train", "test"))
  expect_equal(nrow(dfExtract(mod1)), 2)
  expect_equal(nrow(dfExtract(mod2)), 1)
  expect_equal(nrow(dfExtract(mod3)), 1)
  expect_equal(length(dfExtract(mod1)), 7)
  expect_equal(length(dfExtract(mod2)), 7)
  expect_equal(length(dfExtract(mod3)), 7)
})

test_that("Train and test metrics not identical", {
  expect_false(dfExtract(mod1)[1, 1] == dfExtract(mod1)[2, 1])
  expect_true(dfExtract(mod1)[1, 2] == dfExtract(mod1)[2, 2])
  expect_true(dfExtract(mod1)[1, 3] != dfExtract(mod1)[2, 3])
  expect_true(dfExtract(mod1)[1, 4] != dfExtract(mod1)[2, 4])
  expect_true(dfExtract(mod1)[1, 5] == dfExtract(mod1)[2, 5])
  expect_true(dfExtract(mod1)[1, 6] != dfExtract(mod1)[2, 6])
  expect_true(dfExtract(mod1)[1, 7] == dfExtract(mod1)[2, 7])
})

# modSearch passes
context("Test that modSearch functions for metric Dist")

ctrl <- trainControl(method = "cv", repeats = 5)

resultSet1 <- modSearch(methods = c("knn", "glm", "rpart", "ctree"), 
                        datatype = c("train", "test"), 
                        traindata = dat$traindata, 
                        testdata = dat$testdata, 
                        length = 6, fitControl = ctrl, 
                        metric = "Dist", maximize = FALSE)


resultSet1a <- modSearch(methods = c("knn", "glm", "rpart", "ctree"), 
                         datatype = c("train"), 
                         traindata = dat$traindata, 
                         testdata = dat$testdata, 
                         length = 6, fitControl = ctrl, 
                         metric = "Dist", maximize = FALSE)


resultSet1b <- modSearch(methods = c("knn", "glm", "rpart", "ctree"), 
                         datatype = c("test"), 
                         traindata = dat$traindata, 
                         testdata = dat$testdata, 
                         length = 6, fitControl = ctrl, 
                         metric = "Dist", maximize = FALSE)

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
  expect_error(modSearch(methods = c("knn", "glm", "rpart", "lm"), 
                           datatype = c("test"), 
                           traindata = dat$traindata, 
                           testdata = dat$testdata, 
                           modelKeep = FALSE, length = 6, fitControl = ctrl, 
                           metric = "ROC"))
})


# set.seed(442)
# library(caret)
# train <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
#                      noiseVars = 10, corrVars = 2, corrValue = 0.6)
# test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
#                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
# 
# 
# ctrl <- trainControl(method = "cv", 
#                      number = 3, classProbs = TRUE, 
#                      summaryFunction = fourStatsSummary)
# 
# fullModel <- train(Class ~ ., data = train, 
#                    method = "knn", 
#                    preProc = c("center", "scale"), 
#                    tuneLength = 8, 
#                    metric = "Dist", maximize = FALSE,
#                    trControl = ctrl)
# 
# 
# 
# DIStest(fullModel)@confusematrix
# dist(DIStest(fullModel)@coords)
# 
# fullModel <- train(x = train[, 1:18], y = train[, 19], 
#                    method = "lda2", 
#                    # preProc = c("center", "scale"), 
#                    tuneLength = 8, 
#                    metric = "Dist", maximize = FALSE,
#                    trControl = ctrl)
# 
# DIStest(fullModel, testdata = list(preds = test[, 1:18], class = test[, 19]))
# 
# dfExtract(modAcc(fullModel, datatype = c("test", "train"), 
#                  testdata = list(preds = test[, 1:18], class = test[, 19])))
# 
# EWStools:::probExtract(fullModel, testdata = list(preds = test[, 1:18], class = test[, 19]))
# yhats <- predict(fullModel, newdata = test, type = "prob")
# 
# fullModel <- train(Class ~ ., data = train, 
#                    method = glmDist, 
#                    preProc = c("center", "scale"), 
#                    tuneLength = 8, 
#                    metric = "Dist", maximize = FALSE,
#                    trControl = ctrl)
# 
# 
# 
# 
# 
# 
# dfExtract(test1)
# 
# resultSet <- modSearch(methods = c("knn", "glm", "svmRadial"), 
#                        datatype = c("train", "test"), 
#                        traindata = list(preds = train[, -19], class = train[, 19]), 
#                        testdata = list(preds = test[, -19], class = test[, 19]), 
#                        modelKeep = FALSE, length = 6, fitControl = ctrl, 
#                        metric = "Dist")
# 
# 
