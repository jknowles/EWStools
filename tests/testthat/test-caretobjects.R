# Set up a classification test

set.seed(442)
library(caret)
train <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
                        noiseVars = 10, corrVars = 2, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
                       noiseVars = 10, corrVars = 2, corrValue = 0.6)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, classProbs = TRUE, 
                     summaryFunction = twoClassSummary)


fullModel <- train(Class ~ ., data = train, 
                   method = "svmRadial", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "ROC", 
                   trControl = ctrl)

# Tests
# First correct ROC on the current data
# ROC on current data with custom threshold preserved
# ROC on new data
# ROC on new data with custom threshold
# Inspect all ROC objects

# fullTest <- roc(test$Class, 
#                 predict(fullModel, test, type = "prob")[,1], 
#                 levels = rev(levels(test$Class)))
# fullTest

context("ROCtest works properly with a train object and train data")

res1 <- ROCtest(fullModel)

res2 <- ROCtest(fullModel, best.method="closest.topleft", 
                      best.weights=c(1, .66))

res3 <- ROCtest(fullModel, best.method="closest.topleft", 
                      best.weights=c(100, .66))

test_that("ROCtest produces correct objects", {
  expect_that(res1, is_a("ROCit"))
  expect_that(res2, is_a("ROCit"))
  expect_that(res3, is_a("ROCit"))
})

# test_that("ROCtest throws messages", {
#   expect_that(ROCtest.train(fullModel), shows_message())
# })

test_that("ROCit object has correct slots", {
  expect_that(res1@thresh, is_a("numeric"))
  expect_that(res1@auc, is_a("numeric"))
  expect_that(res1@confusematrix, is_a("data.frame"))
  expect_that(res1@rarepercent, is_a("numeric"))
  expect_that(res1@falsepositive, is_a("numeric"))
  expect_that(res1@rocobj, is_a("roc"))
  expect_that(res1@modtype, is_a("character"))
  expect_that(res1@modcall, is_a("character"))
  expect_that(res1@datatype, is_a("character"))
  expect_identical(res1@datatype, "train")  
})

test_that("ROCit objects pass best threshold parameters through", {
  expect_more_than(res3@thresh, res2@thresh)
  expect_more_than(res3@rarepercent, res2@rarepercent)
  expect_more_than(res3@falsepositive, res2@falsepositive)
  expect_equal(res2@auc, res3@auc)
})

context("ROCtest works correctly with train object and test data")

res1t <- ROCtest(fullModel, 
                       testdata = list(preds = test[, -19], class = test[, 19]))

res2t <- ROCtest(fullModel, best.method="closest.topleft", 
                      best.weights=c(1, .66), 
                      testdata = list(preds = test[, -19], class = test[, 19]))

res3t <- ROCtest(fullModel, best.method="closest.topleft", 
                      best.weights=c(100, .66), 
                      testdata = list(preds = test[, -19], class = test[, 19]))



test_that("ROCtest produces correct objects", {
  expect_that(res1t, is_a("ROCit"))
  expect_that(res2t, is_a("ROCit"))
  expect_that(res3t, is_a("ROCit"))
})


test_that("ROCit object has correct slots", {
  expect_that(res1t@thresh, is_a("numeric"))
  expect_that(res1t@auc, is_a("numeric"))
  expect_that(res1t@confusematrix, is_a("data.frame"))
  expect_that(res1t@rarepercent, is_a("numeric"))
  expect_that(res1t@falsepositive, is_a("numeric"))
  expect_that(res1t@rocobj, is_a("roc"))
  expect_that(res1t@modtype, is_a("character"))
  expect_that(res1t@modcall, is_a("character"))
  expect_that(res1t@datatype, is_a("character"))
  expect_identical(res1t@datatype, "test")
})

test_that("ROCit objects pass best threshold parameters through", {
  expect_more_than(res3t@thresh, res2t@thresh)
  expect_more_than(res3t@rarepercent, res2t@rarepercent)
  expect_more_than(res3t@falsepositive, res2t@falsepositive)
  expect_equal(res2t@auc, res3t@auc)
})

test_that("ROCtest generic functions correctly", {
  expect_identical(ROCtest(fullModel), EWStools:::ROCtest.train(fullModel))
  expect_error(EWStools:::ROCtest.glm(fullModel))
  expect_identical(ROCtest(fullModel, 
                           testdata = list(preds = test[, -19], 
                                           class = test[, 19])), 
                   EWStools:::ROCtest.train(fullModel, testdata = list(preds = test[, -19], 
                                                            class = test[, 19])))
  expect_error(EWStools:::ROCtest.glm(fullModel, testdata = list(preds = test[, -19], 
                                                      class = test[, 19])))
})

context("Test the summary method for the ROCtest")

test_that("ROCtest summaries function correctly", {
  expect_that(summary(res1), is_a("list"))
  expect_that(summary(res1t), is_a("list"))
  
})

context("Errors are thrown appropriately")
test_that("ROCtest throws error when testdata is misspecified", {
  expect_error(ROCtest(fullModel, 
                       testdata = test))
  expect_error(ROCtest(fullModel, 
                       testdata = list(testdata = test[, -19], testclass = test[, 19])))
  
})


# Test if confusion matrix function is accurate

# confuse_mat.train(fullModel, thresh=0.99, prop=TRUE)
# confuse_mat.train(fullModel, thresh=0.99, prop=FALSE)
# confuse_mat.train(fullModel, thresh=0.01, prop=FALSE)
# confuse_mat.train(fullModel, thresh=0.01, prop=FALSE)
# 
# # Test if confusion matrix produces predictions on new data well
# 
# confuse_mat.train(fullModel, thresh = 0.99, prop=FALSE, 
#                   newdata = list(preds = test[, -19], y = test[, 19]))
# 
# confuse_mat.train(fullModel, thresh = 0.5, prop=TRUE, 
#                   newdata = list(preds = test[, -19], y = test[, 19]))
# 
# 
# # Create a plot function
# 
# roc_plot.train(fullModel, s = 20, 
#                newdata = list(preds = test[, -19], y = test[, 19]))
# 
# 
# roc_plot.train(fullModel, 100, newdata = train)
