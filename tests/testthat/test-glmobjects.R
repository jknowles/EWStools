# Test glms

set.seed(442)
library(caret)
train <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
                    noiseVars = 10, corrVars = 2, corrValue = 0.6)



fullModel <- glm(Class ~ ., data = train, 
                   family = binomial)


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
#   expect_that(ROCtest.glm(fullModel), shows_message())
# })

test_that("ROCit object has correct slots", {
  expect_that(res1@thresh, is_a("numeric"))
  expect_that(res1@auc, is_a("numeric"))
  expect_that(res1@confusematrix, is_a("confusionMatrix"))
  expect_that(res1@rarepercent, is_a("numeric"))
  expect_that(res1@falsepositive, is_a("numeric"))
  expect_that(res1@rocobj, is_a("roc"))
  expect_that(res1@modtype, is_a("character"))
  expect_that(res1@modcall, is_a("character"))
  expect_that(res1@datatype, is_a("character"))
  expect_identical(res1@datatype, "train")  
})

test_that("ROCit objects pass best threshold parameters through", {
  expect_false(isTRUE(all.equal(res2@thresh, res3@thresh, tolerance = 0.01)))
  expect_false(isTRUE(all.equal(res2@rarepercent, res3@rarepercent, tolerance = 0.01)))
  expect_false(isTRUE(all.equal(res2@falsepositive, res3@falsepositive, tolerance = 0.01)))
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

# test_that("ROCtest throws messages", {
#   expect_that(ROCtest.glm(fullModel, 
#                             newdata = list(preds = test[, -19], 
#                                            y = test[, 19])), shows_message())
#   
# })

test_that("ROCtest produces correct objects", {
  expect_that(res1t, is_a("ROCit"))
  expect_that(res2t, is_a("ROCit"))
  expect_that(res3t, is_a("ROCit"))
})


test_that("ROCit object has correct slots", {
  expect_that(res1t@thresh, is_a("numeric"))
  expect_that(res1t@auc, is_a("numeric"))
  expect_that(res1t@confusematrix, is_a("confusionMatrix"))
  expect_that(res1t@rarepercent, is_a("numeric"))
  expect_that(res1t@falsepositive, is_a("numeric"))
  expect_that(res1t@rocobj, is_a("roc"))
  expect_that(res1t@modtype, is_a("character"))
  expect_that(res1t@modcall, is_a("character"))
  expect_that(res1t@datatype, is_a("character"))
  expect_identical(res1t@datatype, "test")
})

test_that("ROCit objects pass best threshold parameters through", {
  expect_false(isTRUE(all.equal(res2t@thresh, res3t@thresh, tolerance = 0.01)))
  expect_more_than(res1t@auc, res1@auc)
  expect_false(isTRUE(all.equal(res2t@rarepercent, res3t@rarepercent, tolerance = 0.01)))
  expect_false(isTRUE(all.equal(res2t@falsepositive, res3t@falsepositive, tolerance = 0.01)))
  expect_equal(res2t@auc, res3t@auc)
})

test_that("ROCtest generic functions correctly", {
  expect_identical(ROCtest(fullModel), EWStools:::ROCtest.glm(fullModel))
#   expect_error(EWStools:::ROCtest.train(fullModel))
  expect_identical(ROCtest(fullModel, 
                           testdata = list(preds = test[, -19], 
                                           class = test[, 19])), 
                   EWStools:::ROCtest.glm(fullModel, testdata = list(preds = test[, -19], 
                                                            class = test[, 19])))
#   expect_error(EWStools:::ROCtest.train(fullModel, testdata = list(preds = test[, -19], 
#                                                       class = test[, 19])))
})

# context("Errors are thrown appropriately")
# test_that("ROCtest throws error when testdata is misspecified", {
#   expect_error(ROCtest(fullModel, 
#                        testdata = test))
#   expect_error(ROCtest(fullModel, 
#                        testdata = list(testdata = test[, -19], testclass = test[, 19])))
#   
# })


context("Test the summary method for the ROCtest")

test_that("ROCtest summaries function correctly", {
  expect_that(summary(res1), is_a("list"))
  expect_that(summary(res1t), is_a("list"))
  
})

