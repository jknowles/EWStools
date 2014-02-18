# Test modsearch

# For train objects
# For glm objects
# others?!?


context("Test summaries of train objects")

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

modobj <- modAcc(fullModel, datatype = c("train", "test"), modelKeep = FALSE, 
                 testdata = list(preds = test[, -19], class = test[, 19]))

modobj2 <- modAcc(fullModel, datatype = c("test"), modelKeep = FALSE, 
                 testdata = list(preds = test[, -19], class = test[, 19]))

modobj3 <- modAcc(fullModel, datatype = c("train"), modelKeep = FALSE)


test_that("modAcc produces named lists", {
  expect_that(modobj, is_a("list"))
  expect_that(modobj2, is_a("list"))
  expect_that(modobj3, is_a("list"))
})

test_that("modAcc list has correct slots", {
  expect_that(modobj$method, is_a("character"))
  expect_that(modobj$time, is_a("numeric"))
  expect_that(modobj$summaryTr, is_a("ROCit"))
  expect_that(modobj$summaryTe, is_a("ROCit"))
})

test_that("modAcc reports NULL for slots without objects", {
  expect_that(modobj2$summaryTr, is_null())
  expect_that(modobj3$summaryTe, is_null())
  
})

test_that("modAcc subobjects have correct slots for train data", {
  expect_that(modobj$summaryTr@thresh, is_a("numeric"))
  expect_that(modobj$summaryTr@auc, is_a("numeric"))
  expect_that(modobj$summaryTr@confusematrix, is_a("data.frame"))
  expect_that(modobj$summaryTr@rarepercent, is_a("numeric"))
  expect_that(modobj$summaryTr@falsepositive, is_a("numeric"))
  expect_that(modobj$summaryTr@rocobj, is_a("roc"))
  expect_that(modobj$summaryTr@modtype, is_a("character"))
  expect_that(modobj$summaryTr@modcall, is_a("character"))
  expect_that(modobj$summaryTr@datatype, is_a("character"))
  expect_identical(modobj$summaryTr@datatype, "train")  
  expect_that(modobj$summaryTe@thresh, is_a("numeric"))
  expect_that(modobj$summaryTe@auc, is_a("numeric"))
  expect_that(modobj$summaryTe@confusematrix, is_a("data.frame"))
  expect_that(modobj$summaryTe@rarepercent, is_a("numeric"))
  expect_that(modobj$summaryTe@falsepositive, is_a("numeric"))
  expect_that(modobj$summaryTe@rocobj, is_a("roc"))
  expect_that(modobj$summaryTe@modtype, is_a("character"))
  expect_that(modobj$summaryTe@modcall, is_a("character"))
  expect_that(modobj$summaryTe@datatype, is_a("character"))
  expect_identical(modobj$summaryTe@datatype, "test")  
})

# test_that("ROCit objects pass best threshold parameters through", {
#   expect_more_than(res3@thresh, res2@thresh)
#   expect_more_than(res3@rarepercent, res2@rarepercent)
#   expect_more_than(res3@falsepositive, res2@falsepositive)
#   expect_equal(res2@auc, res3@auc)
# })

context("Converting modAcc lists to dataframes for plotting")

test_that("A data.frame can be extracted from model accuracy objects...", {
  expect_that(dfExtract(modobj), is_a("data.frame"))
  expect_that(dfExtract(modobj2), is_a("data.frame"))
  expect_that(dfExtract(modobj3), is_a("data.frame"))
})

test_that("dfExtract functions when only test or training data present", {
  expect_that(nrow(dfExtract(modobj)), is_more_than(nrow(dfExtract(modobj2))))
  expect_that(nrow(dfExtract(modobj)), is_more_than(nrow(dfExtract(modobj3))))
  expect_identical(nrow(dfExtract(modobj2)), nrow(dfExtract(modobj3)))
})

# 
# class(dfExtract(modobj))
# 
# 
# dfExtract(modobj)
