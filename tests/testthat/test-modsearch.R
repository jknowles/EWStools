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

modobjE <- modAcc(fullModel, datatype = c("train", "test"), modelKeep = FALSE, 
                 testdata = list(preds = test[, -19], class = test[, 19]), 
                 best.method = "closest.topleft", best.weights = c(10, 0.11))

test_that("modAcc passes ellipsis through correct", {
  expect_that(modobjE, is_a("list"))
})

modobj <- modAcc(fullModel, datatype = c("train", "test"), modelKeep = TRUE, 
                 testdata = list(preds = test[, -19], class = test[, 19]))

modobj2 <- modAcc(fullModel, datatype = c("test"), modelKeep = TRUE, 
                  testdata = list(preds = test[, -19], class = test[, 19]))

modobj3 <- modAcc(fullModel, datatype = c("train"), modelKeep = TRUE)

test_that("modAcc keeps the model correctly", {
  expect_that(modobj$model, is_a("train"))
  expect_that(modobj2$model, is_a("train"))
  expect_that(modobj3$model, is_a("train"))
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

context("Evaluate modSearch function ")
 

test1 <- modTest(method = "svmRadial", datatype = c("train", "test"), 
                   traindata = list(preds = train[, -19], class = train[, 19]), 
                   testdata = list(preds = test[, -19], class = test[, 19]), 
                   modelKeep = FALSE, length = 6, fitControl = ctrl, 
                   metric = "ROC")

test1a <- modTest(method = "knn", datatype =  "test", 
                 traindata = list(preds = train[, -19], class = train[, 19]), 
                 testdata = list(preds = test[, -19], class = test[, 19]), 
                 modelKeep = FALSE, length = 6, fitControl = ctrl, 
                 metric = "ROC")
test1b <- modTest(method = "knn", datatype =  "train", 
                  traindata = list(preds = train[, -19], class = train[, 19]), 
                  testdata = list(preds = test[, -19], class = test[, 19]), 
                  modelKeep = FALSE, length = 6, fitControl = ctrl, 
                  metric = "ROC")

test2 <- modTest(method = "svmRadial", datatype = c("train", "test"), 
               traindata = list(preds = train[, -19], class = train[, 19]), 
               testdata = list(preds = test[, -19], class = test[, 19]), 
               modelKeep = TRUE, length = 6, fitControl = ctrl, 
               metric = "ROC")
 
test_that("modTest returns the right objects", {
  expect_that(test1, is_a("list"))
  expect_that(test2, is_a("list"))
  expect_that(test2$model, is_a("train"))
  expect_that(test1a$summaryTr, is_null())
  expect_that(test1a$summaryTe, is_a("ROCit"))
  expect_that(test1b$summaryTe, is_null())
  expect_that(test1b$summaryTr, is_a("ROCit"))
  
})



context("Evaluate modSearch function ")

resultSet <- modSearch(methods = c("knn", "glm", "svmRadial"), 
                            timeout = 10,
                            datatype = c("train", "test"), 
                            traindata = list(preds = train[, -19], class = train[, 19]), 
                            testdata = list(preds = test[, -19], class = test[, 19]), 
                            modelKeep = FALSE, length = 6, fitControl = ctrl, 
                            metric = "ROC")

resultSet2 <- modSearch(methods = c("knn", "glm", "lda2"), 
                       datatype = c("train", "test"), 
                       traindata = list(preds = train[, -19], class = train[, 19]), 
                       testdata = list(preds = test[, -19], class = test[, 19]), 
                       modelKeep = FALSE, length = 6, fitControl = ctrl, 
                       metric = "ROC", omit = 4)

test_that("modSearch returns the right objects", {
  expect_that(resultSet, is_a("data.frame"))
  expect_that(resultSet, is_a("data.frame"))
  expect_error(modSearch(methods = c("knn", "glm", "lda2"), 
                         datatype = c("train", "test"), 
                         traindata = list(preds = train[, -19], class = train[, 19]), 
                         testdata = list(preds = test[, -19], class = test[, 19]), 
                         modelKeep = FALSE, length = 6, fitControl = ctrl, 
                         metric = "ROC", omit = NULL))
})

context("Test modSearch in parallel on Windows")

library(doParallel)
CORES <- 4


resultSet2 <- modSearch(methods = c("knn", "glm", "lda2"), 
                        datatype = c("train", "test"), 
                        traindata = list(preds = train[, -19], class = train[, 19]), 
                        testdata = list(preds = test[, -19], class = test[, 19]), 
                        modelKeep = FALSE, length = 12, fitControl = ctrl, 
                        metric = "ROC", omit = 4, cores = CORES)

