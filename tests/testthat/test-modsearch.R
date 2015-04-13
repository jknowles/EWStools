# Test modsearch
context("Test summaries of train objects")

rm(list = ls())
set.seed(442)
library(caret)
trainT <- twoClassSim(n = 500, intercept = -8, linearVars = 1, 
                     noiseVars = 10, corrVars = 2, corrValue = 0.6)
test <- twoClassSim(n = 1000, intercept = -7, linearVars = 1, 
                    noiseVars = 10, corrVars = 2, corrValue = 0.6)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, classProbs = TRUE, 
                     summaryFunction = twoClassSummary)


fullModel <- train(Class ~ ., data = trainT, 
                   method = "lda2", 
                   preProc = c("center", "scale"), 
                   tuneLength = 3, 
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
  # Threshold not the same as others
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
  expect_that(modobj$summaryTr@confusematrix, is_a("confusionMatrix"))
  expect_that(modobj$summaryTr@rarepercent, is_a("numeric"))
  expect_that(modobj$summaryTr@falsepositive, is_a("numeric"))
  expect_that(modobj$summaryTr@rocobj, is_a("roc"))
  expect_that(modobj$summaryTr@modtype, is_a("character"))
  expect_that(modobj$summaryTr@modcall, is_a("character"))
  expect_that(modobj$summaryTr@datatype, is_a("character"))
  expect_identical(modobj$summaryTr@datatype, "train")  
  expect_that(modobj$summaryTe@thresh, is_a("numeric"))
  expect_that(modobj$summaryTe@auc, is_a("numeric"))
  expect_that(modobj$summaryTe@confusematrix, is_a("confusionMatrix"))
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
  expect_false(identical(nrow(dfExtract(modobj2)), nrow(dfExtract(modobj3))))
})

context("Evaluate modTest function ")
 

test1 <- modTest(method = "lda2", datatype = c("train", "test"), 
                   traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                   testdata = list(preds = test[, -19], class = test[, 19]), 
                   modelKeep = FALSE, length = 3, fitControl = ctrl, 
                   metric = "ROC")

test1a <- modTest(method = "knn", datatype =  "test", 
                 traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                 testdata = list(preds = test[, -19], class = test[, 19]), 
                 modelKeep = FALSE, length = 6, fitControl = ctrl, 
                 metric = "ROC")

test1b <- modTest(method = "knn", datatype =  "train", 
                  traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                  testdata = list(preds = test[, -19], class = test[, 19]), 
                  modelKeep = FALSE, length = 6, fitControl = ctrl, 
                  metric = "ROC")

test2 <- modTest(method = "lda2", datatype = c("train", "test"), 
               traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
               testdata = list(preds = test[, -19], class = test[, 19]), 
               modelKeep = TRUE, length = 3, fitControl = ctrl, 
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

resultSet <- modSearch(methods = c("knn", "glm", "lda2"), 
                            datatype = c("train", "test"), 
                            traindata = list(preds = trainT[1:500, -19], class = trainT[1:500, 19]), 
                            testdata = list(preds = test[1:200, -19], class = test[1:200, 19]), 
                            modelKeep = FALSE, length = 6, fitControl = ctrl, 
                            metric = "ROC")

resultSet2 <- modSearch(methods = c("knn", "glm", "lda2"), 
                       datatype = c("train", "test"), 
                       traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                       testdata = list(preds = test[, -19], class = test[, 19]), 
                       modelKeep = FALSE, length = 6, fitControl = ctrl, 
                       metric = "ROC")


resultSet2a <- modSearch(methods = c("knn", "glm", "lda2"), 
                        datatype = c("train"), 
                        traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                        testdata = list(preds = test[, -19], class = test[, 19]), 
                        modelKeep = FALSE, length = 6, fitControl = ctrl, 
                        metric = "ROC")

resultSet2b <- modSearch(methods = c("knn", "glm", "lda2"), 
                         datatype = c("test"), 
                         traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                         testdata = list(preds = test[, -19], class = test[, 19]), 
                         modelKeep = FALSE, length = 6, fitControl = ctrl, 
                         metric = "ROC")


test_that("modSearch returns the right objects", {
  expect_that(resultSet, is_a("data.frame"))
  expect_that(resultSet2, is_a("data.frame"))
  expect_that(resultSet2a, is_a("data.frame"))
  expect_that(resultSet2b, is_a("data.frame"))
  expect_more_than(nrow(resultSet2), nrow(resultSet2a))
  expect_false(identical(nrow(resultSet2b), nrow(resultSet2a)))
  expect_false(identical(nrow(resultSet), nrow(resultSet2)))
  expect_equal(ncol(resultSet), ncol(resultSet2))
  expect_equal(ncol(resultSet), ncol(resultSet2a))
  expect_equal(ncol(resultSet), ncol(resultSet2b))
  expect_true(nrow(resultSet) > 5)
  expect_true(nrow(resultSet2a) > 5)
  expect_true(nrow(resultSet2) > 5)
  expect_true(nrow(resultSet2b) > 5)
})

context("Check functioning of non standard evaluation")
mymet <- "ROC"
mylen <- 3


resultSetNSE1 <- modSearch(methods = c("knn", "glm", "lda2"), 
                           datatype = c("train", "test"), 
                           traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                           testdata = list(preds = test[, -19], class = test[, 19]), 
                           modelKeep = FALSE, length = mylen, fitControl = ctrl, 
                           metric = mymet)

mydt <- "train"
mypar <- list(metric = mymet, length = mylen, datatype = mydt)

resultSetNSE2 <- modSearch(methods = c("knn", "glm", "lda2"), 
                           datatype = mypar$datatype, 
                           traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                           testdata = list(preds = test[, -19], class = test[, 19]), 
                           modelKeep = FALSE, length = mypar$length, fitControl = ctrl, 
                           metric = mypar$metric)
# 
# anon <- function(){
#   modSearch(methods = c("knn", "glm", "svmRadial"), 
#             datatype = mypar$datatype, 
#             traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
#             testdata = list(preds = test[, -19], class = test[, 19]), 
#             modelKeep = FALSE, length = mypar$length, fitControl = ctrl, 
#             metric = mypar$metric)
# }
# 


test_that("modSearch returns the right objects", {
  expect_that(resultSetNSE1, is_a("data.frame"))
  expect_true(nrow(resultSetNSE1) > 2000)
  expect_equal(ncol(resultSetNSE1), ncol(resultSet))
})

mymet <- "Dist"
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, classProbs = TRUE, 
                     summaryFunction = fourStatsSummary)


resultSetNSE3 <- modSearch(methods = c("knn", "glm", "lda2"), 
                           datatype = c("train", "test"), 
                           traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                           testdata = list(preds = test[, -19], class = test[, 19]), 
                           modelKeep = FALSE, length = mylen, fitControl = ctrl, 
                           metric = mymet)


test_that("modSearch returns the right objects", {
  expect_that(resultSetNSE3, is_a("data.frame"))
  expect_true(nrow(resultSetNSE3) < 20)
  expect_equal(ncol(resultSetNSE3), ncol(resultSet))
})

# 
# context("Test modTest returns reliable results")
# 
# test_that("modSearch results are reliable for train and test", {
#   expect_true(identical(resultSet[resultSet$grp == "train", 5], resultSet[resultSet$grp == "test", 5]))
#   expect_true(identical(resultSet[resultSet$grp == "train", 4], resultSet[resultSet$grp == "test", 4]))
#   expect_false(identical(resultSet[resultSet$grp == "train", 3], resultSet[resultSet$grp == "test", 3]))
#   expect_false(identical(resultSet[resultSet$grp == "train", 2], resultSet[resultSet$grp == "test", 2]))
#   expect_true(identical(resultSet[resultSet$grp == "train", 1], resultSet[resultSet$grp == "test", 1]))
# })
# # 