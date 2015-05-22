# Test-caretEnsemble
library(caretEnsemble)
data(EWStestData)

test_that("ROCtests work for caretEnsembles", {
  skip_on_cran("Takes too long on remotes")
  ctrl <- trainControl(method = "repeatedcv", 
                       repeats = 3, classProbs = TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)
  out <- caretList(methodList = c("knn", "glm", "nb"), 
                   trControl = ctrl, x = modeldat$traindata$preds, 
                   y = modeldat$traindata$class, tuneLength = 5)
  outEns <- caretEnsemble(out)
  res1 <- ROCtest(outEns)
  res2t <- ROCtest(outEns, testdata = modeldat$testdata)
  outEns$metric <- "ROC"
  MA1 <- modAcc(outEns, datatype = c("test", "train"), testdata = modeldat$testdata)
  testdf1 <- dfExtract(modAcc(outEns, datatype = c("test", "train"), testdata = modeldat$testdata))
  expect_is("res1", "ROCit")
  expect_is("res2t", "ROCit")
  expect_is("MA1", "list")
  expect_is("testdf1", "data.frame")
  expect_is("out", "caretList")
  expect_is("outEns", "caretEnsemble")
})

test_that("ROCtests work for caretStack", {
  skip_on_cran("Takes too long on remotes")
  ctrl <- trainControl(method = "repeatedcv", 
                       repeats = 3, classProbs = TRUE, savePredictions = TRUE,
                       summaryFunction = twoClassSummary)
  out <- caretList(methodList = c("knn", "glm", "nb"), 
                   trControl = ctrl, x = modeldat$traindata$preds, 
                   y = modeldat$traindata$class, tuneLength = 5)
  outStack <- caretStack(out, method = 'glm')
  res1 <- ROCtest(outStack)
  res2t <- ROCtest(outEns, testdata = modeldat$testdata)
  outStack$metric <- "ROC"
  MA1 <- modAcc(outStack, datatype = c("test", "train"), testdata = modeldat$testdata)
  testdf1 <- dfExtract(modAcc(outStack, datatype = c("test", "train"), testdata = modeldat$testdata))
  expect_is("res1", "ROCit")
  expect_is("res2t", "ROCit")
  expect_is("MA1", "list")
  expect_is("testdf1", "data.frame")
  expect_is("out", "caretList")
  expect_is("outEns", "caretEnsemble")
})
