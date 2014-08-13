# Test-caretEnsemble

#install_github("jknowles/caretEnsemble")
library(caretEnsemble)
library(EWStools)
data(EWStestData)


ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, classProbs = TRUE, savePredictions = TRUE,
                     summaryFunction = twoClassSummary)


out <- buildModels(methodList = c("knn", "glm", "nb", "lda", "ctree"), 
                   control = ctrl, x = modeldat$traindata$preds, 
                   y = modeldat$traindata$class, tuneLength = 5)

# ensemble we will

outEns <- caretEnsemble(out)

res1 <- ROCtest(outEns)

res2t <- ROCtest(outEns, testdata = modeldat$testdata)


outEns$metric <- "ROC"

MA1 <- modAcc(outEns, datatype = c("test", "train"), testdata = modeldat$testdata)
testdf1 <- dfExtract(modAcc(outEns, datatype = c("test", "train"), testdata = modeldat$testdata))


