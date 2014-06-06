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

out.ens <- caretEnsemble(out)


ROCtest(out.ens)

confusionMatrix(reclassProb(yhats = yhats, thresh =0.1), 
                reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])

mroc <- roc(.outcome ~ yhat, data=yhats, precent=TRUE, algorithm=3)
a <- mroc$auc[1]
t <- coords.roc(mroc, x="best")[1]

confusionMatrix(reclassProb(yhats = yhats, thresh =t), 
                reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])


confusionMatrix(reclassProb(yhats = yhats, thresh =t), 
                reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])


cm <- confuse_mat(test, t)
