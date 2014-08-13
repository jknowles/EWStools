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

if(missing(testdata)){
  yhats <- probExtract(mod)
  if(is.null(yhats)==TRUE) stop("Cannot generate probabilities")
  mroc <- roc(.outcome ~ yhat, data=yhats, percent=TRUE, algorithm=2)
  a <- mroc$auc[1]
  thresh <- coords.roc(mroc, x="best")[1]
  cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = thresh), 
                        reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
  myROC <- ROCit(thresh=t, auc=a, confusematrix=cm, 
                 rarepercent=cm$byClass["Neg Pred Value"], 
                 falsepositive=1 - cm$byClass["Neg Pred Value"], 
                 rocobj=mroc,
                 modtype = class(mod), 
                 modcall = paste(mod$call), datatype="train")
  return(myROC)
} 


yhats <- probExtract(mod, testdata = testdata)
if(is.null(yhats)==TRUE) stop("Cannot generate probabilities")
mroc <- roc(.outcome ~ yhat, data=yhats, precent=TRUE, algorithm=2)
a <- mroc$auc[1]
thresh <- coords(mroc, x="best")[1]
cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = thresh), 
                      reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
myROC <- ROCit(thresh=thresh, auc=a, confusematrix=cm, 
               rarepercent=cm$byClass["Neg Pred Value"], 
               falsepositive=1 - cm$byClass["Neg Pred Value"], 
               rocobj=mroc,
               modtype = class(mod), 
               modcall = paste(mod$call), datatype="test")
return(myROC)

# check functions with different types of levels

ROCtest(out.ens)@confusematrix

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
