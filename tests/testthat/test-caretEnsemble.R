# Test-caretEnsemble

#install_github("jknowles/caretEnsemble")
library(caretEnsemble)
data(EWStestData)


ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3, classProbs = TRUE, savePredictions = TRUE,
                     summaryFunction = twoClassSummary)


out <- buildModels(methodList = c("knn", "glm", "nb", "lda", "ctree"), 
                   control = ctrl, x = modeldat$traindata$preds, 
                   y = modeldat$traindata$class, tuneLength = 5)

# ensemble we will

out.ens <- caretEnsemble(out)

test <- predict(out.ens, keepNA = TRUE)
test <- cbind(test, 1-test, out.ens$models$knn$trainingData$.outcome)
test <- as.data.frame(test)
names(test) <- c("common", "rare", "obs")

mroc <- roc(obs ~ common, data=test, precent=TRUE, algorithm=3)
a <- mroc$auc[1]
t <- coords.roc(mroc, x="best", method = "closest.topleft", best.weights = c(.5, .2))[1]
cm <- confuse_mat(test, t)

test$yn <- as.character(test$obs)
mode <- statmod(test$yn)
test$yn[test$yn == mode] <- "1"
test$yn[test$yn != "1"] <- 0
test$yn <- as.character(test$yn)
test$y <- as.numeric(test$yn)
tp <- length(x$y[x$y > 0 & x[, 1] >= thresh])
fp <- length(x$y[x$y < 1 & x[, 1] >= thresh])
fn <- length(x$y[x$y > 0 & x[, 1] <= thresh])
tn <- length(x$y[x$y < 1 & x[, 1] <= thresh])
mat <- data.frame(`Predicted NC` = c(tn, fn), `Predicted C` = c(fp, 
                                                                tp))
row.names(mat) <- c("Actual NC", "Actual C")
return(mat)

ROCtest.caretEnsemble <- function(mod, testdata, ...){
  if(missing(testdata)){
      test <- predict(mod, type="prob")
      test <- cbind(test, 1-test, mod$models[[1]]$trainingData$.outcome) # hack for outcome data now
      test <- as.data.frame(test)
      names(test) <- c("common", "rare","obs")
    if(is.null(test)==TRUE) stop("Cannot generate probabilities")
    #message("Generating ROC...")
    mroc <- roc(obs ~ common, data=test, precent=TRUE, algorithm=3)
    a <- mroc$auc[1]
    t <- coords.roc(mroc, x="best", ...)[1]
    cm <- confuse_mat.train(test, t)
    rc <- cm[1,1] / (cm[1,1] + cm[1,2])
    fp <- cm[2,1] / (cm[1,1] + cm[2,1])
    myROC <- ROCit(thresh=t, auc=a, confusematrix=cm, 
                   rarepercent=rc, falsepositive=fp, rocobj=mroc,
                   modtype = class(mod), 
                   modcall = paste(mod$call), datatype="train")
    return(myROC)
  }
  else if(!missing(testdata)){
    # error handling
    if(class(testdata) != "list"){
      stop("Please provide testdata as a named list with elements 'preds' and 'class'")
    }
    if("preds" %in% names(testdata)){
      
    } else {
      stop("Please provide testdata as a named list with elements 'preds' and 'class'")
    }
    # end error handling
    if(is.null(mod$terms)==TRUE){
      test <- extractProb(list(mod), testX = testdata$preds, testY=testdata$class)
      test <- subset(test, dataType == "Test")
      names(test)[1:3] <- c("common", "rare", "obs")
    } else if(is.null(mod$terms)==FALSE){
      test <- predict(mod, newdata=cbind(testdata$class, testdata$preds), 
                      type="prob")
      test <- cbind(test, testdata$class)
      names(test) <- c("common", "rare", "obs")
    }
    if(is.null(test)==TRUE) stop("Cannot generate probabilities")
    #message("Generating ROC...")
    mroc <- roc(obs ~ common, data=test, precent=TRUE, algorithm = 3)
    a <- mroc$auc[1]
    t <- coords.roc(mroc, x="best", ...)[1]
    cm <- confuse_mat.train(test, t)
    rc <- cm[1,1] / (cm[1,1] + cm[1,2])
    fp <- cm[2,1] / (cm[1,1] + cm[2,1])
    myROC <- ROCit(thresh=t, auc=a, confusematrix=cm, 
                   rarepercent=rc, falsepositive=fp, rocobj=mroc,
                   modtype = class(mod), 
                   modcall = paste(mod$call), 
                   datatype="test")
    return(myROC)
  }
}


resultSet <- modSearch(methods = c("knn", "glm", "svmRadial"), 
                       datatype = c("train", "test"), 
                       traindata = list(preds = trainT[, -19], class = trainT[, 19]), 
                       testdata = list(preds = test[, -19], class = test[, 19]), 
                       modelKeep = FALSE, length = 6, fitControl = ctrl, 
                       metric = "ROC")