setOldClass("roc")

##' @export
ROCit <- setClass("ROCit", representation(thresh = "numeric", 
                                          auc = "numeric", 
                                          confusematrix = "data.frame", 
                                          rarepercent = "numeric",
                                          falsepositive = "numeric", 
                                          rocobj = "roc",
                                          modtype = "character",
                                          modcall = "character", 
                                          datatype = "character"),
                  S3methods=TRUE)

##' Generic function to build ROCtest
##'
##' Allow the user to specify a formula for generating a binomial dependent variable
##' 
##' @param mod A model object to generate an \code{\linkS4class{ROCtest}} for
##' @param testdata A dataframe to generate the ROC for the mode on
##' @return A \code{\linkS4cass{ROCtest}} object 
##' @details
##' The object has the following items
##' \itemize{
##' \item{thresh - the threshold for the ROC}
##' \item{auc - the area under the curve}
##' \item{confusematrix - the confusion matrix for the ROC fit}
##' \item{rarepercent - percent of rare class correct}
##' \item{falsepositive - percent of false rare class identifications}
##' \item{modtype - the class of the model object}
##' \item{modcall - the call to the model fitting function}
##' \item{datatype - whether the ROC was computed on the "train" or the "test" data}
##' }
##' @note Yadda yadda yadda
##' @export
##' @author Jared E. Knowles
ROCtest <- function(mod, testdata, ...){
  UseMethod("ROCtest")
}

##' @aliases ROCtest
##' @title Getting an ROCtest on a generalized linear model
##' @S3method ROCtest glm
ROCtest.glm <- function(mod, testdata, ...){
  # can pass optional values such as: 
  #best.method="closest.topleft", 
  #best.weights=c(100, .11)
  if(missing(testdata)){
    #z <- terms(mod)
    #     test <- mod$data[, colnames(mod$data) %in% attr(z, "term.labels")]
    #     test <- na.omit(test)
    yhat <- mod$fitted.values
    y <- mod$y
    message("Generating ROC...")
    mroc <- roc(y~yhat, percent=TRUE)
    a <- mroc$auc[1]
    thresh <- coords(mroc, x="best", ...)[1]
    cm <- confuse_mat(mod, thresh, prop=FALSE)
    rc <- cm[1,1] / (cm[1,1] + cm[1,2])
    fp <- cm[2,1] / (cm[1,1] + cm[2,1])
    myROC <- ROCit(thresh=thresh, auc=a, confusematrix=cm, 
                   rarepercent=rc, falsepositive=fp, rocobj=mroc,
                   modtype = class(mod), modcall=paste(mod$formula),
                   datatype="train")
    return(myROC)
  }
  else if(!missing(testdata)){
    dv <- as.character(mod$formula[[2]])
    if(!exists("impute")){impute <- FALSE}
    testdata <- factor_norm(mod, testdata, impute=impute)
    testdata$fitted <- predict(mod, newdata=testdata, type="response")
    rocS <- testdata[, c(dv, "fitted")]
    names(rocS) <- c("y", "fitted")
    message("Generating ROC...")
    mroc <- roc(y~fitted, percent=TRUE, data=rocS)
    a <- mroc$auc[1]
    thresh <- coords(mroc, x="best", ...)[1]
    cm <- confuse_mat(mod, thresh, prop=FALSE, testdata=testdata)
    rc <- cm[1,1] / (cm[1,1] + cm[1,2])
    fp <- cm[2,1] / (cm[1,1] + cm[2,1])
    myROC <- ROCit(thresh=thresh, auc=a, confusematrix=cm, 
                   rarepercent=rc, falsepositive=fp, rocobj=mroc,
                   modtype = class(mod), modcall=paste(mod$formula), 
                   datatype="test")
    return(myROC)
    
  }
} 



##' @aliases ROCtest
##' @title Getting an ROCtest on a train object
##' @S3method ROCtest train
ROCtest.train <- function(mod, testdata){
  if(missing(testdata)){
    if(is.null(mod$terms)==TRUE){
      test <- extractProb(list(mod))
    } else if (is.null(mod$terms)==FALSE){
      test <- predict(mod, type="prob")
      test <- cbind(test, trainCLASS)
      names(test) <- c("Grad", "Non.grad", "obs")
    }
    if(is.null(test)==TRUE) stop("Cannot generate probabilities")
    print("Generating ROC...")
    mroc <- roc(obs ~ Grad, data=test, precent=TRUE)
    a <- mroc$auc[1]
    t <- coords.roc(mroc, x="best")[1]
    cm <- confuse_mat.train2(test, t)
    rc <- cm[1,1] / (cm[1,1] + cm[1,2])
    fp <- cm[2,1] / (cm[1,1] + cm[2,1])
    myROC <- ROCit(thresh=t, auc=a, confusematrix=cm, 
                   rarepercent=rc, falsepositive=fp, rocobj=mroc,
                   modtype = class(mod), 
                   modcall = paste(mod$call), datatype="train")
    return(myROC)
  }
  else if(!missing(testdata)){
    if(is.null(mod$terms)==TRUE){
      test <- extractProb(list(mod), testX = testData, testY=testCLASS)
    } else if (is.null(mod$terms)==FALSE){
      test <- predict(mod, newdata=cbind(testCLASS, testData), 
                      type="prob")
      test <- cbind(test, testCLASS)
      names(test) <- c("Grad", "Non.grad", "obs")
    }
    if(is.null(test)==TRUE) stop("Cannot generate probabilities")
    print("Generating ROC...")
    mroc <- roc(obs ~ Grad, data=test, precent=TRUE)
    a <- mroc$auc[1]
    t <- coords.roc(mroc, x="best")[1]
    cm <- confuse_mat.train2(test, t)
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


##' @aliases summary
##' @title Getting a summary of an ROCtest object
##' @S3method summary ROCit
summary.ROCit <- function(x){
  cat("Model Classification Statistics \n")
  cat("Performance on", x@datatype, "data \n")
  cat("Area under the ROC curve:", x@auc,"\n")
  cat("Rare Class Classification:", x@rarepercent*100, "\n")
  cat("Rare Class False Positives:", x@falsepositive*100, "\n")
  cat("Optimal ROC Threshold:", x@thresh, "\n")
  cat("\n")
  cat("Confusion Matrix: \n")
  cat("\n")
  print(x@confusematrix)
}
