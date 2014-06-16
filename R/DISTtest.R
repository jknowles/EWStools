# Dist test

#http://appliedpredictivemodeling.com/blog/?offset=1393611546982

fourStats <- function (data, lev = levels(data$obs), model = NULL) {
  ## This code will get use the area under the ROC curve and the
  ## sensitivity and specificity values using the current candidate
  ## value of the probability threshold.
  out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
  
  ## The best possible model has sensitivity of 1 and specifity of 1. 
  ## How far are we from that value?
  coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), 
                   ncol = 2, 
                   byrow = TRUE)
  colnames(coords) <- c("Spec", "Sens")
  rownames(coords) <- c("Best", "Current")
  c(out, Dist = dist(coords)[1])
}

##' Class "DISit" of distance statistics
##'
##' A \code{\link{glm}} or \code{\link{train}} object with a binary classification 
##' with rare events can be easily summarized using a distance statistic. 
##' This class provides an efficient mechanism to store and compare these results 
##' across models and across test, train, and validation datasets. 
##'
##' @name DISit-class
##' @aliases DISit-class
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{DISit}}.
##' @details
##' The object has the following items
##' \itemize{
##' \item{bestFit - best results from train}
##' \item{RMSE - best RMSE}
##' \item{datatype - whether the ROC was computed on the "train" or the "test" data}
##' \item{modtype - the class of the model object}
##' \item{modcall - the call to the model fitting function}
##' }
##' @seealso \code{\link{DIStest}}
##' @keywords classes
##' @examples
##'
##' showClass("DISit")
##' methods(class="DISit")
##' @export
DISit <- setClass("DISit", representation(thresh = "numeric", 
                                          auc = "numeric", 
                                          confusematrix = "confusionMatrix", 
                                          rarepercent = "numeric",
                                          falsepositive = "numeric", 
                                          rocobj = "roc",
                                          modtype = "character",
                                          modcall = "character", 
                                          datatype = "character"),
                  S3methods=TRUE)

##' @title Getting an DIStest on a train object
##' @rdname DIStest
##' @method DIStest train
##' @export
DIStest.train <- function(mod, testdata, ...){
  if(missing(testdata)){
    yhats <- probExtract(mod)
    if(is.null(yhats)==TRUE) stop("Cannot generate probabilities")
    
    # create a distance matrix
    coords <- matrix(c(1, 1, out["Spec"], out["Sens"]), 
                     ncol = 2, 
                     byrow = TRUE)
    colnames(coords) <- c("Spec", "Sens")
    rownames(coords) <- c("Best", "Current")
    #
    #
    mroc <- roc(.outcome ~ yhat, data=yhats, percent=TRUE, algorithm=3)
    a <- mroc$auc[1]
    thresh <- coords.roc(mroc, x="best", ...)[1]
    cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = thresh), 
                          reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
    myROC <- ROCit(thresh=mod$bestTune[["threshold"]], 
                   auc=a, confusematrix=cm, 
                   rarepercent=cm$byClass["Neg Pred Value"], 
                   falsepositive=1 - cm$byClass["Neg Pred Value"], 
                   rocobj=mroc,
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
    yhats <- probExtract(mod, testdata = testdata)
    if(is.null(yhats)==TRUE) stop("Cannot generate probabilities")
    mroc <- roc(.outcome ~ yhat, data=yhats, precent=TRUE, algorithm=3)
    a <- mroc$auc[1]
    thresh <- coords.roc(mroc, x="best", ...)[1]
    cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = thresh), 
                          reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
    myROC <- ROCit(thresh=thresh, auc=a, confusematrix=cm, 
                   rarepercent=cm$byClass["Neg Pred Value"], 
                   falsepositive=1 - cm$byClass["Neg Pred Value"], 
                   rocobj=mroc,
                   modtype = class(mod), 
                   modcall = paste(mod$call), datatype="test")
    return(myROC)
  }
}
