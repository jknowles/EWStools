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


setOldClass("roc")
setOldClass("confusionMatrix")

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
                                          dist = "numeric", 
                                          coords = "matrix", 
                                          auc = "numeric", 
                                          confusematrix = "confusionMatrix", 
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
    
    mroc <- roc(.outcome ~ yhat, data=yhats, percent=TRUE, algorithm=3)
    a <- mroc$auc[1]
    thresh <- coords.roc(mroc, x="best", ...)[1]
    cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = thresh), 
                          reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
    # create a distance matrix
    coords <- matrix(c(1, 1, 1 - cm$byClass["Specificity"], 
                       cm$byClass["Sensitivity"]), 
                     ncol = 2, 
                     byrow = TRUE)
    colnames(coords) <- c("Spec", "Sens")
    rownames(coords) <- c("Best", "Current")
    Dist = dist(coords)[1]
    myDIS <- DISit(thresh = thresh, 
                   dist = Dist, coords = coords,
                   auc = a, confusematrix = cm, 
                   rocobj = mroc,
                   modtype = class(mod), 
                   modcall = paste(mod$call), datatype="train")
    return(myDIS)
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
    # create a distance matrix
    coords <- matrix(c(1, 1, 1 - cm$byClass["Specificity"], 
                       cm$byClass["Sensitivity"]), 
                     ncol = 2, 
                     byrow = TRUE)
    colnames(coords) <- c("Spec", "Sens")
    rownames(coords) <- c("Best", "Current")
    Dist = dist(coords)[1]
    myDIS <- DISit(thresh = thresh, 
                   dist = Dist, coords = coords,
                   auc = a, confusematrix = cm, 
                   rocobj = mroc,
                   modtype = class(mod), 
                   modcall = paste(mod$call), datatype="test")
    return(myDIS)
  }
}



##' Generic function to build DIStest
##'
##' Explore the accuracy of binary classifiers using distance from a perfect classifier
##' @usage DIStest(mod, testdata, ...)
##' @param mod A model object to generate an \code{\linkS4class{DISit}} for
##' @param testdata A dataframe to generate the ROC for the mode on
##' @param ... optional additional parameters. 
##' @return A \code{\linkS4class{DISit}} object 
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
##' @export DIStest
##' @rdname DIStest
##' @author Jared E. Knowles
DIStest <- function(mod, testdata=NULL, ...){
  UseMethod("DIStest")
}


