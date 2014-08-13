################################################################################
# Title: ROC methods and objects for rare classification
################################################################################
################################################################################
# ROC Functions
################################################################################

setOldClass("roc")
setOldClass("confusionMatrix")
##' Class "ROCit" of ROC classification statistics
##'
##' A \code{\link{glm}} or \code{\link{train}} object with a binary classification 
##' can be easily summarized using the receiver-operating characteristic statistic. 
##' This class provides an efficient mechanism to store and compare these results 
##' across models. 
##'
##' @name ROCit-class
##' @aliases ROCit-class
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{ROCtest}}.
##' @details
##' The object has the following items
##' \itemize{
##' \item{thresh - the threshold for the ROC}
##' \item{auc - the area under the curve}
##' \item{confusematrix - the confusion matrix for the ROC fit, 
##' as provided by \code{\link{confusionMatrix}}}
##' \item{rarepercent - percent of rare class correct}
##' \item{falsepositive - percent of false rare class identifications}
##' \item{modtype - the class of the model object}
##' \item{modcall - the call to the model fitting function}
##' \item{datatype - whether the ROC was computed on the "train" or the "test" data}
##' }
##' @seealso \code{\link{ROCtest}}
##' @keywords classes
##' @examples
##'
##' showClass("ROCit")
##' methods(class="ROCit")
##' @export
ROCit <- setClass("ROCit", representation(thresh = "numeric", 
                                          auc = "numeric", 
                                          confusematrix = "confusionMatrix", 
                                          rarepercent = "numeric",
                                          falsepositive = "numeric", 
                                          rocobj = "roc",
                                          modtype = "character",
                                          modcall = "character", 
                                          datatype = "character"),
                  S3methods=TRUE)

##' Generic function to build ROCtest
##'
##' Explore the accuracy of classifiers using ROC as the metric
##' @usage ROCtest(mod, testdata, ...)
##' @param mod A model object to generate an \code{\linkS4class{ROCit}} for
##' @param testdata A dataframe to generate the ROC for the mode on
##' @param ... optional additional parameters. 
##' @return A \code{\linkS4class{ROCit}} object 
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
##' @export ROCtest
##' @rdname ROCtest
##' @author Jared E. Knowles
ROCtest <- function(mod, testdata=NULL, ...){
  UseMethod("ROCtest")
}

##' @title Getting an ROCtest on a generalized linear model
##' @rdname ROCtest
##' @method ROCtest glm
##' @export
ROCtest.glm <- function(mod, testdata, ...){
  if(missing(testdata)){
    yhats <- probExtract(mod)
   # message("Generating ROC...")
    mroc <- roc(.outcome ~ yhat, percent=TRUE, data = yhats)
    a <- mroc$auc[1]
    thresh <- coords(mroc, x="best", ...)[1]
    cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = thresh), 
                         reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
    myROC <- ROCit(thresh=thresh, auc=a, confusematrix=cm, 
                  rarepercent=cm$byClass["Neg Pred Value"], 
                  falsepositive=1 - cm$byClass["Neg Pred Value"], 
                  rocobj=mroc,
                  modtype = class(mod), 
                  modcall = paste(mod$formula), datatype="train")
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
#     dv <- as.character(mod$formula[[2]])
#     testdata2 <- cbind(testdata$preds, testdata$class)
#     names(testdata2) <- c(names(testdata$preds), dv)
#     testdata <- testdata2; rm(testdata2)
#     # end hack
#     testdata <- factor_norm(mod, testdata, ...)
    yhats <- probExtract(mod, testdata = testdata)
    #message("Generating ROC...")
    mroc <- roc(.outcome ~ yhat, percent=TRUE, data=yhats)
    a <- mroc$auc[1]
    thresh <- coords(mroc, x="best", ...)[1]
    cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = thresh), 
                          reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
    myROC <- ROCit(thresh=thresh, auc=a, confusematrix=cm, 
                   rarepercent=cm$byClass["Neg Pred Value"], 
                   falsepositive=1 - cm$byClass["Neg Pred Value"], 
                   rocobj=mroc,
                   modtype = class(mod), 
                   modcall=paste(mod$formula), 
                   datatype="test")
    return(myROC)
    
  }
} 

##' @title Getting an ROCtest on a train object
##' @rdname ROCtest
##' @method ROCtest train
##' @export
ROCtest.train <- function(mod, testdata, ...){
  if(missing(testdata)){
    yhats <- probExtract(mod)
    if(is.null(yhats)==TRUE) stop("Cannot generate probabilities")
    mroc <- roc(.outcome ~ yhat, data=yhats, percent=TRUE, algorithm=2)
    a <- mroc$auc[1]
    thresh <- coords.roc(mroc, x="best", ...)[1]
    cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = thresh), 
                          reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
    myROC <- ROCit(thresh=thresh, auc=a, confusematrix=cm, 
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
    mroc <- roc(.outcome ~ yhat, data=yhats, precent=TRUE, algorithm=2)
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

##' @title Getting an ROCtest on a caretEnsemble object
##' @rdname ROCtest
##' @method ROCtest caretEnsemble
##' @export
ROCtest.caretEnsemble <- function(mod, testdata, ...){
  if(missing(testdata)){
    yhats <- probExtract(mod)
    if(is.null(yhats)==TRUE) stop("Cannot generate probabilities")
    mroc <- roc(.outcome ~ yhat, data=yhats, percent=TRUE, algorithm=2)
    a <- mroc$auc[1]
    modThresh <- coords.roc(mroc, x="best")[1]
    cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = modThresh), 
                          reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
    myROC <- ROCit(thresh=modThresh, auc=a, confusematrix=cm, 
                   rarepercent=cm$byClass["Neg Pred Value"], 
                   falsepositive=1 - cm$byClass["Neg Pred Value"], 
                   rocobj= mroc,
                   modtype = names(mod$models), 
                   modcall = "", datatype="train")
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
    mroc <- roc(.outcome ~ yhat, data=yhats, percent=TRUE, algorithm=2)
    a <- mroc$auc[1]
    modThresh <- coords.roc(mroc, x="best")[1]
    cm <- confusionMatrix(reclassProb(yhats = yhats, thresh = modThresh), 
                          reference = yhats$.outcome, positive = levels(yhats$.outcome)[1])
    myROC <- ROCit(thresh=modThresh, auc=a, confusematrix=cm, 
                   rarepercent=cm$byClass["Neg Pred Value"], 
                   falsepositive=1 - cm$byClass["Neg Pred Value"], 
                   rocobj= mroc,
                   modtype = names(mod$models), 
                   modcall = "", datatype="train")
    return(myROC)
  }
}


##' @title Print a summary of an \code{\linkS4class{ROCit}} object
##' @param x a \code{\linkS4class{ROCit}} object generated by \code{\link{ROCtest}}
##' @param ... optional additional parameters. 
##' @return Values to the screen.
##' @note The values presented are for the optimal threshold as computed by the \code{\link{roc}} function.
##' @method print ROCit
##' @export
print.ROCit <- function(x, ...){
  cat("Model Classification Statistics \n")
  cat("Performance on", x@datatype, "data \n")
  cat("Area under the ROC curve:", x@auc,"\n")
  cat("Negative Predictive Values:", x@rarepercent*100, "\n")
  cat("Negative Class False Positive:", x@falsepositive*100, "\n")
  cat("Optimal ROC Threshold:", x@thresh, "\n")
  cat("\n")
  cat("Confusion Matrix: \n")
  cat("\n")
  print(x@confusematrix$table)
}



##' @title Extract a summary of an \code{\linkS4class{ROCit}} object
##' @param object a \code{\linkS4class{ROCit}} object generated by \code{\link{ROCtest}}
##' @param ... optional additional parameters. 
##' @return A list with the following items:
##' \itemize{
##' \item{datatype - whether the ROC was computed on the "train" or the "test" data}
##' \item{auc - the area under the curve}
##' \item{rarepercent - percent of rare class correct}
##' \item{falsepositive - percent of false rare class identifications}
##' \item{threshold - the optimal ROC threshold given by \code{\link{ROCtest}}}
##' \item{confusematrix - the confusion matrix for the ROC fit}
##' } 
##' @note The values presented are for the optimal threshold as computed by the \code{\link{roc}} function.
##' @method summary ROCit
##' @export
summary.ROCit <- function(object, ...){
  summary <- list(
    datatype = object@datatype, 
    auc = object@auc, 
    rarepercent =  object@rarepercent*100, 
    falsepositive = object@falsepositive*100,
    threshold = object@thresh,
    confusionmatrix = object@confusematrix
    )
  return(summary)
}
