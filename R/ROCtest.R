################################################################################
# Title: ROC methods and objects for rare classification
################################################################################
################################################################################
# ROC Functions
################################################################################

setOldClass("roc")

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
##' \item{confusematrix - the confusion matrix for the ROC fit}
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
  # can pass optional values such as: 
  #best.method="closest.topleft", 
  #best.weights=c(100, .11)
  if(missing(testdata)){
    #z <- terms(mod)
    #     test <- mod$data[, colnames(mod$data) %in% attr(z, "term.labels")]
    #     test <- na.omit(test)
    yhat <- mod$fitted.values
    y <- mod$y
   # message("Generating ROC...")
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
    # error handling
    if(class(testdata) != "list"){
      stop("Please provide testdata as a named list with elements 'preds' and 'class'")
    }
    if("preds" %in% names(testdata)){
      
    } else {
      stop("Please provide testdata as a named list with elements 'preds' and 'class'")
    }
    # end error handling
    dv <- as.character(mod$formula[[2]])
    testdata2 <- cbind(testdata$preds, testdata$class)
    names(testdata2) <- c(names(testdata$preds), dv)
    testdata <- testdata2; rm(testdata2)
    # end hack
    if(!exists("impute")){impute <- FALSE}
    testdata <- factor_norm(mod, testdata, ...)
    testdata$fitted <- predict(mod, newdata=testdata, type="response")
    rocS <- testdata[, c(dv, "fitted")]
    names(rocS) <- c("y", "fitted")
    #message("Generating ROC...")
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

##' @title Getting an ROCtest on a train object
##' @rdname ROCtest
##' @method ROCtest train
##' @export
ROCtest.train <- function(mod, testdata, ...){
  if(missing(testdata)){
    if(is.null(mod$terms)==TRUE){
      test <- extractProb(list(mod))
      names(test)[1:3] <- c("common", "rare", "obs")
    } else if (is.null(mod$terms)==FALSE){
      test <- predict(mod, type="prob")
      test <- cbind(test, mod$trainingData$.outcome)
      names(test) <- c("common", "rare", "obs")
    }
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


##' @title Internal function to aide with factors in predicting new data for ROCtest
##' @keywords internal
factor_norm <- function(mod, testdata, impute=FALSE, ...){
  facnames <- names(mod$xlevels)
  if(impute==FALSE){
    for(i in facnames){
      x <- as.character(testdata[,i])
      levels <- c(unlist(mod$xlevels[i]))
      chk <- unique(x) %in% levels
      if(length(chk[isTRUE(chk)]) > 0){
        testdata[, i] <- as.character(testdata[, i])
        id <- which(!(testdata[, i] %in% levels))
        testdata[id, i] <- NA
        testdata[,i] <- factor(testdata[,i])
      }
    }
    return(testdata)
  } else if(impute==TRUE){
    for(i in facnames){
      x <- as.character(testdata[,i])
      levels <- c(unlist(mod$xlevels[i]))
      chk <- unique(x) %in% levels
      if(length(chk[!is.na(chk)]) > 0){
        testdata[, i] <- as.character(testdata[, i])
        id <- which(!(testdata[, i] %in% levels))
        a <- as.data.frame(mod$coefficients)
        a$name <- row.names(a)
        a <- a[grepl(i, row.names(a)), ]
        a <- a[order(a[1]) ,]
        newlevel <- a$name[nrow(a) %/% 2]
        testdata[id, i] <- gsub(i,"",newlevel)
        testdata[, i] <- as.factor(testdata[, i])
      }
    }
    return(testdata)
  }
}




##' @title Print a summary of an \code{\linkS4class{ROCit}} object
##' @param x a \code{\linkS4class{ROCit}} object generated by \code{\link{ROCtest}}
##' @param ... optional additional parameters. 
##' @return A 
##' @note The values presented are for the optimal threshold as computed by the \code{\link{roc}} function.
##' @method print ROCit
print.ROCit <- function(x, ...){
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
