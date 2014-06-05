##' Class "RMSEit" of RMSE statistics
##'
##' A \code{\link{glm}} or \code{\link{train}} object with a binary classification 
##' can be easily summarized using the receiver-operating characteristic statistic. 
##' This class provides an efficient mechanism to store and compare these results 
##' across models. 
##'
##' @name RMSEit-class
##' @aliases RMSEit-class
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{RMSEit}}.
##' @details
##' The object has the following items
##' \itemize{
##' \item{bestFit - best results from train}
##' \item{RMSE - best RMSE}
##' \item{datatype - whether the ROC was computed on the "train" or the "test" data}
##' \item{modtype - the class of the model object}
##' \item{modcall - the call to the model fitting function}
##' }
##' @seealso \code{\link{RMSEtest}}
##' @keywords classes
##' @examples
##'
##' showClass("RMSEit")
##' methods(class="RMSEit")
##' @export
RMSEit <- setClass("RMSEit", representation(bestFit = "data.frame", 
                                          RMSE = "numeric", 
                                          datatype = "character",
                                          modtype = "character",
                                          modcall = "call"),
                  S3methods=TRUE)

##' Generic function to build RMSEtest
##'
##' Allow the user to specify a formula for generating a binomial dependent variable
##' @usage RMSEtest(mod, testdata, ...)
##' @param mod A model object to generate an \code{\linkS4class{RMSEit}} for
##' @param testdata A dataframe to generate the ROC for the mode on
##' @param ... optional additional parameters. 
##' @return A \code{\linkS4class{RMSEit}} object 
##' @details
##' The object has the following items
##' \itemize{
##' \item{bestFit - best results from train}
##' \item{RMSE - best RMSE}
##' \item{datatype - whether the ROC was computed on the "train" or the "test" data}
##' \item{modtype - the class of the model object}
##' \item{modcall - the call to the model fitting function}
##' }
##' @note Yadda yadda yadda
##' @export RMSEtest
##' @rdname RMSEtest
##' @author Jared E. Knowles
RMSEtest <- function(mod, testdata=NULL, ...){
  UseMethod("RMSEtest")
}

##' @title Getting an RMSEtest on a train object
##' @rdname RMSEtest
##' @method RMSEtest train
##' @export
RMSEtest.train <- function(mod, testdata, ...){
  if(missing(testdata)){
    RMSEt <- RMSE(pred = predict(mod), obs = mod$trainingData$.outcome)
    best <- mod$results[mod$results[,"RMSE"] == min(mod$results[,"RMSE"]),]
    myRMSE <- RMSEit(bestFit = best, RMSE = RMSEt, datatype = "train", 
                   modcall = mod$call)
    return(myRMSE)
  } else{
    RMSEt <- RMSE(pred = predict(mod, newdata = testdata$preds), obs = testdata$class)
    best <- mod$results[mod$results[,"RMSE"] == min(mod$results[,"RMSE"]),]
    myRMSE <- RMSEit(bestFit = best, RMSE = RMSEt, datatype = "test", modtype = class(mod), 
                   modcall = mod$call)
    return(myRMSE)
  }
}
