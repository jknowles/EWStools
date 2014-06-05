#' Summary of the Accuracy of Early Warning Indicators
#'
#' @name bowersEWS
#' @docType data
#' @description This dataset contains accuracy indicators for 110 separate predictive indicators 
#' of student dropout. These indicators are drawn from a literature search of over 
#' 30 separate studies in American public schools of early indicators of dropout. 
#' @source Bowers, A.J., Sprott, R., Taff, S.A. (2013) Do we Know Who Will Drop Out? A Review of 
#' the Predictors of Dropping out of High School: Precision, Sensitivity and Specificity. The 
#' High School Journal, 96(2), 77-100. \url{doi:10.1353/hsj.2013.0000} (Preprint available.)
#' @references Alex J. Bowers: \url{http://www.tc.columbia.edu/academics/?facid=ab3764}
#' @keywords data
NULL

#' Extended version of caret model list from caret package
#'
#' @name caretMethods
#' @docType data
#' @description Data on caret model types. 
#' @details
##' A dataframe with the following fields - most binary indicators:
#' \itemize{
#' \item{model - Name of the model}
#' \item{methodName - name of method for calling model in \code{\link{caret}}}
#' \item{type - a factor with three levels: Classification, Dual Use, and Regression}
#' \item{packages - names of packages needed for the model}
#' \item{tuningParameters - comma separated string of tuning parameters}
#' \item{tags - comma separated string of tags associated with model}
#' \item{requiresJava - yes/no indicating whether Java is needed to install packages and run model}
#' }
#' @source Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan
#' Engelhardt, Tony Cooper, Zachary Mayer and the R Core Team (2014). caret: Classification and
#' Regression Training. R package version 6.0-24. http://CRAN.R-project.org/package=caret
#' @references Caret website: \url{http://caret.r-forge.r-project.org/}
#' @keywords data
NULL

#' Matrix of caret models by tag
#'
#' @name caretTags
#' @docType data
#' @description Data on caret model types. 
#' @details
##' A dataframe with the following fields - most binary indicators:
##' \itemize{
##' \item{model - Name of the model}
##' \item{Classification - Binary indicator of classification model}
##' \item{Regression  - Binary indicator of regression model}
##' \item{Bagging - model does bagging}
##' \item{Bayesian.Model - model is Bayesian}
##' }
#' @source Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan
#' Engelhardt, Tony Cooper, Zachary Mayer and the R Core Team (2014). caret: Classification and
#' Regression Training. R package version 6.0-24. http://CRAN.R-project.org/package=caret
#' @references Caret website: \url{http://caret.r-forge.r-project.org/}
#' @keywords data
NULL


#' EWS test data
#'
#' @name EWStestData
#' @docType data
#' @description Sample data for testing EWStools methods.
#' @details
##' A list named modeldat with the following structure: 
##' \itemize{
##'   \item{traindata - a list of elements in training data}
##'   \item{testdata - a list of elements in test-data}
##'}
##' Each list contains two elements, a dataframe called preds and a vector 
##' called class. The preds dataframe is a numeric matrix that represents the 
##' model matrix of the predictors. The class is a factor vector that represents 
##' the classification of the cases to be checked. 
##' 
#' @source The code to build this dataset is in the inst directory
#' @keywords data
NULL
