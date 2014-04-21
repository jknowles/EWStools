#' Summary of the Accuracy of Early Warning Indicators
#'
#' @name bowersEWS
#' @docType data
#' @description This dataset contains accuracy indicators for 110 separate predictive indicators 
#' of student dropout. These indicators are drawn from a literature search of over 
#' 30 separate studies in American public schools of early indicators of dropout. 
#' @author Jared Knowles 
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
#' @author Jared Knowles 
#' @source Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan
#' Engelhardt, Tony Cooper, Zachary Mayer and the R Core Team (2014). caret: Classification and
#' Regression Training. R package version 6.0-24. http://CRAN.R-project.org/package=caret
#' @references Caret website: \url{http://caret.r-forge.r-project.org/}
#' @keywords data
NULL
# caretMethods <- read.csv(file = "data/caretMethods.csv", stringsAsFactors = FALSE)
# 
# names(caretMethods) <- c("model", "methodName", "type", "packages", "tuningParameters", 
#                          "tags", "requiresJava")
# caretMethods[, 7] <- factor(caretMethods[, 7])
# caretMethods[, 3] <- factor(caretMethods[, 3])
# 
# save(caretMethods, file = "data/caretMethods.rda", compress = "gzip")

#' Matrix of caret models by tag
#'
#' @name caretTags
#' @docType data
#' @description Data on caret model types. 
#' @author Jared Knowles 
#' @source Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan
#' Engelhardt, Tony Cooper, Zachary Mayer and the R Core Team (2014). caret: Classification and
#' Regression Training. R package version 6.0-24. http://CRAN.R-project.org/package=caret
#' @references Caret website: \url{http://caret.r-forge.r-project.org/}
#' @keywords data
NULL

# caretTags <- read.csv(file = "data/tag_data.csv", stringsAsFactors = FALSE)
# names(caretTags)[1] <- "model"
# 
# save(caretTags, file = "data/caretTags.rda", compress = "gzip")


