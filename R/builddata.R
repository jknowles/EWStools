##' @title Split data into a training and a test dataset
##' @param data a dataframe that the user would like to split into training and sample sets
##' @param class character value of the name of the dependent variable 
##' @param p the proportion of data to be placed into a training set 
##' @param pvalid the proportion of data to be placed into the validation set 
##' @return A list with the following items:
##' \itemize{
##' \item{train - a data frame of the training values}
##' \item{test - a data frame of the test values}
##' \item{indexes - the row indexes of the original data frame selected to be in the training set}
##' } 
##' @note Built on the \code{\link{createDataPartition}} function in the \code{caret} package. pvalid 
##' is defined against the records remaining after holding out p for the training set.
##' @export
splitData <- function(data, class, p, pvalid = NULL){
  if(p == 1){
    warning("Extreme value of p selected and may result in NULL test data set")
  } else if(p == 0){
    stop("Training data partition set to 0. Did you mean p = 1?")
  }
  if(missing(pvalid)){
    idx <- createDataPartition(data[, class], times = 1, p = p)
    train <- data[idx[[1]], ]
    test <- data[-idx[[1]], ]
    return(list(train = train, test = test, indexes = idx))
    } else if(!missing(pvalid)) {
      if(pvalid == 1){
        warning("Extreme value of pvalid selected and may result in NULL test data set")
      } else if(pvalid == 0){
        stop("Validation data partition set to 0. Did you mean pvalid = 1?")
      }
      idx <- createDataPartition(data[, class], times = 3, p = p)
      train <- data[idx[[1]], ]
      rest <- data[-idx[[1]], ]
      idx2 <- createDataPartition(rest[, class], times = 2, p = pvalid)
      valid <- rest[idx2[[1]], ]
      test <- rest[-idx2[[1]], ]
      return(list(train = train, test = test, valid = valid, 
                  indexes = list(idx, idx2)))
      
    }

}

##' @title Turn a dataframe into a model matrix for caret functions
##' @param data a dataframe that the user would like to convert to a model matrix
##' @param predvars  a character vector of the names of predictor variables
##' @param keepNA  behavior with missing values, defaults to FALSE
##' @return A model matrix
##' @note Built on the \code{\link{model.matrix}} function. Does not produce an 
##' intercept term. Does not drop collinear factor levels. 
##' @export
buildModelMatrix <- function(data, predvars = NULL, keepNA = FALSE){
  if(class(data) != "data.frame"){
    stop("Please supply a data.frame with column names")
  }
  if(missing(predvars)){
    predvars <- colnames(data)
  }
  findFac <- function(x) !is.numeric(x) # quick function to find non-numeric columns
  # convert non-numeric columns to factors
  data[, sapply(data, findFac)] <- lapply(data[, sapply(data, findFac)], factor)
  FORM <-  paste0("~ 0 + ", paste0(predvars, collapse = " + "))
  FORM <- as.formula(FORM)
  if(keepNA == FALSE){
    data <- model.frame(FORM, data = data, na.action = na.omit)
    out <- model.matrix(FORM, data, 
                        contrasts.arg = lapply(data[,sapply(data, findFac)], 
                                               contrasts, contrasts=FALSE))
  } else {
    data <- model.frame(FORM, data = data, na.action = na.pass)
    out <- model.matrix(FORM, data, 
                        contrasts.arg = lapply(data[,sapply(data, findFac)], 
                                               contrasts, contrasts=FALSE))
  }
   return(out)
}

##' @title Assemble train and test data for model building with EWStools
##' @param data a dataframe that the user would like to split into training and sample sets
##' @param class character value of the name of the dependent variable 
##' @param predvars  a character vector of the names of predictor variables
##' @param p the proportion of data to be placed into a training set 
##' @param classification Is the training set for a classification problem or not? Default is TRUE.
##' @param keepNA Should missing values be preserved in the data sets? Logical. Default is FALSE.
##' @param ... additional arguments to be passed to assembleData
##' @return A list of lists with the following items:
##' \itemize{
##' \item{traindata - a list with a dataframe of the predictor matrix called preds, and the class called class}
##' \item{testdata - a list with a dataframe of the predictor matrix called preds, and the class called class}
##' } 
##' @note Built on the \code{\link{createDataPartition}} function in the \code{caret} package.
##' @details To return a 3-way split with a validation set, use the \code{pvalid} argument.If classification is set to true the "class" component of the list will be forced to a factor 
##' for being fed into the train routine. 
##' @export
assembleData <- function(data, class, p, predvars, classification = TRUE, keepNA = FALSE, ...){
  args <- as.list(substitute(list(...)))
  completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  if("pvalid" %in% names(args)){
    if(class(data) != "matrix"){
      if(!missing(predvars)){
        full.p <- buildModelMatrix(data, predvars, keepNA = keepNA)
        full.p <- as.data.frame(full.p)
        if(keepNA == FALSE){
          tmp <- completeFun(data, names(data) %in% predvars)
          full.p <- cbind(full.p, tmp[, class])
        } else{
          full.p <- cbind(full.p, data[, class])
        }
        names(full.p)[ncol(full.p)] <- class
        splits <- splitData(data = full.p, class = class, p = p, ...)
      } else {
        splits <- splitData(data = data, class = class, p = p, ...)
      }
    } else {
      splits <- splitData(data = data, class = class, p = p, ...)
    }
    traindata <- list(preds = splits$train[, colnames(splits$train) != class], 
                      class = splits$train[, class])
    testdata <- list(preds = splits$test[, colnames(splits$test) != class], 
                     class = splits$test[, class])
    validdata <- list(preds = splits$valid[, colnames(splits$valid) != class], 
                     class = splits$valid[, class])
    if(class(data) == "matrix"){
      mode(traindata$preds) <- "numeric"
      mode(testdata$preds) <- "numeric"
      mode(validdata$preds) <- "numeric"
      if(classification == TRUE){
        traindata$class <- as.factor(traindata$class)
        testdata$class <- as.factor(testdata$class)
        validdata$class <- as.factor(validdata$class)
      }
    }
    return(list(traindata = traindata, testdata = testdata, validdata = validdata))
  } else {
    if(class(data) != "matrix"){
      if(!missing(predvars)){
        full.p <- buildModelMatrix(data, predvars, keepNA = keepNA)
        full.p <- as.data.frame(full.p)
        if(keepNA == FALSE){
          tmp <- completeFun(data, names(data) %in% predvars)
          full.p <- cbind(full.p, tmp[, class])
        } else{
          full.p <- cbind(full.p, data[, class])
        }
         names(full.p)[ncol(full.p)] <- class
        splits <- splitData(data = full.p, class = class, p = p)
      } else {
        splits <- splitData(data = data, class = class, p = p)
      }
    } else {
      splits <- splitData(data = data, class = class, p = p)
    }
    
    traindata <- list(preds = splits$train[, colnames(splits$train) != class], 
                      class = splits$train[, class])
    testdata <- list(preds = splits$test[, colnames(splits$test) != class], 
                     class = splits$test[, class])
    if(class(data) == "matrix"){
      mode(traindata$preds) <- "numeric"
      mode(testdata$preds) <- "numeric"
      if(classification == TRUE){
        traindata$class <- as.factor(traindata$class)
        testdata$class <- as.factor(testdata$class)
      }
    }
    return(list(traindata = traindata, testdata = testdata))
  }
}


##' @title Omit linear combinations from a predictor matrix
##' @description Some \code{\link{train}} methods do not play well with linear 
##' combinations in the predictor matrix. This function allows the user to strip 
##' them out using the \code{\link{findLinearCombos}} function in the \code{caret} 
##' package.
##' @param preds A matrix of predictors
##' @return A trimmed matrix
##' @note Built on the \code{\link{findLinearCombos}} function. To find out which 
##' columns will be trimmed, call \code{\link{findLinearCombos}} on the matrix 
##' first. 
##' @export
omitLinearCombos <- function(preds){
  combos <- findLinearCombos(preds)
  preds <- preds[, -combos$remove]
  return(preds)
}