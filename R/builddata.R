################################################################################
# Build data
################################################################################

##' @title Split data into a training and a test dataset
##' @param data a dataframe that the user would like to split into training and sample sets
##' @param class character value of the name of the dependent variable 
##' @param p the proportion of data to be placed into a training set 
##' @return A list with the following items:
##' \itemize{
##' \item{train - a data frame of the training values}
##' \item{test - a data frame of the test values}
##' \item{indexes - the row indexes of the original data frame selected to be in the training set}
##' } 
##' @note Built on the \code{\link{createDataPartition}} function in the \code{caret} package.
##' @export
splitData <- function(data, class, p){
  idx <- createDataPartition(data[, class], 1, p = p)
  train <- data[idx[[1]], ]
  test <- data[-idx[[1]], ]
  return(list(train = train, test = test, indexes = idx))
}

##' @title Turn a dataframe into a model matrix for caret functions
##' @param data a dataframe that the user would like to convert to a model matrix
##' @param predvars  a character vector of the names of predictor variables
##' @param na.omit  behavior with missing values, defaults to TRUE
##' @return A model matrix
##' @note Built on the \code{\link{model.matrix}} function 
##' @export
buildModelMatrix <- function(data, predvars, na.omit = TRUE){
  if(na.omit == TRUE){
    data <- na.omit(data[, predvars])
  } else {
    data <- data[, predvars]
  }
  
  FORM <-  paste0("~ 0 + ", paste0(predvars, collapse = " + "))
  FORM <- as.formula(FORM)
  out <- model.matrix(FORM, data = data)
  return(out)
}

##' @title Assemble train and test data for model building with EWStools
##' @param data a dataframe that the user would like to split into training and sample sets
##' @param class character value of the name of the dependent variable 
##' @param predvars  a character vector of the names of predictor variables
##' @param p the proportion of data to be placed into a training set 
##' @return A list of lists with the following items:
##' \itemize{
##' \item{traindata - a list with a dataframe of the predictor matrix called preds, and the class called class}
##' \item{testdata - a list with a dataframe of the predictor matrix called preds, and the class called class}
##' } 
##' @note Built on the \code{\link{createDataPartition}} function in the \code{caret} package.
##' @export
assembleData <- function(data, class, p, ...){
  if(class(data) != "matrix"){
    full.p <- buildModelMatrix(data, ...)
    full.p <- as.data.frame(full.p)
    full.p <- cbind(full.p, data[, class])
    names(full.p)[ncol(full.p)] <- class
    splits <- splitData(data = full.p, class = class, p = p)
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
    traindata$class <- as.factor(traindata$class)
    testdata$class <- as.factor(testdata$class)
  }
  return(list(traindata = traindata, testdata = testdata))
  
}


##' @title Omit linear combinations from a predictor matrix
##' @description Some \code{\link{train}} methods do not play well with linear 
##' combinations in the predictor matrix. This function allows the user to strip 
##' them out using the \code{\link{findLinearCombos}} function in the \code{caret} 
##' package.
##' @param A matrix of predictors
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