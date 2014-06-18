##' @title Sample caret methods to maximize dissimilarity in the algorithm
##' @description Create a subset of caret methods in an effort to maximize 
##' dissimilarity between them along the features identified in the caretTags 
##' dataset
##' @param methods a vector of character representing method names in caret
##' @param k the number of clusters
##' @param distancemethod the method used to calculate the dissimilarity
##' @param n  number methods to sample per cluster
##' @param what what to return, either "tags" for the caretTags data or "matrix" 
##' for the dissimilarity matrix
##' @return Returns either a dataframe or a matrix with the dissimilarity values 
##' for the methods sampled and the names of the methods. 
##' @export
##' @examples
#' require(EWStools)
#' data(caretTags)
#' dissimMethod(sample(caretTags$method, 30), k = 4, distancemethod = "maximum", n = 2, what = "tags") 
dissimMethod <- function(methods, k, distancemethod, n, 
                         what = c("tags", "matrix")){
  data(caretTags)
  sub <- caretTags[caretTags$method %in% methods, ]
  sub$model <- NULL
  sub$method <- NULL
  if(distancemethod %in% c("euclidean", "maximum", "manhattan", "canberra", 
                           "binary", "minkowski")){
    D <- stats:::dist(sub, method = distancemethod)
  } else if(distancemethod %in% c("Jaccard", "Kulczynski1", "Kulczynski2", 
                                  "Mountford", "Fager", "Russel", 
                                  "simple matching", "Hamman", "Faith", "Tanimoto", 
                                  "Dice","Phi", "Stiles", "Michael", "Mozley", "Yule", 
                                  "Yule2", "Ochiai", "Simpson", "Braun-Blanquet", 
                                  "cosine", "eJaccard", "fJaccard", "correlation", 
                                  "Chi-squared", "Phi-squared", "Tschuprow", "Cramer", 
                                  "Pearson", "Gower", "Euclidean", "Mahalanobis", "Bhjattacharyya", 
                                  "Manhattan", "supremum", "Minkowski", "Canberra", 
                                  "Wave", "divergence", "Kullback", "Bray", "Soergel", 
                                  "Levenshtein", "Podani", "Chord", "Geodesic", 
                                  "Whittaker", "Hellinger")){
    D <- proxy:::dist(sub, method = distancemethod)
  } else{
    stop("I don't recognize the method to calculate the distance you provided. 
         Check pr_DB in the proxy package for available methods or to register 
         a new method.")
  }
  mat <- as.matrix(D)
  dimnames(mat)[[1]] <- methods
  dimnames(mat)[[2]] <- methods
  clustObj <- hclust(D)
  cluster <- cutree(clustObj, k = k)
  mat <- cbind(mat, cluster)
  # choose how many records to sample per unique 'carb' value
  if(missing(what)){
    what <- "matrix"
    
  }
  
  if(what == "matrix"){
    method.sample <- mat[ 
      unlist( 
        tapply( 
          1:nrow(mat) , 
          mat[, "cluster"] , 
          sample , 
          n 
        ) 
      ) , ]
    method.sample <- method.sample[,  match(rownames(method.sample), colnames(method.sample))]
    return(method.sample)
  } else if(what == "tags"){
    data(caretTags)
    sub <- caretTags[caretTags$method %in% methods, ]
    sub$cluster <- cuts
    method.sample <- sub[ 
      unlist( 
        tapply( 
          1:nrow(sub) , 
          sub$cluster , 
          sample , 
          n 
        ) 
      ) , ]
    return(method.sample)
  }
}



# 
# data(caretTags)
# caretTags$model <- NULL
# caretTags$method <- NULL
# 
# library(proxy)
# D <- dist(caretTags, method = "Jaccard")
# Dm <- as.matrix(D)
# sim <- Dm
# 
# grps <- rep(NA, nrow(sim))
# grps[caretTags[,"Classification"] == 1 & caretTags[,"Regression"] == 1] <- 4
# grps[caretTags[,"Classification"] == 0 & caretTags[,"Regression"] == 1] <- 2
# grps[caretTags[,"Classification"] == 1 & caretTags[,"Regression"] == 0] <- 3


##' @title An function for creating a confidence interval from a vector
##' @description Given a vector of numerics, x, this returns a numeric vector 
##' of length 3, low confidence interval, high confidence interval, and the median
##' @param x A numeric vector which contain NAs
##' @param scale The scale factor of the standard deviation out of which to build the CI
##' @export
ci <- function(x, scale){
  # x = var
  # size = number of sds to include in ci
  lowCI <- median(x, na.rm=T) - (scale * sd(x, na.rm=T))
  hiCI <- median(x, na.rm=T) + (scale * sd(x, na.rm=T))
  med <- median(x, na.rm=T)
  CI <- c(lowCI, med, hiCI)
  return(CI)
}

##' @title Summarize the results of modSearch
##' @description Given a dataframe produced by modSearch, identify the top and bottom n 
##' models and describe their accuracy and their efficiency of fit
##' @param df a dataframe resulting from a call to \code{\link{modSearch}}
##' @param n an integer specified by the user representing how many "best" and "worst" models to report, 
##' defaults to 5
##' @return a list with a bunch of elements
##' @details if the number of unique methods is less than n, then n defaults to the number of unique methods
##' @export
modSearchResults <- function(df, n = 5){
  # sanitize n to avoid NA padding
  n <- ifelse(n < length(unique(df$method)), n, length(unique(df$method)))
  # identify type of results
  if(names(df)[1] == "RMSE"){
    dv  <- "RMSE"
    df$sort <- 1 - df[, dv]
  } else if(names(df[1]) == "sens"){
    dv <- "auc"
    df$sort <- df[, dv]
  }
  modSuc <- unique(df$method[!is.na(df[, dv])])
  modFail <- unique(df$method[is.na(df[, dv])])
  # look at test performance
  if(!"test" %in% df$grp == TRUE){ # awkward negation checking for test results
    warning("Rerun modSearch with option to return test data statistics using datatype = 'test'")
  } 
  teststats <- df[df$grp == "test", ]
  topMetric <- unique(teststats[, dv])[order(-unique(teststats[, "sort"]))][1:n]
  badMetric <- unique(teststats[, dv])[order(unique(teststats[, "sort"]))][1:n]
  bestMethod <- unique(df$method[df[, dv] %in% topMetric])
  worstMethod <- unique(df$method[df[, dv] %in% badMetric])
  effDF <- df[df$grp == "test", c(dv, names(df)[4:7], "sort")]
  effDF <- effDF[!duplicated(effDF), ]
  if(dv == "RMSE"){
    effDF$efficiency <- round(10/(effDF$elapsedTime + effDF[, dv]^2), digits = 3)
  } else{
    effDF$efficiency <- effDF[, "sort"] / effDF$elapsedTime
  }
  fastMethods <-effDF$method[order(-effDF$efficiency)][1:n]
  slowMethods <-effDF$method[order(effDF$efficiency)][1:n]
  effDF[, "sort"] <- NULL
  row.names(effDF) <- NULL
  #
  TrainResults <- list(modSuc = modSuc, modFail = modFail, 
                       topMetric = topMetric, badMetric = badMetric,
                       worstMethod = worstMethod, 
                       bestMethod = bestMethod,
                       fastMethods = fastMethods,
                       slowMethods = slowMethods,
                       efficiencyResults = effDF)
  return(TrainResults)
}

##' @title Extract probabilities from various types of R models used for classification
##' @description Extracts probabilities consistently for using in confusion matrix calculations
##' @param mod an R model object either of glm, caret, or caretEnsemble class
##' @param testdata a list with two elements, a preds dataframe and a factor called class
##' @return a dataframe with yhat, the inverse of yhat, and the outcome as a factor
##' @details Only works for caretEnsemble, train, and glm objects currently. 
##' @export
probExtract <- function(mod, testdata = NULL){
  if(class(mod)[1] == "caretEnsemble"){
    if(missing(testdata)){
      yhats <- predict(mod, keepNA = TRUE) # caretEnsemble seems to predict non-reference class
      yhats <- data.frame(yhat = yhats, yhatInv = 1 - yhats, 
                          .outcome = mod$models[[1]]$trainingData$.outcome)
      names(yhats) <- c("yhatInv", "yhat", ".outcome") # hack to make prediction line up with 
                                                        # proper class
      return(yhats)
    } else {
      yhats <- predict(mod, keepNA = TRUE, newdata = testdata$preds)
      yhats <- data.frame(yhat = yhats, yhatInv = 1 - yhats, 
                          .outcome = mod$models[[1]]$trainingData$.outcome)
      names(yhats) <- c("yhatInv", "yhat", ".outcome") # hack to make prediction line up with 
      return(yhats)
    }
  } else if(class(mod)[1]== "train"){
    if(missing(testdata)){
      yhats <- predict(mod, type = "prob")
      yhats <- data.frame(yhats, 
                          .outcome = mod$trainingData$.outcome)
      names(yhats) <- c("yhat", "yhatInv", ".outcome")
      return(yhats)
    } else {
      newdata <- cbind(testdata$preds, testdata$class)
      names(newdata) <- c(names(testdata$preds), names(testdata$class))
      yhats <- predict(mod, newdata = testdata$preds, type = "prob")
      yhats <- data.frame(yhats, 
                          .outcome = testdata$class)
      names(yhats) <- c("yhat", "yhatInv", ".outcome")
      return(yhats)
    }
  } else if(class(mod)[1] == "glm"){
    if(missing(testdata)){
      yhats <- predict(mod, type = "response")
      yhats <- data.frame(yhat = yhats, yhatInv = 1 - yhats, 
                          .outcome = mod$y)
      names(yhats) <- c("yhatInv", "yhat", ".outcome") # hack to make prediction line up with 
      return(yhats)
    } else {
      yhats <- predict(mod, newdata = testdata$preds, type = "response")
      yhats <- data.frame(yhat = yhats, yhatInv = 1 - yhats, 
                          .outcome =  testdata$class)
      names(yhats) <- c("yhatInv", "yhat", ".outcome") # hack to make prediction line up with 
      return(yhats)
    }
    } else {
    stop("Please provide either a caretEnsemble or train object")
  }
}

##' @title Convert probabilities to classifications 
##' @description For building confusion matrices
##' @param yhats a dataframe resulting from a call to \code{\link{probExtract}}
##' @param thresh a numeric from 0 to 1 representing the cutpoint for the probability
##' @return a vector the length of rows in yhats representing the conversion from a numeric 
##' probability to a factor using the threshold thresh
##' @details Only works for caretEnsemble, train, and glm objects currently. Corrects for 
##' the differential ordering in what the predicted probability is with respect to. 
##' @note If you find errors, it is likely that the probabilities are inverted. Please report this. 
##' @keywords internal
reclassProb <- function(yhats, thresh){
  if(class(yhats$.outcome) != "factor"){
    yhats$.outcome <- as.factor(yhats$.outcome)
    warning("attempting to force outcome to factor")
    if(length(levels(yhats$.outcome)) != 2){
      stop("Please ensure that .outcome is a two-class factor")
    }
  }
  if(class(yhats) != "data.frame"){
    yhats <- as.data.frame(yhats)
  }
  if(class(yhats$yhat) != "numeric"){
    stop("Check predictions, yhat must be numeric")
  }
  predLvl <- levels(yhats$.outcome)[1]
  yclass <- ifelse(yhats$yhat >= thresh, predLvl, levels(yhats$.outcome)[2])
  return(yclass)
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

