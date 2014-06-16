## Rewrite caret model objects to optimize thresholds:

rfDist <- list(label = "rf",
                  library = c("randomForest"),
                  type = c("Classification"),
                  parameters  = data.frame(parameter = c("mtry", "threshold"),
                                           class = c("numeric", "numeric"),
                                           label = c("#Randomly Selected Predictors",
                                                     "Probability Cutoff")),
                  grid = function(x, y, len = NULL) {
                    p <- ncol(x)
                    expand.grid(mtry = floor(sqrt(p)), 
                                threshold = seq(.01, .99, length = len))
                  },
                  loop = function(grid) {   
                    library(plyr)
                    loop <- ddply(grid, c("mtry"),
                                  function(x) c(threshold = max(x$threshold)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$threshold)) {
                      index <- which(grid$mtry == loop$mtry[i])
                      cuts <- grid[index, "threshold"] 
                      submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(length(levels(y)) != 2)
                      stop("This works only for 2-class problems")
                    randomForest(x, y, mtry = param$mtry, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    class1Prob <- predict(modelFit, 
                                          newdata, 
                                          type = "prob")[, modelFit$obsLevels[1]]
                    ## Raise the threshold for class #1 and a higher level of
                    ## evidence is needed to call it class 1 so it should 
                    ## decrease sensitivity and increase specificity
                    out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                                  modelFit$obsLevels[1], 
                                  modelFit$obsLevels[2])
                    if(!is.null(submodels))
                    {
                      tmp2 <- out
                      out <- vector(mode = "list", length = length(submodels$threshold))
                      out[[1]] <- tmp2
                      for(i in seq(along = submodels$threshold)) {
                        out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                                             modelFit$obsLevels[1], 
                                             modelFit$obsLevels[2])
                      }
                    } 
                    out  
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- as.data.frame(predict(modelFit, newdata, type = "prob"))
                    if(!is.null(submodels))
                    {
                      probs <- out
                      out <- vector(mode = "list", length = length(submodels$threshold)+1)
                      out <- lapply(out, function(x) probs)
                    } 
                    out 
                  },
                  predictors = function(x, ...) {
                    ## After doing some testing, it looks like randomForest
                    ## will only try to split on plain main effects (instead
                    ## of interactions or terms like I(x^2).
                    varIndex <- as.numeric(names(table(x$forest$bestvar)))
                    varIndex <- varIndex[varIndex > 0]
                    varsUsed <- names(x$forest$ncat)[varIndex]
                    varsUsed
                  },
                  varImp = function(object, ...){
                    varImp <- randomForest::importance(object, ...)
                    if(object$type == "regression")
                      varImp <- data.frame(Overall = varImp[,"%IncMSE"])
                    else {
                      retainNames <- levels(object$y)
                      if(all(retainNames %in% colnames(varImp))) {
                        varImp <- varImp[, retainNames]
                      } else {
                        varImp <- data.frame(Overall = varImp[,1])
                      }
                    }
                    
                    out <- as.data.frame(varImp)
                    if(dim(out)[2] == 2) {
                      tmp <- apply(out, 1, mean)
                      out[,1] <- out[,2] <- tmp  
                    }
                    out
                  },
                  levels = function(x) x$classes,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])

knnDist <- list(label = "k-Nearest Neighbors", 
                library = NULL, 
                type = c("Classification"), 
                parameters = data.frame(parameter = c("k", "threshold"), 
                                        class = c("numeric", "numeric"), 
                                        label = c("# Neighbors", "Probability cutoff")),
                grid = function(x, y, len = NULL){
                  dat <- expand.grid(k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0], 
                                     threshold =  seq(.01, .99, length = len))
                  return(dat)
                }, 
               loop =  function(grid) {   
                 library(plyr)
                 loop <- ddply(grid, c("k"),
                               function(x) c(threshold = max(x$threshold)))
                 submodels <- vector(mode = "list", length = nrow(loop))
                 for(i in seq(along = loop$threshold)) {
                   index <- which(grid$k == loop$k[i])
                   cuts <- grid[index, "threshold"] 
                   submodels[[i]] <- data.frame(threshold = cuts[cuts != loop$threshold[i]])
                 }    
                 list(loop = loop, submodels = submodels)
               }, 
              fit = function (x, y, wts, param, lev, last, classProbs, 
                                  ...) 
               {
                 if (is.factor(y)) {
                   knn3(as.matrix(x), y, k = param$k, ...)
                 }
                 else {
                   knnreg(as.matrix(x), y, k = param$k, ...)
                 }
               }, 
               predict = function(modelFit, newdata, submodels = NULL) {
                 class1Prob <- predict(modelFit, 
                                       newdata, 
                                       type = "prob")[, modelFit$obsLevels[1]]
                 ## Raise the threshold for class #1 and a higher level of
                 ## evidence is needed to call it class 1 so it should 
                 ## decrease sensitivity and increase specificity
                 out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                               modelFit$obsLevels[1], 
                               modelFit$obsLevels[2])
                 if(!is.null(submodels))
                 {
                   tmp2 <- out
                   out <- vector(mode = "list", length = length(submodels$threshold))
                   out[[1]] <- tmp2
                   for(i in seq(along = submodels$threshold)) {
                     out[[i+1]] <- ifelse(class1Prob >= submodels$threshold[[i]],
                                          modelFit$obsLevels[1], 
                                          modelFit$obsLevels[2])
                   }
                 } 
                 out  
               },
               prob = function(modelFit, newdata, submodels = NULL) {
                 out <- as.data.frame(predict(modelFit, newdata, type = "prob"))
                 if(!is.null(submodels))
                 {
                   probs <- out
                   out <- vector(mode = "list", length = length(submodels$threshold)+1)
                   out <- lapply(out, function(x) probs)
                 } 
                 out 
               },
               predictors = function (x, ...) {
                 colnames(x$learn$X)}, 
                 tags = "Prototype Models", 
                 levels = function(x) levels(x$learn$y), 
               sort = function (x){
                 x[order(-x[, 1]), ] 
               }  )


glmDist <- list(label = "Generalized Linear Model", 
          library = NULL, 
           type = "Classification", 
         parameters = data.frame(parameter = c("threshold"), 
                        class = c( "numeric"), 
                        label = c("Probability cutoff")),
          grid = function(x, y, len = NULL){
           data.frame(threshold = seq(0.01, 0.99, length = len))
          },
          fit = function (x, y, wts, param, lev, last, classProbs, ...){
                            dat <- x
                            dat$.outcome <- y
                            if (length(levels(y)) > 2) 
                             stop("glm models can only use 2-class outcomes")
                            theDots <- list(...)
                              if (!any(names(theDots) == "family")) {
                                    theDots$family <- if (is.factor(y)) 
                                        binomial()
                                      else gaussian()
                            }
                              if (!is.null(wts)) 
                                      theDots$weights <- wts
                                        modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
                                                          data = dat), theDots)
                                        out <- do.call("glm", modelArgs)
                                        out$call <- NULL
                                        out
                              }, 
         predict = function(modelFit, newdata, submodels = NULL) {
           class1Prob <- predict(modelFit, 
                                 newdata, 
                                 type = "response")
           out <- ifelse(class1Prob >= modelFit$tuneValue$threshold,
                         modelFit$obsLevel[1], 
                         modelFit$obsLevel[2])
            out  
         },
         prob = function(modelFit, newdata, submodels = NULL) {
           out <- predict(modelFit, newdata, type = "response")
           out <- cbind(1 - out, out)
           dimnames(out)[[2]] <- modelFit$obsLevel
           out
         },
         varImp = function (object, ...) {
                            values <- summary(object)$coef
                            varImps <- abs(values[-1, grep("value$", colnames(values))])
                            out <- data.frame(varImps)
                            colnames(out) <- "Overall"
                                if (!is.null(names(varImps))) 
                                        rownames(out) <- names(varImps)
                                        out
                                      }, 
          predictors = function (x, ...) 
                                predictors(x$terms), 
              levels = function (x) if (any(names(x) == "obsLevels")) x$obsLevels else NULL, 
              tags = c("Generalized Linear Model", "Linear Classifier"), 
              sort = function (x) x)


