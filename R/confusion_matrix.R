confuse_mat <- function(mod, thresh, prop = FALSE, testdata){
  if(!missing(testdata)){
    dv <- as.character(mod$formula[[2]])
    mod.mat <- model.matrix(mod, data=testdata)
    testdata <- testdata[match(row.names(mod.mat), row.names(testdata)),]
    rm(mod.mat)
    yhat <- predict(mod, newdata=testdata, type="response")
    testdata$y <- testdata[, match(dv, colnames(mod$data))]
    testdata$yhat <- yhat
    # hack
    testdata$y <- as.numeric(testdata$y) - 1 # force into binary
    tp <- length(testdata$y[testdata$y > 0 & testdata$yhat >= thresh])
    fp <- length(testdata$y[testdata$y < 1 &  testdata$yhat >= thresh])
    fn <- length(testdata$y[testdata$y > 0 &  testdata$yhat <= thresh])
    tn <- length(testdata$y[testdata$y < 1 &  testdata$yhat <= thresh])
    mat <- data.frame("Predicted NC" = c(tn, fn), "Predicted C" = c(fp, tp))
    row.names(mat) <- c("Actual NC", "Actual C")
    ifelse(prop==TRUE, return((mat / length(testdata$y)) * 100), return(mat))
    
  } else if(missing(testdata)){
    if(is.null(mod$y)){
      dv <- as.character(mod$formula[[2]])
      mod$model <- model.matrix(mod, data=mod$data)
      mod$model <- na.omit(mod$model)
      mod$y <- mod$data[match(row.names(mod$model),row.names(mod$data)), 
                    match(dv, colnames(mod$data))]
    }
    tp <- length(mod$y[mod$y > 0 &  mod$fitted.values >= thresh])
    fp <- length(mod$y[mod$y < 1 &  mod$fitted.values >= thresh])
    fn <- length(mod$y[mod$y > 0 &  mod$fitted.values <= thresh])
    tn <- length(mod$y[mod$y < 1 &  mod$fitted.values <= thresh])
    mat <- data.frame("Predicted NC" = c(tn, fn), "Predicted C" = c(fp, tp))
    row.names(mat) <- c("Actual NC", "Actual C")
    ifelse(prop==TRUE, return((mat / length(mod$y)) * 100), return(mat))
  }
}


###################################
# For VGLM objects (S4)
###################################

################################################################################
# Title: A function to create a confusion matrix out of a model object
# x = an S4 object, VGLM
# thresh = a value from 0 to 1 to assign binary responses to
# prop = a logical vector indicating if proportions instead of counts shoud be
#         returned
# Returns a data.frame
################################################################################


confuse_mat.vglm <- function(x, thresh, prop,...){
  tp  <- length(x@y[x@y > 0 & predict(x, type="response") >= thresh])
  fp  <- length(x@y[x@y < 1 & predict(x, type="response") >= thresh])
  fn  <- length(x@y[x@y > 0 & predict(x, type="response") <= thresh])
  tn  <- length(x@y[x@y < 1 & predict(x, type="response") <= thresh])
  mat <- data.frame("Predicted NC" = c(tn, fn), "Predicted C" = c(fp, tp))
  row.names(mat) <- c("Actual NC", "Actual C")
  ifelse(prop==TRUE, return((mat/length(x@y))*100), return(mat))
}

################################
# For TRAIN objects
################################

################################################################################
# Title: A function to create a confusion matrix out of a model object
# x = a train model fit object (from caret)
# preds = a data.frame of the predictors
# thresh = a value from 0 to 1 to assign binary responses to
# prop = a logical vector indicating if proportions instead of counts shoud be
#         returned
# 
# Returns a data.frame
################################################################################

confuse_mat.train <- function(mod, thresh, prop = FALSE, newdata = NULL){
  if(!missing(newdata)){
    mypred <- predict(mod, newdata=newdata$preds, type="prob")[1]
    y <- as.character(newdata$class)
  } else {
    mypred <- predict(mod, type="prob")
    y <- as.character(mod$trainingData$.outcome)
  }
  
  statmod <- function(x) {
    z <- table(as.vector(x))
    names(z)[z == max(z)]
  }
  
  mode <- statmod(y)
  y[y == mode] <- "1"
  y[y != "1"] <- 0
  y <- as.character(y)
  y <- as.numeric(y)
  # normalize preds
  mypred <- mypred[, 1]
  tp <- length(y[y > 0 & mypred >= thresh])
  fp <- length(y[y < 1 & mypred >= thresh])
  fn <- length(y[y > 0 & mypred <= thresh])
  tn <- length(y[y < 1 & mypred <= thresh])
  mat <- data.frame("Predicted NC"=c(tn, fn), "Predicted C"=c(fp, tp))
  row.names(mat) <- c("Actual NC", "Actual C")
  ifelse(prop==TRUE, return((mat / length(y)) * 100), return(mat))
}

confuse_mat.train2 <- function(x, thresh){
  x$yn <- as.character(x$obs)
  statmod <- function(x) {
    z <- table(as.vector(x))
    names(z)[z == max(z)]
  }
  mode <- statmod(x$yn)
  x$yn[x$yn == mode] <- "1"
  x$yn[x$yn != "1"] <- 0
  x$yn <- as.character(x$yn)
  x$y <- as.numeric(x$yn)
  
  tp <- length(x$y[x$y > 0 & x[,1] >= thresh])
  fp <- length(x$y[x$y < 1 & x[,1] >= thresh])
  fn <- length(x$y[x$y > 0 & x[,1] <= thresh])
  tn <- length(x$y[x$y < 1 & x[,1] <= thresh])
  mat <- data.frame("Predicted NC"=c(tn, fn), "Predicted C"=c(fp, tp))
  row.names(mat) <- c("Actual NC", "Actual C")
  return(mat)
}
