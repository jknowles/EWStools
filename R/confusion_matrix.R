
##' @title An internal function for creating the confusion matrix for \code{\linkS4class{ROCit}} 
##' objects
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


##' @title An internal function for creating the confusion matrix for \code{\linkS4class{ROCit}} 
##' objects from train
confuse_mat.train <- function(x, thresh){
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
