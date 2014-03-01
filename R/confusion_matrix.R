
confuse_mat.train <- function(model, testdata=NULL){
  if(missing(testdata)){
    cm1 <- confusionMatrix(model$trainingData$.outcome, predict(model))
    return(cm1)
  } else{
    cm1 <- confusionMatrix(testdata$class, predict(model, newdata = testdata$preds))
    return(cm1)
  }
}

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
################################################################################
# Title: A function to calculate the F measure of classification accuracy
# x = an lm or glm model fit object
# returns a named list with precision, recall, false alarm, and the F statistic
# depends on confuse_mat
# Returns a named list
################################################################################

rarecase_f <- function(x,...){
  mat    <- confuse_mat(x,...,prop=FALSE)
  recall <- mat[2, 2]/(mat[2, 2]+mat[2, 1])
  recall <- ifelse(is.finite(recall)==TRUE, recall, 0)
  prec   <- mat[2, 2]/(mat[2, 2]+mat[1, 2])
  prec   <- ifelse(is.finite(prec)==TRUE, prec, 0)
  falsea <- mat[2, 1]/(mat[2, 1]+mat[1, 1])
  falsea <- ifelse(is.finite(falsea)==TRUE,  1-falsea, 0)
  f <- (2*recall*prec)/(prec+recall)
  f <- ifelse(is.finite(f)==TRUE, f, 0)
  results <- list("Recall"=recall, "Precision"=prec, "F-Measure:"=f,
                  "False Alarm:"=falsea)
  return(results)
}

################################################################################
# Title: A function to draw a ROC curve from a model fit object
# x = an lm or glm model fit object
# s = the number of points along the curve to calculate
# Requires ggplot2
################################################################################
ROCplot.train <- function(mod, datatype = NULL, ...){
  if(!missing(datatype)){
    dt <- datatype
  } else{
    dt <- "train"
  }
  test1 <- modAcc(fullModel, datatype=dt, ...)
  plotdf <- dfExtract(test1)
  qplot(1-spec, sens, data=plotdf, geom='line', group=1, se=FALSE) + 
    geom_abline(slope=1, intercept=0) + theme_bw()+ 
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
  
}

# KILL THIS FOR LOOP

roc_plot<-function(x,s,...){
  require(ggplot2)
  dat <- data.frame(xax=rep(NA, s), yax=rep(NA, s), 
                    thresha=seq(0.01,0.99, length.out=s))
  
  for(i in seq(0.01, 0.99, length.out=s)){
    mat <- confuse_mat(x,thresh=i,prop=FALSE,...)
    recall <- mat[2, 2]/(mat[2, 2]+mat[2, 1])
    recall <- ifelse(is.finite(recall)==TRUE, recall, 0)
    falsea <- mat[2, 1] / (mat[2, 1] + mat[1, 1])
    falsea <- ifelse(is.finite(falsea)==TRUE, falsea, 0)
    dat$xax[dat$thresha==i] <- falsea
    dat$yax[dat$thresha==i] <- recall
  }
  qplot(xax, yax, data=dat, colour=thresha, label=round(thresha, digits=2),
        geom='point')+theme_bw()+geom_line()+
    geom_text(hjust=-0.5, angle=-45, size=I(4))+
    geom_abline(aes(intercept=0, slope=1))+
    labs(x="False Alarm", y="Recall", title="ROC Curve", colour="Threshold")+
    coord_cartesian(xlim=c(0, 1), ylim=c(0, 1)) + scale_x_reverse()
}

################################################################################
# Title: A function to draw a classification curve from a model fit object
# x = an lm or glm model fit object
# s = the number of points along the curve to calculate
# Requires ggplot2
################################################################################


classcorrect_plot<-function(x,s,...){
  require(ggplot2)
  dat <- data.frame(xax=rep(NA, s), yax=rep(NA, s), 
                    thresha=seq(0.01, 0.99, length.out=s))
  for(i in seq(0.01, 0.99, length.out=s)){
    mat <- confuse_mat(x, thresh=i, prop=FALSE,...)
    class  <- mat[1, 1] / (mat[1, 2] + mat[1, 1])
    class  <- ifelse(is.finite(class) == TRUE, class, 0)
    falsea <- mat[1, 2] / (mat[1, 2] + mat[1, 1])
    falsea <- ifelse(is.finite(falsea) == TRUE, 1-falsea, 0)
    dat$xax[dat$thresha==i] <- falsea
    dat$yax[dat$thresha==i] <- class
  }
  qplot(factor(round(xax * 100, digits=1)), yax * 100, data=dat, 
        label=round(thresha, digits=2),geom='bar', stat="identity") + 
    theme_bw() + geom_text(vjust = -0.5, angle = -15, size= I(4)) +
    geom_abline(aes(intercept = 50, slope = 0)) +
    labs(x = "False Alarm", y = "Correct Rare Class %",
         title = "Classification Metric", colour = "Threshold") +
    ylim(c(0, 100))
  # return(dat)
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

################################################################################
# Title: A function to calculate the F measure of classification accuracy
# x = an S4 object, VGLM
# returns a named list with precision, recall, false alarm, and the F statistic
# depends on confuse_mat
# Returns a named list
################################################################################

rarecase_f.vglm <- function(x,...){
  mat    <- confuse_mat.vglm(x,..., prop=FALSE)
  recall <- mat[2, 2] / (mat[2, 2] + mat[2, 1])
  recall <- ifelse(is.finite(recall)==TRUE, recall, 0)
  prec   <- mat[2, 2] / (mat[2, 2] + mat[1, 2])
  prec   <- ifelse(is.finite(prec)==TRUE, prec, 0)
  f <- (2 * recall * prec) / (prec + recall)
  f <- ifelse(is.finite(f)==TRUE, f, 0)
  results <- list("Recall"=recall, "Precision"=prec, "F-Measure:"=f)
  return(results)
}

################################################################################
# Title: A function to draw a ROC curve from a model fit object
# x = an S4 object, VGLM
# s = the number of points along the curve to calculate
# Requires ggplot2
################################################################################

roc_plot.vglm <- function(x,s,...){
  require(ggplot2)
  dat <- data.frame(xax=rep(NA, s), yax=rep(NA, s), 
                    thresha=seq(0.01, 0.99, length.out=s))
  for(i in seq(0.01, 0.99, length.out=s)){
    mat    <- confuse_mat.vglm(x, thresh=i, prop=FALSE,...)
    recall <- mat[2, 2] / (mat[2, 2] + mat[2, 1])
    recall <- ifelse(is.finite(recall) == TRUE, recall, 0)
    falsea <- mat[1, 2] / (mat[1, 2] + mat[1, 1])
    falsea <- ifelse(is.finite(falsea) == TRUE, falsea, 0)
    dat$xax[dat$thresha==i] <- falsea
    dat$yax[dat$thresha==i] <- recall
  }
  qplot(xax, yax, data=dat, colour=thresha, label=round(thresha, digits=2),
        geom='point') + theme_bw() + geom_line() + geom_text(hjust=-0.5) +
    geom_abline(aes(intercept=0, slope=1))+
    labs(x="False Alarm", y="Recall", title="ROC Curve", 
         colour="Threshold") + coord_cartesian(xlim=c(0, 1), ylim=c(0, 1))
}


###################################
# For MER objects
###################################

################################################################################
# Title: A function to create a confusion matrix out of a model object
# x = an S4 object, lmer model fit
# thresh = a value from 0 to 1 to assign binary responses to
# prop = a logical vector indicating if proportions instead of counts shoud be
#         returned
# Returns a data.frame
################################################################################

confuse_mat.mer<-function(x,thresh,prop,...){
  tp <- length(x@y[x@y > 0 & fitted(x) >= thresh])
  fp <- length(x@y[x@y<1 & fitted(x)>=thresh])
  fn <- length(x@y[x@y>0 & fitted(x)<=thresh])
  tn <- length(x@y[x@y<1 & fitted(x)<=thresh])
  mat<-data.frame("Predicted NC"=c(tn,fn),"Predicted C"=c(fp,tp))
  row.names(mat)<-c("Actual NC","Actual C")
  ifelse(prop==TRUE,return((mat/length(x@y))*100),return(mat))
}

################################################################################
# Title: A function to calculate the F measure of classification accuracy
# x = an S4 object, lmer model fit
# returns a named list with precision, recall, false alarm, and the F statistic
# depends on confuse_mat
# Returns a named list
################################################################################

rarecase_f.mer <- function(x,...){
  mat    <- confuse_mat.mer(x,..., prop=FALSE)
  recall <- mat[2, 2] / (mat[2, 2] + mat[2, 1])
  recall <- ifelse(is.finite(recall) == TRUE, recall, 0)
  prec   <- mat[2, 2] / (mat[2, 2] + mat[1, 2])
  prec   <- ifelse(is.finite(prec) == TRUE, prec, 0)
  f <- (2 * recall * prec) / (prec + recall)
  f <- ifelse(is.finite(f) == TRUE, f, 0)
  results <- list("Recall" = recall, "Precision" = prec, "F-Measure:" = f)
  return(results)
}

################################################################################
# Title: A function to draw a ROC curve from a model fit object
# x = an S4 object, lmer model fit
# s = the number of points along the curve to calculate
# Requires ggplot2
################################################################################

roc_plot.mer <- function(x, s,...){
  require(ggplot2)
  dat <- data.frame(xax = rep(NA, s), yax = rep(NA, s), 
                    thresha = seq(0.01, 0.99, length.out = s))
  for (i in seq(0.01, 0.99, length.out = s)){
    mat    <- confuse_mat.mer(x, thresh=i, prop=FALSE,...)
    recall <- mat[2, 2] / (mat[2, 2] + mat[2, 1])
    recall <- ifelse(is.finite(recall) == TRUE, recall, 0)
    falsea <- mat[1, 2] / (mat[1, 2] + mat[1, 1])
    falsea <- ifelse(is.finite(falsea) == TRUE, falsea, 0)
    dat$xax[dat$thresha == i] <- falsea
    dat$yax[dat$thresha == i] <- recall
  }
  qplot(xax, yax, data=dat, colour = thresha, label = round(thresha, digits=2),
        geom = 'point') + theme_bw() + geom_line() + geom_text(hjust=-0.5) + 
    geom_abline(aes(intercept = 0, slope = 1)) +
    labs(x = "False Alarm", y = "Recall", title = "ROC Curve", 
         colour = "Threshold")+ coord_cartesian(xlim=c(0, 1),ylim=c(0, 1))
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

confuse_mat.train <- function(model, testdata=NULL){
  if(missing(testdata)){
    cm1 <- confusionMatrix(model$trainingData$.outcome, predict(model))
    return(cm1)
  } else{
    cm1 <- confusionMatrix(testdata$class, predict(model, newdata = testdata$preds))
    return(cm1)
  }
}


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

################################################################################
# Title: A function to calculate the F measure of classification accuracy
# x = a train model fit object (from caret)
# depends on confuse_mat.train
# Returns a named list
################################################################################

rarecase_f.train <- function(x,...){
  mat <- confuse_mat.train(x, ..., prop=FALSE)
  recall <- mat[2, 2] / (mat[2, 2] + mat[2, 1])
  prec <- mat[2, 2] / (mat[2, 2] + mat[1, 2])
  f    <- (2 * recall * prec) / (prec + recall)
  results <- list("Recall" = recall, "Precision" = prec, "F-Measure:" = f)
  return(results)
}

################################################################################
# Title: A function to draw a ROC curve from a model fit object
# x = a train object, caret package
# s = the number of points along the curve to calculate
# Requires ggplot2
################################################################################

roc_plot.train <- function(x, s, ...){
  require(ggplot2)
  dat <- data.frame(xax=rep(NA, s), yax=rep(NA, s), 
                    thresha=seq(0.01, 0.99, length.out=s))
  for (i in seq(0.01, 0.99, length.out=s)){
    mat <- confuse_mat.train(x, thresh=i, ...)
    recall <- mat[2, 2] / (mat[2, 2] + mat[2, 1])
    falsea <- mat[1, 2] / (mat[1, 2] + mat[1, 1])
    dat$xax[dat$thresha == i] <- falsea
    dat$yax[dat$thresha == i] <- recall
  }
  
  qplot(xax, yax, data=dat, colour=thresha, label=round(thresha, digits=2),
        geom='point') + theme_bw() + geom_line() + geom_text(hjust=-0.5) +
    geom_abline(aes(intercept=0, slope=1)) +
    labs(x="False Alarm", y="Recall", title="ROC Curve", colour="Threshold") +
    coord_cartesian(xlim=c(0, 1), ylim=c(0, 1))
}

################################################################################
# Title: A function to test for rank-deficient model matrix
# x = a formula
# df = a dataframe
################################################################################

rankdef_test <- function(x, df,...){
  mm1  <- model.matrix(x, df)
  rank <- qr(mm1)$rank
  n <- ncol(mm1)
  z <- ifelse(rank >= n, "NO", "YES")
  result <- list("model rank"=rank, "mod matrix columns"=n, "rank deficient?"=z)
  return(result)
}


###############################
