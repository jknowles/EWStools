shrink_group_effects <- function(mod, group){
  # Extract group coefficients
  myFE <- summary(mod)$coefficients[grep(group, 
                                         row.names(summary(mod)$coefficients)), 1:2]
  rowmatch <- gsub(group, "", row.names(myFE))
  myFE <- as.data.frame(myFE)
  myFE <- cbind(rowmatch, myFE)
  myFE$rowmatch <- as.character(myFE$rowmatch)
  # Calculate group binomial means
  gmeans <- aggregate(mod$model[, 1], 
                      list(group = mod$model[, match(group, colnames(mod$model))]), 
                      mean)
  gsize <- aggregate(mod$model[, 1], 
                     list(group = mod$model[, match(group, colnames(mod$model))]), 
                     length)
  grads <- aggregate(mod$model[, 1], 
                     list(group = mod$model[, match(group, colnames(mod$model))]), 
                     sum)
  
  wghts <- cbind(gmeans, gsize)
  wghts[,3] <- NULL
  wghts <- cbind(wghts, grads)
  wghts[, 4] <- NULL
  names(myFE) <- c("rowmatch", "est", "se")
  names(wghts) <- c("rowmatch", "gradrate", "count", "grads")
  gradmean <- sum(wghts$grads) / sum(wghts$count)
  
  # Generate the probability associated with each observation given size
  wghts$prob <- 1- dbinom(wghts$grads, wghts$count, prob=gradmean)
  
  myFE <- merge(myFE, wghts, by=c("rowmatch"))
  myFE$grandmean <- median(myFE$est)
  
  # Caculate the distance each fixed effect is from the median fixed effect
  myFE$delta <- myFE$est - myFE$grandmean
  
  # Calculate how much it should be shrunk given how unlikely the observed 
  # graduation rate was
  # Shrink the distance from the mean by the sqrt of this
  
  myFE$delta_adj <- myFE$delta * sqrt(1-myFE$prob)
  
  # Create an adjusted coefficient 
  myFE$est_adj <- myFE$delta_adj + myFE$grandmean
  
  myFE$rowmatch2 <- paste0(group, myFE$rowmatch)
  temp1 <- myFE[,match(c("rowmatch2", "est_adj"), names(myFE))]
  temp2 <- as.data.frame(mod$coefficients)
  temp2$new_est <- NA
  
  for(i in 1:length(temp2[,1])){
    var <- row.names(temp2)[i]
    # print(paste(i, "of ", length(temp2[,1])))
    if((row.names(temp2)[i] %in% temp1$rowmatch2)==TRUE)
      temp2$new_est[i] <- temp1[which(temp1$rowmatch2== var),"est_adj"]
    if((row.names(temp2)[i] %in% temp1$rowmatch2)==FALSE)
      temp2$new_est[i] <- temp2$"mod$coefficients"[i]
  }
  
  mod$coefficients[1:length(temp2[,1])] <- temp2$new_est[1:length(temp2[,1])]
  return(mod)
}



estfun2.glm <- function(x, ...)
{
  xmat <- model.matrix(x, data=x$data)
  xmat <- naresid(x$na.action, xmat)
  if(any(alias <- is.na(coef(x)))) xmat <- xmat[, !alias, drop = FALSE]
  wres <- as.vector(residuals(x, "working")) * weights(x, "working")
  dispersion <- if(substr(x$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial")) 1
  else sum(wres^2, na.rm = TRUE)/sum(weights(x, "working"), na.rm = TRUE)
  rval <- wres * xmat / dispersion
  attr(rval, "assign") <- NULL
  attr(rval, "contrasts") <- NULL
  #res <- residuals(x, type = "pearson")
  #if(is.ts(res)) rval <- ts(rval, start = start(res), frequency = frequency(res))
  #if(is.zoo(res)) rval <- zoo(rval, index(res), attr(res, "frequency"))
  return(rval)
}

cl  <- function(fm, cluster) {
  library(sandwich)
  #fm <- SRD$model
  tmp <- model.matrix(fm, data=fm$data)
  #cluster <- "schg8"
  tmp.cls <- fm$data[, match(cluster, colnames(fm$data))]
  cluster <- tmp.cls[row.names(fm$data) %in% row.names(tmp)]
  tmp <- estfun2.glm(fm)
  tmp <- na.omit(tmp) # hack to deal with NAs
  
  M <- length(unique(cluster))   
  N <- length(cluster)              
  K <- fm$rank                   
  dfc <- (M/(M-1))*((N-1)/(N-K-1))
  
  
  uj <- apply(tmp, 2, function(x) tapply(x, cluster, sum, na.rm=TRUE))
  uj[is.na(uj)] <- 0 # temporary hack to deal with NAs
  
  #uj  <- apply(estfun2.glm(fm), 2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc * sandwich2(fm, meat = crossprod(uj)/N)
  return(vcovCL)
}

sandwich2 <- function(x, bread. = bread, meat. = meat, ...)
{
  if(is.function(bread.)) bread. <- bread.(x)
  if(is.function(meat.)) meat. <- meat.(x, ...)
  n <- NROW(estfun2.glm(x))
  return(1/n * (bread. %*% meat. %*% bread.))
}

meat2 <- function(x, adjust = FALSE, ...)
{
  psi <- estfun2.glm(x, ...)
  k <- NCOL(psi)
  n <- NROW(psi)
  rval <- crossprod(as.matrix(psi))/n
  if(adjust) rval <- n/(n-k) * rval
  rownames(rval) <- colnames(rval) <- colnames(psi)
  return(rval)
}


predict.rob <- function(x,clcov,newdata, trans){
  if(missing(newdata)){ newdata <- x$model }
  m.mat <- model.matrix(x, data=newdata)
  m.coef <- x$coef
  # conform
  vcovL <- vcovL[, colnames(vcovL) %in% colnames(m.mat)]
  vcovL <- vcovL[rownames(vcovL) %in% colnames(m.mat),]
  
  m.mat <- m.mat[, colnames(m.mat) %in% colnames(vcovL)]
  m.coef <- m.coef[names(m.coef) %in% colnames(m.mat)]
  #
  fit <- as.vector(m.mat %*% m.coef)
  #se.fit <- sqrt(diag(m.mat%*%clcov%*%t(m.mat)))
  tmp <- m.mat %*% clcov
  library(compiler)
  diagMult <- cmpfun(function(m1, m2) sapply(seq_len(nrow(m1)), 
                                             function(i) m1[i,] %*% m2[,i]))
  
  predvar <-  diagMult(tmp, t(m.mat))
  #   if(trans == "probit"){
  #     fit <- pnorm(fit)
  #     predvar <- pnorm(predvar)
  #   } else {
  #     fit <- fit
  #     predvar <- predvar
  #   }
  return(list(fit=fit, se.fit=predvar))
}


