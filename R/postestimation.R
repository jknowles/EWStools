
##' @title Calculate a confidence interval for a vector
##' @description Given a vector, this function produces a three number summary consisting 
##' of the median, the median minus the standard deviation (multiplied by a scaling 
##' factor), and the median plus the standard deviation multiplied by a scaling factor. 
##' @param x a numeric vector, can contain missing values, if so, they are ignored
##' @param scale the factor to multiply the standard deviation by when creating the 
##' interval
##' @return a numeric vector with three elements, the low end of the confidence 
##' interval, the median, and the high end of the confidence interval
##' @export
ci <- function(x, scale){
  lowCI <- median(x, na.rm=T) - (scale * sd(x, na.rm=T))
  hiCI <- median(x, na.rm=T) + (scale * sd(x, na.rm=T))
  med <- median(x, na.rm=T)
  CI <- c(lowCI, med, hiCI)
  if(hiCI > max(x)){
    warning("High value of confidence interval greater than max(x), check scale")
  }
  if(lowCI < min(x)){
    warning("Low value of confidence interval less than min(x), check scale")
  }
  return(CI)
}


##' @title Calculate subscores from an overall score in a dataframe
##' @description Given a dataframe, specify a variable to be the overall risk field and 
##' loop through all other fields and score them on whether the observation is closest to 
##' the others on the overall risk 
##' @param df a dataframe
##' @param VAR a character representing the name of a variable in DF
##' @return RISK a character representing the name of a variable in DF
##' @export
subscores <- function(df, VAR, RISK){
  cis <- tapply(df[, match(VAR, colnames(df))], 
                df[, match(RISK, colnames(df))], 
                function(x){(ci(x, 0.5))}, simplify=TRUE)
  
  thresh <- expand.grid(group = c("High", "Moderate", "Low"), 
                        min = rep(NA), med = rep(NA),
                        max = rep(NA) )
  
  thresh[thresh$group=="High", c(2:4)] <- cis$High
  thresh[thresh$group=="Moderate", c(2:4)] <- cis$Moderate
  thresh[thresh$group=="Low", c(2:4)] <- cis$Low
  
  df$subscore <- NA
  df$subscore[df[, match(VAR, colnames(df))] > thresh[thresh$group=="Moderate", "max"]] <- "Low"
  
  df$subscore[df[, match(VAR, colnames(df))] <= thresh[thresh$group=="Moderate", "max"] & 
                df[, match(VAR, colnames(df))] >= thresh[thresh$group=="Low", "max"]] <- "Moderate"
  
  df$subscore[df[, match(VAR, colnames(df))] < thresh[thresh$group=="Low", "max"]] <- "High"
  
  df$subscore[is.na(df[, match(VAR, colnames(df))])] <- "Missing"
  
  subscore <- list(subvec = df$subscore, thresholds = thresh)
  
  return(subscore)
}
