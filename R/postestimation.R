
## Create subscores
subscores <- function(df, VAR, RISK){
  ci <- function(x, size){
    # x = var
    # size = number of sds to include in ci
    lowCI <- median(x, na.rm=T) - (size * sd(x, na.rm=T))
    hiCI <- median(x, na.rm=T) + (size * sd(x, na.rm=T))
    med <- median(x, na.rm=T)
    CI <- c(lowCI, med, hiCI)
    return(CI)
  }
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
