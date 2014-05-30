##' @title Calculate subscores from a dataframe of overall scores
##' @description Create a new dataframe of subscores for individual observations 
##' across predictor domains
##' @param df a vector of character representing method names in caret
##' @param VAR the number of clusters
##' @param RISK the method used to calculate the dissimilarity
##' @return Returns either a dataframe or a matrix with the dissimilarity values 
##' for the methods sampled and the names of the methods. 
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
