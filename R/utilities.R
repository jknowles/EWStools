##' @description Create a subset of caret methods in an effort to maximize 
##' dissimilarity between them along the features identified in the caretTags 
##' dataset
##' @param methods a vector of character representing method names in caret
##' @param k the number of clusters
##' @param distancemethod the method used to calculate the dissimilarity
##' @param n  number methods to sample per cluster
##' @param what what to return, either "tags" for the caretTags data or "matrix" 
##' for the dissimilarity matrix
##' @export
##' @return Returns either a dataframe or a matrix with the dissimilarity values 
##' for the methods sampled and the names of the methods. 
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
#' @example
#' data(caretTags)
#' dissimMethod(sample(caretTags$method, 30), k = 4, distancemethod = "maximum", 
#' n = 2, what = "tags") 



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
