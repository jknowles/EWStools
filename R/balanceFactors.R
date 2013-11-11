
factor_norm <- function(mod, testdata, impute=FALSE){
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
      if(length(chk[is.na(chk)]) > 0){
        testdata[, i] <- as.character(testdata[, i])
        id <- which(!(testdata[, i] %in% c(mod$xlevels[i])))
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
