################################################################################
# Model Search
################################################################################


# For use in lists when ensembling models, stores the fit characteristics, but 
# not necessarily the model
modAcc <- function(fit, datatype = c("test", "train"), testdata, modelKeep = FALSE){
  if (length(datatype) > 1){
    ROCtr <- ROCtest(fit)
    ROCte <-  ROCtest(fit, testdata=list(preds = testdata$preds, 
                                          class = testdata$class))    
  } else if(length(datatype) < 2 & datatype=="test"){
    ROCte <- ROCtest(fit, testdata=list(preds = testdata$preds, 
                                        class = testdata$class))
    ROCtr <- NULL
  } else if (length(datatype) < 2 & datatype=="train"){
    ROCtr <- ROCtest.train(fit)
    ROCte <- NULL
  }
  if(modelKeep == TRUE){
    return(list(model=fit, summaryTr = ROCtr, summaryTe = ROCte, method=fit$method, 
                time = fit$times$everything[3]))
  } else if(modelKeep == FALSE){
    return(list(method=fit$method, summaryTr = ROCtr, summaryTe = ROCte, 
                time = fit$times$everything[3]))
  }
  
}

# enables easy ggplot2 performance curves
dfExtract <- function(mod){
  if(class(mod$summaryTr) != "NULL"){
      newdatB <- data.frame(sens = smooth(mod$summaryTr@rocobj)$sensitivities, 
                            spec = smooth(mod$summaryTr@rocobj)$specificities, 
                            grp="train", 
                            auc = mod$summaryTr@auc,
                            method = mod$method, 
                            elapsedTime = ifelse(is.null(mod$time), NA, mod$time), 
                            check.rows=FALSE, 
                            row.names=NULL)
      }
    if(class(mod$summaryTe) != "NULL"){
      newdatA <- data.frame(sens = smooth(mod$summaryTe@rocobj)$sensitivities, 
                            spec = smooth(mod$summaryTe@rocobj)$specificities, 
                            grp="test",
                            auc  = mod$summaryTe@auc,
                            method = mod$method, 
                            elapsedTime =ifelse(is.null(mod$time), NA, mod$time), 
                            check.rows=FALSE, 
                            row.names=NULL)
      }    
    if(class(mod$summaryTr) != "NULL" & class(mod$summaryTe) != "NULL"){
      tmp <- rbind(newdatA, newdatB)
    } else if(class(mod$summaryTr) != "NULL"){
      tmp <- newdatB
    } else if(class(mod$summaryTe) != "NULL"){
      tmp <- newdatA
    }
    tmp$sens <- as.numeric(tmp$sens)
    tmp$spec <- as.numeric(tmp$spec)
    tmp$auc <- as.numeric(tmp$auc)
    tmp$method <- as.character(tmp$method)
    tmp$auc <- as.numeric(tmp$auc)
    tmp$grp <- as.character(tmp$grp)
    tmp$elapsedTime <- as.numeric(tmp$elapsedTime)
    return(tmp)
}




