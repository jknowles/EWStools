################################################################################
# Model Search
################################################################################



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


dfExtract <- function(mod){
  if(class(mod$summaryTr) != "NULL"){
      newdatB <- data.frame(sens = smooth(mod$summaryTr@rocobj)$sensitivities, 
                            spec = smooth(mod$summaryTr@rocobj)$specificities, 
                            grp="train", 
                            auc = mod$summaryTr@auc,
                            method = mod$method, 
                            elapsedTime = mod$time, check.rows=FALSE, 
                            row.names=NULL)
      }
    if(class(mod$summaryTe) != "NULL"){
      newdatA <- data.frame(sens = smooth(mod$summaryTe@rocobj)$sensitivities, 
                            spec = smooth(mod$summaryTe@rocobj)$specificities, 
                            grp="test",
                            auc  = mod$summaryTe@auc,
                            method = mod$method, 
                            elapsedTime = mod$time, check.rows=FALSE, 
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



modSearch <- function(x, datatype=c("train", "test"), modelKeep=NULL, 
                      length = LENGTH, timeout = NULL){
  require(R.utils)
  datD <- c("rda", "lda2", "hda", 'mlp', 'mlpWeightDecay', 'rbf', 'rpart2', 
            "treebag", 'rf', 'plr', 'lda', 'xyf')
  if(x %in% datD){
    fit <- tryCatch({
      evalWithTimeout({
        train(trainData[, -9], trainCLASS,
              method=x,
              trControl=fitControl,
              tuneLength = length, metric="ROC")
      }, timeout = timeout, elapsed = timeout)}, 
      TimeoutException = function(ex) {
        print("Timeout. Skip");
      }, error = function(e) print(paste0("Failure of model: ", x, 
                                          "\n", " For: ",e)))
  } else {
    fit <- tryCatch({
      evalWithTimeout({
        train(trainData, trainCLASS,
              method=x,
              trControl=fitControl,
              tuneLength = length, metric="ROC")
      }, timeout = timeout, elapsed = timeout)}, 
      TimeoutException = function(ex) {
        print("Timeout. Skip");
      }, error = function(e) print(paste0("Failure of model: ", x, 
                                          "\n", " For: ", e)))
  }
  if(class(fit) == "character"){
    cat(fit)
    
  } else if(class(fit) == "train"){
    if (length(datatype) > 1){
      ROCtr <- ROCtest.train(fit)
      ROCte <-  ROCtest.train(fit, testdata=list(testData = testData, testCLASS = testCLASS), 
                              best.method="closest.topleft", 
                              best.weights=c(10, .11))    
    } else if(length(datatype) < 2 & datatype=="test"){
      ROCte <- ROCtest.train(fit, testdata=list(testData = testData, testCLASS = testCLASS), 
                             best.method="closest.topleft", 
                             best.weights=c(10, .11))
      ROCtr <- NULL
    } else if (length(datatype) < 2 & datatype=="train"){
      ROCtr <- ROCtest.train(fit)
      ROCte <- NULL
    }
    if(modelKeep == TRUE){
      return(list(model=fit, summaryTr=ROCtr, summaryTe = ROCte, method=fit$method, 
                  time = fit$times$everything[3]))
    } else if(modelKeep == FALSE){
      return(list(method=fit$method, summaryTr=ROCtr, summaryTe = ROCte, 
                  time = fit$times$everything[3]))
    }
    
  }
}


# Search loop

# 
# ModelFits <- expand.grid(sens = NA, spec = NA, grp = NA, auc = NA, 
#                          method = rep(candidatemods, each = 1028), 
#                          elapsedTime = NA)
# 
# # Class variables correctly to avoid errors
# ModelFits$grp <- as.character(ModelFits$grp)
# ModelFits$method <- as.character(ModelFits$method)
# ModelFits$sens <- as.numeric(ModelFits$sens)
# ModelFits$spec <- as.numeric(ModelFits$spec)
# ModelFits$auc <- as.numeric(ModelFits$auc)
# 
# # set up a progress bar
# pb <- txtProgressBar(min = 0, max = length(candidatemods), style = 3)
# 
# # Optional changes to parameters
# # TIMEOUT <- 600
# # CORES <- 8
# 
# # Loop to test models and capture results
# for(i in candidatemods){
#   p <- match(i, candidatemods)
#   fit <- try(modSearch(i, datatype=c("test","train"), modelKeep=FALSE, 
#                        length = LENGTH, timeout = TIMEOUT))
#   tmp <- tryCatch(dfExtract(fit), error = function(e) "No Model Ran")
#   #
#   if(class(tmp) == "data.frame"){
#     ModelFits[ModelFits$method == i,] <- tmp[tmp$method == i,]
#   } else{
#     ModelFits <- ModelFits
#     print(paste(tmp, "failure for model type:", i, sep=" "))
#   }
#   setTxtProgressBar(pb, p)
# }



# Modsearch setup

# #MODE <- "PROD"
# MODE <- "DEV"
# CORES <- 10
# myOS <- Sys.info()['sysname']
# 
# # Set up parallel processing for speed
# if(myOS!="Windows"){
#   library(doMC)
#   if(MODE=="DEV"){
#     CORES <- CORES / 2
#     registerDoMC(CORES)
#   } else{
#     registerDoMC(CORES)
#   }
# } else {
#   message("Not running in parallel /n I pity you and your single thread...")
#   CORES <- 1
# }
# 
# # Sample sizes for training sets
# if(MODE == "PROD"){
#   SAMPLE <- TRUE
#   N <- 55000
#   TIMEOUT <- 15000
# } else {
#   TIMEOUT <- 20
#   SAMPLE <- TRUE
#   N <- 6000
# }
# 
# # CV settings
# folds <- ifelse(MODE == "DEV", 5, 10)
# reps  <- ifelse(MODE == "DEV", 1, 3)
# 
# # Train methods for caret package to estimate the test error
# if(MODE == "PROD"){
#   fitControl <- trainControl(method='cv', number=folds, repeats=reps,
#                              classProbs=TRUE, summaryFunction=twoClassSummary, 
#                              savePredictions = FALSE)
# } else if(MODE == "DEV"){
#   fitControl <- trainControl(method='boot', savePredictions = FALSE, 
#                              classProbs=TRUE)
# }
# 
# # Show the user what they have selected
# print("The following settings have been chosen:")
# print(paste0("Running in ", myOS))
# print(paste0("Mode: ", MODE))
# print(paste0("Timeout: ", TIMEOUT, " seconds"))
# print(paste0("CPU Cores: ", CORES))
# print(paste0("Sample size: ", N))