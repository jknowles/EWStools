################################################################################
# Build data
################################################################################

##' @title Split data into a training and a test dataset
##' @param data a dataframe that the user would like to split into training and sample sets
##' @param class character value of the name of the dependent variable 
##' @param p the proportion of data to be placed into a training set 
##' @return A list with the following items:
##' \itemize{
##' \item{train - a data frame of the training values}
##' \item{test - a data frame of the test values}
##' \item{indexes - the row indexes of the original data frame selected to be in the training set}
##' } 
##' @note Built on the \code{\link{createDataPartition}} function in the \code{caret} package.
splitData <- function(data, class, p){
  idx <- createDataPartition(data[, class], 1, p = p)
  train <- data[idx[[1]], ]
  test <- data[-idx[[1]], ]
  return(list(train = train, test = test, indexes = idx))
}


buildModelMatrix <- function(data, predvars, na.omit = TRUE){
  if(na.omit == TRUE){
    data <- na.omit(data[, predvars])
  } else {
    data <- data[, predvars]
  }
  
  FORM <-  paste0("~ 0 + ", paste0(predvars, collapse = " + "))
  FORM <- as.formula(FORM)
  out <- model.matrix(FORM, data = data)
  return(out)
}



###########################################


# Subset predictors
predsTRAIN <- train[,c('ontime_grad2','math8','read8','race','frpl2','gender',
                       'disab_code', 'disflag_g8', 'disdays_g8', 'disab_flag',
                       'ell_comp_med2','att_rate_g8','mobility_dist_yr8',
                       'attendance', 'attend_var', 'mean_mathG8', 'sd_mathG8', 
                       'mean_readG8', 'sd_read8', 'cohort_size2', 'per_nonwhite', 
                       'per_ell', 'per_frpl')]

predsTRAIN <- na.omit(predsTRAIN)

predsTEST <- test[,c('ontime_grad2','math8','read8','race','frpl2','gender',
                     'disab_code', 'disflag_g8','disdays_g8', 'disab_flag',
                     'ell_comp_med2','att_rate_g8','mobility_dist_yr8',
                     'attendance', 'attend_var', 'mean_mathG8', 'sd_mathG8', 
                     'mean_readG8', 'sd_read8', 'cohort_size2', 'per_nonwhite', 
                     'per_ell', 'per_frpl')]

predsTEST <- na.omit(predsTEST)

# Sample data if necessary
if(SAMPLE==TRUE){
  sampsize <- N
  tmp2 <- predsTRAIN[sample(nrow(predsTRAIN), sampsize), ,drop=FALSE]
  tmp <- predsTRAIN[predsTRAIN$ontime_grad2 == "Non.grad", ]
  tmp <- tmp[sample(nrow(tmp), .5 * N), , drop=FALSE]
  predsTRAIN <- rbind(tmp2, tmp)
  predsTRAIN <- predsTRAIN[sample(nrow(predsTRAIN), N), , drop=FALSE]
  #predsTRAIN <- tmp2
  rm(tmp2)
  #rm(tmp2, tmp)
} else{
  
}

# Split into classification and predictors, split into training and test data
trainCLASS <- predsTRAIN$ontime_grad2
trainCLASS <- as.factor(trainCLASS)
testCLASS <- predsTEST$ontime_grad2
testCLASS <- as.factor(testCLASS)

predsTRAIN <- predsTRAIN[, -1]
predsTEST <- predsTEST[, -1]

rm(test, train)


# Build a matrix of predictors for training

myF2 <- formula( ~ 0 + math8 + read8 + I(math8^2) + I(read8^2) + 
                   factor(race) + factor(frpl2) + factor(gender) + 
                   factor(ell_comp_med2) +
                   factor(disflag_g8) + disdays_g8 + factor(disab_flag) +
                   att_rate_g8 + mobility_dist_yr8 + attendance + attend_var + 
                   mean_mathG8 + sd_mathG8 + mean_readG8 + sd_read8 + 
                   I(mean_mathG8^2) + I(mean_readG8^2) + I(sd_mathG8^2) + 
                   I(sd_read8^2) + cohort_size2 + per_nonwhite + per_ell + 
                   per_frpl)

trainData <- model.matrix(myF2, data=predsTRAIN)
testData <- model.matrix(myF2, data=predsTEST)


# Read in candidate models from a list and subset
candidatemods <- mods$method..[mods$EWS.=="Yes"]
modeltests <- vector("list", length(candidatemods))

varnames <- c("math8", "read8", "math8_sq", "read8_sq", "raceA", "raceB", 
              "raceH", "raceI", "raceW", "frpl2Y", "genderM", "ellLO", 
              "ellMED", "ellNAT", "disflagMinor", "disflagNo", "disflagSevere", 
              "disdays_g8", "disab_flagNone", "disab_flagOtherSevere", 
              "att_rate_g8", "mobility_dist_yr8", "attendance", 
              "attend_var", "mean_mathG8", "sd_mathG8", "mean_readG8", 
              "sd_readG8", "mean_mathG8_sq", "mean_readG8_sq", 
              "sd_mathG8_sq", "sd_readG8_sq", "cohort_size", 
              "per_nonwhite", "per_ell", "per_frpl")

colnames(trainData) <- varnames
colnames(testData) <- varnames
rm(predsTEST, predsTRAIN)
# Clean up

print("Data is now prepared...")