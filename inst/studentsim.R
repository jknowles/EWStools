# Generate some good fake EWS data
#library(devtools)
#install_github("jknowles/datasynthR")

library("datasynthR")
set.seed(14155)
rholist <- c(0.6, 0.7, 0.75, 0.54, 0.45, 0.5, 0.01, 0.6, 0.8, 0.2, 
             0.4, 0.5, 0.6, 0.4, 0.8)

distlist <- c("norm", "norm", "norm", "pois", "pois", "pois", 
              "norm", "negbinom", "binom", "pois", "pois", 
              "binom", "negbinom", "negbinom", "pois")

nameslist <- c("attendanceTotal", "assessmentMath", "assessmentRead", 
               "attendance30day", "courseGradesCore", "courseGradesAll", 
               "GPA", "retention", "remedialCourse", "tardies", 
               "majorDiscipline", "minorDiscipline", "officeReferral", 
               "expulsionDays", "suspensionDays")

struc <- list(dist = distlist, 
              rho = rholist,
              names = nameslist)

fulldat <- genNumeric(1500, pattern = struc, na.rm = TRUE, scale = .5)
summary(fulldat)
head(fulldat)
fulldat$seed <- NULL
datFact <- genFactor(1500,  k = 6, rho = 0.4, nlevel = 5, seed = fulldat[,5])
datFact <- datFact[, -1]
names(datFact) <- c("Ethnicity", "FRL", "gender", "ELP", "Mobility","IEP")
levels(datFact$Ethnicity) <- c("AI", "H", "B", "W", "A")
levels(datFact$FRL) <- c("N", "R", "F", "N", "F")
levels(datFact$gender) <- c("M", "M", "F", "M", "F")
levels(datFact$ELP) <- c("Lim", "Nat", "Nat", "Nat", "Lim")
levels(datFact$Mobility) <- c("MoveBoth", "MoveSch", "NoMove", "NoMove", "MoveDist")
levels(datFact$IEP) <- c("Severe", "LD", "None", "None", "None")

fulldat <- cbind(fulldat, datFact)
rm(datFact)

nvars <- 21

modterms <- list(coefs = rnorm(nvars), 
                 vars = sample(names(fulldat), nvars)) 

y <- genBinomialDV(fulldat, form=modterms, intercept=-25)
fulldat <- cbind(y, fulldat)
table(fulldat$y)

fulldat$y <- ifelse(fulldat$y == 0, "Non.Grad", "Grad")
fulldat$y <- factor(fulldat$y)

library(caret)
library(EWStools)

modeldat <- assembleData(fulldat, class = "y", predvars = names(fulldat[,2:21]), 
                         p = 0.75)

out <- train(x = modeldat$traindata$preds, 
             y = modeldat$traindata$class, method = "knn", 
             trControl = trainControl(classProbs = TRUE, 
                                      summaryFunction = twoClassSummary), 
             metric = "ROC")


# Reorg later
# Need to add sensible ranges to data elements yet

save(modeldat, file = "data/EWStestData.rda")
