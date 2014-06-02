# Generate some good fake EWS data
#library(devtools)
#install_github("jknowles/datasynthR")

library("datasynthR")
set.seed(14155)
rholist <- c(0.6, 0.7, 0.75, 0.54, 0.45, 0.5, 0.01, 0.6, 0.8, 0.2, 
             0.4, 0.5, 0.6, 0.4, 0.8)

distlist <- c("norm", "norm", "norm", "pois", "pois", "pois", 
              "norm", "negbinom", "binom", "norm", "pois", 
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

y <- genBinomialDV(fulldat, form=modterms, intercept=9)
fulldat <- cbind(y, fulldat)
table(fulldat$y)

fulldat$y <- ifelse(fulldat$y == 0, "Non.Grad", "Grad")
fulldat$y <- factor(fulldat$y)


fulldat[, 2] <- (180 - (qpois(pnorm(fulldat[, 2]), lambda = 10))) /180
fulldat[, 5] <- (30 - fulldat[, 5]) / 30
fulldat[, 6] <- scales::rescale(fulldat[, 6], to = c(-3, 4))+3
fulldat[, 6] <- ifelse(fulldat[, 6] >=4, 4, fulldat[, 6])
fulldat[, 7] <- scales::rescale(fulldat[, 7], to = c(-3, 4))+3
fulldat[, 7] <- ifelse(fulldat[, 7] >=4, 4, fulldat[, 7])
fulldat[, 8] <- scales::rescale(fulldat[, 8], to = c(-3, 4))+3
fulldat[, 8] <- fulldat[, 8] - runif(nrow(fulldat), min = -1.2, max = 0)
fulldat[, 8] <- ifelse(fulldat[, 8] >=5, 5, fulldat[, 8])
fulldat[, 11] <- (180 - qpois(pnorm(fulldat[, 11]), lambda = 20) + 8) / 188
fulldat[, 14] <- logb(fulldat[, 14]+1, base = .9) * -1
fulldat[, 14] <- scales::rescale(fulldat[, 14], to = c(-10, 25))
fulldat[, 14] <- ifelse(fulldat[, 14] <=5, 0, fulldat[, 14])
fulldat[, 14] <- round(sqrt(fulldat[, 14]))
fulldat[, 15] <- fulldat[, 15]^2
fulldat[, 15] <- fulldat[, 15] - 5
fulldat[, 15] <- ifelse(fulldat[, 15] <= 0, 0, fulldat[, 15])
fulldat[, 15] <- ifelse(fulldat[, 15] >= 1, fulldat[, 15] + 2, fulldat[, 15])
fulldat[, 16] <- fulldat[, 16] - 10
fulldat[, 16] <- ifelse(fulldat[, 16] <= 0, 0, fulldat[, 16])


# Reorg later
# Need to add sensible ranges to data elements yet
library(caret)
library(EWStools)


modeldat <- assembleData(fulldat, class = "y", predvars = names(fulldat[,2:21]), 
                         p = 0.75)

out <- train(x = modeldat$traindata$preds, 
             y = modeldat$traindata$class, method = "knn", 
             trControl = trainControl(classProbs = TRUE, 
                                      summaryFunction = twoClassSummary), 
             metric = "ROC")



save(fulldat, modeldat, file = "data/EWStestData.rda")
