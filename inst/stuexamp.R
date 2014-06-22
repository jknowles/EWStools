library(EWStools)

data(EWStestData)

str(fulldat)

# Simple model

simple_glm <- glm(y ~ attendanceTotal, data = fulldat, family = binomial)



# More serious
library(caret)

ctrl <- trainControl(method = "cv", classProbs = TRUE, 
                     summaryFunction = twoClassSummary)



fullModel <- train(x = fulldat[, 2:10], y = fulldat[, 1], 
                   method = "knn", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "ROC", 
                   trControl = ctrl)



# Need to build out data
modeldat2 <- assembleData(fulldat, class = "y", predvars = names(fulldat)[-1], 
             p = 0.6, classification = TRUE, pvalid = 0.15)


fullModel <- train(x = modeldat2$traindata$preds, y = modeldat2$traindata$class, 
                   method = "knn", 
                   preProc = c("center", "scale"), 
                   tuneLength = 8, 
                   metric = "ROC", 
                   trControl = ctrl)


mod.out <- modTest("knn", datatype=c("train", "test"),
                   traindata = modeldat2$traindata,
                   testdata = modeldat2$testdata, 
                   modelKeep=FALSE, length = 6, fitControl = ctrl, 
                   metric = "ROC")

mod.out <- modSearch(methods = c("glm", "knn", "lda2", "svmRadial"), 
                     datatype=c("train", "test"),
                     traindata = modeldat2$traindata,
                     testdata = modeldat2$testdata, 
                     modelKeep=FALSE, length = 5, fitControl = ctrl, 
                     metric = "ROC")

library(eeptools)

ggplot(mod.out[mod.out$grp == "test",], 
       aes(x = 1- spec, y = sens, group = method, color = method)) + 
  geom_line(size = I(1.1)) + 
  theme_dpi() + theme(legend.position = c(.8, .2))





