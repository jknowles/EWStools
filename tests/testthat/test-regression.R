# Tests for EWStools for regression


# Build data

library(mlbench)
data(BostonHousing)

#
dat <- assembleData(BostonHousing, class = "medv", p = 0.8)

length(dat) == 2

names(dat) == c("traindata", "testdata")

# Split works

rpartFit <- train(x = dat$traindata$preds, y = dat$traindata$class,
                  "rpart",
                  tuneLength = 9)


# Simple model search

modAcc(rpartFit)

list(model = fit, summaryTr = )
list(model=fit, summaryTr = ROCtr, summaryTe = ROCte, method=fit$method, 
            time = fit$times$everything[3]))


RMSEtest.train(rpartFit)
RMSEtest.train(rpartFit, testdata = dat$testdata)
RMSEtest(rpartFit)
RMSEtest(rpartFit, testdata = dat$testdata)

modAcc(rpartFit, datatype = c("train", "test"), testdata = dat$testdata)

ctrl <- trainControl(method = "cv", repeats = 5)

modTest(method = "rpart", datatype = "train", traindata = dat$traindata, 
        modelKeep = FALSE, length = 12, fitControl = ctrl, metric = "RMSE")

