# Simulate student data

library(devtools)
#install_github("jknowles/datasynthR")
library(datasynthR)
library(eeptools)
library(arm)
getData <- function(N, luck, treatments){
  
  if(!missing(luck)){
    degree <- luck
  } else if(missing(luck)){
    degree <- sample(c(-0.5, 0, 0.5), 1)
  }
  
  #degree <- 0.5
  struc <- list(names=c("read", "math", "luck", "attend", "discipline", "mobility"),
                dist = c("norm", "norm", "norm", "gamma", "pois", "pois"), 
                rho = c(0.9, 0.75, degree, 0.6, -0.7, 0.45))
  studat <- genNumeric(N, pattern=struc)
  
  # consider paramaterizing the rho here:
  # e.g. RHO2 <- runif(1, -0.5, 0.5)
  # myFactors <- genFactor(N, 4, nlevel=4, rho=RHO2)
  myFactors <- genFactor(N, 4, nlevel=4, rho=0.3)
  names(myFactors) <- c("econ", "race", "sped", "ell")
  studat <- cbind(studat, myFactors)
  rm(myFactors)

  
  myF <- list(vars=c("read", "math", "luck", "attend", "discipline", "mobility", "econ"), 
              coefs=c(0.4, 0.4, 0.7, 0.3, -0.75, -0.5, -0.8))
  
  mdf <- subset(studat, select=myF$vars)
  mdf <- as.data.frame(sapply(mdf, rescale))
  studat$treat_pr <- genBinomialDV(mdf, form=myF, intercept=-8, type="response")
  weighted_binom <- function(x) rbinom(1, 1, x)
  studat$treat <- sapply(studat$treat_pr, weighted_binom)
  studat$treat_pr <- NULL
  rm(myF, mdf)

  # Get schools
  schools <- genFactor(N, 1, 200, 0.1, seed=studat$seed, keepSeed=FALSE)
  studat$schools <- schools[,1]
  names(studat)[ncol(studat)] <- "schools"
  studat$ever_treat <- studat$treat
  studat$seed <- NULL; rm(schools)
  
  #
  studat$discipline <- scale(studat$discipline)
  studat$discipline[studat$discipline <0] <- 0
  studat$discipline <- round(studat$discipline, digits=0)
  ###############################################
  # Discipline variable needs more variability!
  studat$mobility <- scale(studat$mobility)
  studat$mobility[studat$mobility <0] <- 0
  studat$mobility <- round(studat$mobility, digits=0)
  
  sids <- N:(6*N)
  studat$sid <- sample(sids, nrow(studat), replace=FALSE)
  studat$year <- 2
  row.names(studat) <- NULL
  mtmp <- studat
  mtmp$year <- 1
  mtmp$treat <- 0
  row.names(mtmp) <- (N+1):(2*N)
  studat <- rbind(studat, mtmp)
  mtmp$year <- 3
  mtmp$treat <- 0
  studat <- rbind(studat, mtmp)
  rm(mtmp, sids); gc()
  
  
  growthG <- function(x) x + runif(1, -0.9, 0.9)
  
  studat$attend2 <- studat$attend
  studat$attend2[studat$year==2] <- sapply(studat$attend2[studat$year==2], growthG)
  studat$attend2[studat$year==3] <- sapply(studat$attend2[studat$year==3], growthG)
  
  growth <- function(x) x + rnorm(1, 0.1, 1)
  
  
  studat$read2 <- studat$read
  studat$read2[studat$year==2] <- sapply(studat$read2[studat$year==2], growth)
  studat$read2[studat$year==3] <- sapply(studat$read2[studat$year==3], growth)
  
  
  studat$math2 <- studat$math
  studat$math2[studat$year==2] <- sapply(studat$math2[studat$year==2], growth)
  studat$math2[studat$year==3] <- sapply(studat$math2[studat$year==3], growth)
  
  growth2 <- function(x) rpois(1, x)
  
  studat$discipline2 <- studat$discipline
  studat$discipline2[studat$year==2] <- sapply(studat$discipline2[studat$year==2], growth2)
  studat$discipline2[studat$year==3] <- sapply(studat$discipline2[studat$year==3], growth2)
  
  studat$mobility2 <- studat$mobility
  studat$mobility2[studat$year==2] <- sapply(studat$mobility2[studat$year==2], growth2)
  studat$mobility2[studat$year==3] <- sapply(studat$mobility2[studat$year==3], growth2)
  
  
  ################################################################################
  # Treatment
  
  if(!missing(treatments)){
    trt <- treatments
  } else if (missing(treatments)){
    trt <- list(mathM = rnorm(1, 0.1, 0.01), mathSD = runif(1, .001, .1), 
                readM = rnorm(1, 0.08, 0.01), readSD = runif(1, .001, .1))
  }
  
  fuzzy <- function(x) x + rnorm(1,trt$readM, trt$readSD)
  
  studat$read2 <- studat$read
  studat$read2[studat$ever_treat==1 & studat$year==3] <- 
    sapply(studat$read2[studat$ever_treat==1 & studat$year==3],fuzzy)
  
  fuzzy <- function(x) x + rnorm(1,trt$mathM, trt$mathSD)
  
  studat$math2 <- studat$math
  studat$math2[studat$ever_treat==1 & studat$year==3] <- 
    sapply(studat$math2[studat$ever_treat==1 & studat$year==3],fuzzy)
  
  studat$math <- studat$math2; studat$math2 <- NULL
  studat$read <- studat$read2; studat$read2 <- NULL
  studat$attend <- studat$attend2; studat$attend2 <- NULL
  studat$mobility <- studat$mobility2; studat$mobility2 <- NULL
  studat$discipline <- studat$discipline2; studat$discipline2 <- NULL
  
  studat$attend <- studat$attend + 0.3
  studat$attend[studat$attend < 0] <- 0
  studat$attend[studat$attend > 1] <- 1
  
  if(!exists("mag")){
    bias <- NA
  } else if(exists("mag")){
    bias <- list("correlation" = degree, "coefficient" = mag)
  }
  
  Monte <- list(data=studat, pattern = struc, treatment = trt, bias=bias)
  
  return(Monte)
}


test <- getData(100)
