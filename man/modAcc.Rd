\name{modAcc}
\alias{modAcc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
modAcc(fit, datatype = c("test", "train"), testdata, modelKeep = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{datatype}{
%%     ~~Describe \code{datatype} here~~
}
  \item{testdata}{
%%     ~~Describe \code{testdata} here~~
}
  \item{modelKeep}{
%%     ~~Describe \code{modelKeep} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fit, datatype = c("test", "train"), testdata, modelKeep = FALSE) 
{
    if (length(datatype) > 1) {
        ROCtr <- ROCtest(fit)
        ROCte <- ROCtest(fit, testdata = list(preds = testdata$preds, 
            class = testdata$class))
    }
    else if (length(datatype) < 2 & datatype == "test") {
        ROCte <- ROCtest(fit, testdata = list(preds = testdata$preds, 
            class = testdata$class))
        ROCtr <- NULL
    }
    else if (length(datatype) < 2 & datatype == "train") {
        ROCtr <- ROCtest.train(fit)
        ROCte <- NULL
    }
    if (modelKeep == TRUE) {
        return(list(model = fit, summaryTr = ROCtr, summaryTe = ROCte, 
            method = fit$method, time = fit$times$everything[3]))
    }
    else if (modelKeep == FALSE) {
        return(list(method = fit$method, summaryTr = ROCtr, summaryTe = ROCte, 
            time = fit$times$everything[3]))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
