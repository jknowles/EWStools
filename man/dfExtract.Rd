\name{dfExtract}
\alias{dfExtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dfExtract(mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
%%     ~~Describe \code{mod} here~~
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
function (mod) 
{
    if (class(mod$summaryTr) != "NULL") {
        newdatB <- data.frame(sens = smooth(mod$summaryTr@rocobj)$sensitivities, 
            spec = smooth(mod$summaryTr@rocobj)$specificities, 
            grp = "train", auc = mod$summaryTr@auc, method = mod$method, 
            elapsedTime = ifelse(is.null(mod$time), NA, mod$time), 
            check.rows = FALSE, row.names = NULL)
    }
    if (class(mod$summaryTe) != "NULL") {
        newdatA <- data.frame(sens = smooth(mod$summaryTe@rocobj)$sensitivities, 
            spec = smooth(mod$summaryTe@rocobj)$specificities, 
            grp = "test", auc = mod$summaryTe@auc, method = mod$method, 
            elapsedTime = ifelse(is.null(mod$time), NA, mod$time), 
            check.rows = FALSE, row.names = NULL)
    }
    if (class(mod$summaryTr) != "NULL" & class(mod$summaryTe) != 
        "NULL") {
        tmp <- rbind(newdatA, newdatB)
    }
    else if (class(mod$summaryTr) != "NULL") {
        tmp <- newdatB
    }
    else if (class(mod$summaryTe) != "NULL") {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
