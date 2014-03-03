\name{ROCit-class}
\Rdversion{1.1}
\docType{class}
\alias{ROCit-class}
\alias{ROCit}

\title{Class \code{"ROCit"}}
\description{
ROCit objects allow the user to store the results of various ROC 
tests for classification models. These objects conveniently summarize 
various classification metrics for models from \code{\link{train}} or 
\code{\link{glm}} objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{ROCtest(model)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{thresh}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{auc}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{confusematrix}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{rarepercent}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{falsepositive}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{rocobj}:}{Object of class \code{"roc"} ~~ }
    \item{\code{modtype}:}{Object of class \code{"character"} ~~ }
    \item{\code{modcall}:}{Object of class \code{"character"} ~~ }
    \item{\code{datatype}:}{Object of class \code{"character"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
The following methods exist:
  \describe{
  \item{\code{summary}:}{Computes and returns a list of summary statistics of the fitted model, the amount of output can be controlled via the \code{print} method, see also \code{\link{summary}}.}

}
}

\references{
%%  ~~put references to the literature/web site here~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("ROCit")
}
\keyword{classes}
\usage{
ROCit(...)
}
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
