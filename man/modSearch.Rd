\name{modSearch}
\alias{modSearch}
\title{Generate an empty dataframe to match \code{\link{modAcc}} lists}
\usage{
modSearch(methods, timeout = NULL, ...)
}
\arguments{
  \item{methods}{a list of \code{train} method names to
  generate the dataframe for}

  \item{timeout}{an integer representing when the function
  should quit on a method and move on}

  \item{...}{additional arguments passed to
  \code{\link{modSearch}}}
}
\value{
a \code{\link{data.frame}} with the following columns:
\itemize{ \item{sens - the sensitivities of the model at
various thresholds} \item{spec - the specificities of the
model at various thresholds} \item{grp - whether the model
is using training or test data} \item{auc - the area under
the curve} \item{method - the model method}
\item{elapsedTime - the time reported for the model to run}
}
}
\description{
Used for generating the data to make good looking ROC
curves of training and test data.
}
\note{
Timeout does not work for all model types. See
documentation on \code{\link{evalWithTimeout}} for details.
Importantly it does not work for methods that call
underlying C code.

The sensitivities and specificities come from the
\code{\link{roc}} object stored in the
\code{\linkS4class{ROCit}} object
}

