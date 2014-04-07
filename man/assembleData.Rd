\name{assembleData}
\alias{assembleData}
\title{Assemble train and test data for model building with EWStools}
\usage{
assembleData(data, class, predvars, p, ...)
}
\arguments{
  \item{data}{a dataframe that the user would like to split
  into training and sample sets}

  \item{class}{character value of the name of the dependent
  variable}

  \item{predvars}{a character vector of the names of
  predictor variables}

  \item{p}{the proportion of data to be placed into a
  training set}
}
\value{
A list of lists with the following items: \itemize{
\item{traindata - a list with a dataframe of the predictor
matrix called preds, and the class called class}
\item{testdata - a list with a dataframe of the predictor
matrix called preds, and the class called class} }
}
\description{
Assemble train and test data for model building with
EWStools
}
\note{
Built on the \code{\link{createDataPartition}} function in
the \code{caret} package.
}

