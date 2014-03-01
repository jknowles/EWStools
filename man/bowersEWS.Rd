\name{bowersEWS}
\alias{bowersEWS}
\docType{data}
\title{
Summary of the Accuracy of Early Warning Indicators
}
\description{
This dataset contains accuracy indicators for 110 separate predictive indicators 
of student dropout. These indicators are drawn from a literature search of over 
30 separate studies in American public schools of early indicators of dropout. 
}
\usage{data(bowersEWS)}
\format{
  A data frame with 110 observations on the following 12 variables.
  \describe{
    \item{\code{id}}{a unique id for each study}
    \item{\code{study}}{a short description of the study and authors}
    \item{\code{year}}{the year the study was published}
    \item{\code{grade}}{the lowest grade in the study}
    \item{\code{grade.1}}{the highest grade in the study}
    \item{\code{dropout_indicator}}{a description of the indicator evaluated in the study}
    \item{\code{sample_size}}{a numeric vector representing the number of cases in the study}
    \item{\code{dropout_rate}}{the rate of dropout in the study population}
    \item{\code{prediction}}{the predictive accuracy of the dropout indicator}
    \item{\code{sensitivity}}{the sensitivity of the dropout indicator}
    \item{\code{specificity}}{the specificity of the dropout indicator}
    \item{\code{false.positives_proportion}}{the proportion of false positives from the indicator}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Bowers, A.J., Sprott, R., Taff, S.A. (2013) Do we Know Who Will Drop Out? A Review of 
the Predictors of Dropping out of High School: Precision, Sensitivity and Specificity. The 
High School Journal, 96(2), 77-100. \url{doi:10.1353/hsj.2013.0000} (Preprint available.)
}
\references{

}
\examples{
data(bowersEWS)
plot( 1-bowersEWS$specificity, bowersEWS$sensitivity, pch = 16, xlab="Spec", ylab = "Sens", main = "Sensitivity and Specificity", xlim = c(0, 1)); abline(0, 1, lty=3)
}
\keyword{datasets}
