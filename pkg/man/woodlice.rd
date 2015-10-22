\name{woodlice}
\alias{woodlice}
\docType{data}
\title{
woodlice - data frame
}
\description{
A sample data frame with data on a woodlice population.
}
\usage{data(woodlice)}
\format{
  A data frame with 274 lines and 3 variables.
  \describe{
    \item{\code{Date}}{the date of observation.}
    \item{\code{HeadWidth}}{the size of the head as a measure of the individual size.}
    \item{\code{PercentOfTotal}}{the number of individual in a given class as a percentage of total population.}
  }
}
\examples{
data(woodlice)
STdiag(PercentOfTotal~Date*HeadWidth,woodlice,log=TRUE)
}
\keyword{datasets}
