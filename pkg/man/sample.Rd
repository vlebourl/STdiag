\name{sample}
\alias{sample}
\docType{data}
\title{
sample - data frame
}
\description{
A sample data frame to demonstrate how to use STdiag, of a size structured population of collembolas during more than 600 days.
}
\usage{data(sample)}
\format{
  A data frame with 6960 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{Time in days (a numeric vector discribing coordinates on x axis, usually time).}
    \item{\code{y}}{Body length in millimeters (a numeric vector discribing coordinates on y axis, the structuring variable).}
    \item{\code{z}}{a numeric vector giving the number or density of individuals/counts at time x and structuring element value y.}
  }
}
\examples{
data(sample)
STdiag(data=sample)
}
\keyword{datasets}
