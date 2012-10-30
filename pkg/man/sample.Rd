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
    \item{\code{x}}{a numeric vector discribing coordinates on x axis (usually time)}
    \item{\code{y}}{a numeric vector discribing coordinates on y axis (structuring variable)}
    \item{\code{z}}{a numeric vector giving the number or density of individuals/counts at time x and structuring element value y}
  }
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(sample)
STdiag(data=sample)
}
\keyword{datasets}
