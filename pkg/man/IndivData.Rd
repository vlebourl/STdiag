\name{IndivData}
\alias{IndivData}
\docType{data}
\title{
IndivData - data frame
}
\description{
A sample data frame with individual data to demonstrate how to use Indiv2DataFrame. Each line in the data frame represents one individual by the date of observation and the measured size.
}
\usage{data(IndivData)}
\format{
  A data frame with 114635 observations on the following 2 variables.
  \describe{
    \item{\code{date}}{the date of observation.}
    \item{\code{size}}{the measured size of the individual.}
  }
}
\examples{
data(IndivData)
df=Indiv2DataFrame(IndivData,classes=seq(0,2,0.025))
STdiag(df,log=TRUE)
}
\keyword{datasets}
