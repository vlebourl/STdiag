\name{volcano.sub}
\alias{volcano.sub}
\docType{data}
\title{
Subset of the data frame volcano.df}
\description{
This subset is intended to give an exmaple of interpolation, it is a subset of volcano with 13 rows and 17 columns from the original data.
}
\usage{data(volcano.sub)}
\format{
  A data frame with 1947 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{X axis coordinates.}
    \item{\code{y}}{Y axis coordinates.}
    \item{\code{z}}{Values of the data set volcano corresponding the the xy coordinates.}
  }
}
\source{
Digitized from a topographic map by Ross Ihaka. These data should not be regarded as accurate.}
\references{
filled.contour for a nice plot.}
\examples{
data(volcano.sub)
STdiag(data=volcano.sub,color="topo",log=FALSE)
STdiag(data=Interpolation(volcano.sub),color="topo",log=FALSE)
}
\keyword{datasets}
