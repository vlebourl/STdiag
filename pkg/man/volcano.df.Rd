\name{volcano.df}
\alias{volcano.df}
\docType{data}
\title{
Data set volcano converted to a data frame.}
\description{
Maunga Whau (Mt Eden) is one of about 50 volcanos in the Auckland volcanic field. This data set gives topographic information for Maunga Whau on a 10m by 10m grid.
}
\usage{data(volcano.df)}
\format{
  A data frame with 5307 observations on the 3 variables x coordinates, y coordinates and the topographic informations.
}
\source{
Digitized from a topographic map by Ross Ihaka. These data should not be regarded as accurate.
}
\examples{
data(volcano.df)
STdiag(data=volcano.df,color="topo",log=FALSE)}
\keyword{datasets}
