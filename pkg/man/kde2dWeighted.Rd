\name{kde2dWeighted}
\alias{kde2dWeighted}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two-Dimensional Weighted Kernel Density Estimation
}
\description{
Two-dimensional weighted kernel density estimation with an axis-aligned bivariate normal kernel, evaluated on a rectangular grid}
\usage{
kde2dWeighted(x, y, w, h, n, lims = c(range(x), range(y)),proba.min=1E-6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x coordinate of data
}
  \item{y}{y coordinate of data
}
  \item{w}{Vector of same length than x and y, weight of (x,y) coordinate.}
  \item{h}{Vector of bandwidths for x and y directions. Defaults to normal reference bandwidth (see \link{bandwidth.nrd}). A scalar value will be taken to apply to both directions.}
  \item{n}{Vector of number of grid points in the two directions. A scalar value will be taken to apply to both directions.}
  \item{lims}{
  The limits of the rectangle covered by the grid as c(xl, xu, yl, yu).
  }
  \item{proba.min}{Scalar giving the minimum value for the density estimation. Every density <proba.min will be set to 0.}
  }
\value{
A dataframe of dim n[1]*n[2], 3 giving x, y and z.
}
\references{
Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
}
\seealso{
\code{\link{kde2d}}
}
\examples{
data(sample)

# Diagram with linear grayscale for z axis
STdiag(data=sample)

# Kernel density estimation on data sample
s2=kde2dWeighted(x=sample$x,y=sample$y,w=sample$z)
STdiag(data=s2)
}
