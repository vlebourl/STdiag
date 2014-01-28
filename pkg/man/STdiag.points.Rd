\name{STdiag.points}
\alias{STdiag.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a point to STdiag
}
\description{
This function updates an STdiag with points}
\usage{
STdiag.points(stdiag,px ,py , 
  alpha=1,cex=0.8,col=1,font=1,
  pch=16,fill="transparent")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stdiag}{The STdiag to update, returned by function STdiag
%%     ~~Describe \code{x} here~~
}
\item{px,py}{Coordinates of the points to add to the diagram}
\item{alpha,cex,col,font,pch,fill}{Standard parameters of the panel.points function}
}
\value{
Returns the stdiag with the point added to it}
\author{
Vincent Le Bourlot}
\examples{
st<-STdiag(z~x*y,sample)
st<-STdiag.points(st,300,1,col="white")
}
