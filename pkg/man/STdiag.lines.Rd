\name{STdiag.lines}
\alias{STdiag.lines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Add a line to STdiag
}
\description{
  This function updates an STdiag with a line}
\usage{
  STdiag.lines(stdiag, lx, ly, 
                type="l", alpha=1,cex=0.8,
                col=1,font=1,pch=16,
                fill="transparent",lty=1,lwd=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stdiag}{The STdiag to update, returned by function STdiag
                %%     ~~Describe \code{x} here~~
  }
  \item{lx,ly}{Coordinates of the line points to add to the diagram}
\item{type,alpha,cex,col,font,pch,fill,lty,lwd}{Standard parameters of the panel.lines function}
}
\value{
  Returns the stdiag with a line added to it}
\author{
  Vincent Le Bourlot}
\examples{
  st<-STdiag(z~x*y,sample)
  st<-STdiag.lines(st,c(200,300,300),c(0.5,0.5,1),col="white")
}
