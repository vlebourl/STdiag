\name{STdiag.measure}
\alias{STdiag.measure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hand measures on a structure time diagram}
\description{
This function allows to extract the coordinates of a point, a line or a region of a structure time diagram.}
\usage{
STdiag.measure(stdiag,type=c("point","line"),region=FALSE,range=3.3,pch=21,lwd=3,col.line=1,col='white',fill=1,cex=1.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stdiag}{
  The plot returned by \code{STdiag}.
  }
  \item{type}{
  Object to identify, either "point" or "line".
  }
  \item{region}{TRUE / FALSE, extract a region of the plot.}
  \item{range}{Factor of the mean distance between two coordinates to define the region to extract.}
  \item{pch,lwd,col.line,col,fill,cex}{Graphical parameters of the points and line superimposed on the diagram.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This function allows to extract the coordinates of a point, a line or a region of a structure time diagram. If \code{region=TRUE}, the points to return are determined by the range of the region. The parameter \code{range} defines this range. If the wanted point coordinates are \code{x} and \code{y} and the original data are \code{data}, the region is defined by x in \code{x - range * mean(diff(unique(data$x)))} and \code{x + range * mean(diff(unique(data$x)))} and y in \code{y - range * mean(diff(unique(data$y)))} and \code{y - range * mean(diff(unique(data$y)))}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
If region is \code{FALSE}, returns a data frame with the x, y and z coordinates of the points selected and the index of the corresponding line in the original table, and (if type="l") the slope of the line plotted.

If region is \code{TRUE}, returns a LIST where 
\item{selection}{is the same data frame as described above}
\item{region}{is the subset of the original data included in the region extracted.}
}
\author{
Vincent Le Bourlot}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{STdiag}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

# stdiag=STdiag(sample, log=TRUE)
# STdiag.measure(stdiag,"p",TRUE,range=5)
}
