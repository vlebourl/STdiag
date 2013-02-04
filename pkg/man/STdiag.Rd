\name{STdiag}
\alias{STdiag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to add specific options to levelplot for plotting structure-time diagram.
}
\description{
This function is based on the lattice function levelplot and is specifically designed to help plot structure-time diagrams. It takes as input a data frame of the form x, y, z, and plots z~x*y.}
\usage{
STdiag(formula=NULL,data=NULL,
           x=NULL,y=NULL,z=NULL,
           main=NULL,
           xlab=NULL,ylab=NULL,
           log=FALSE,
           zlim=NULL,znb=50,color="",
           smooth=FALSE,sm,n,probamin = 1e-06,
           bgcolor=rgb(254,254,226,maxColorValue=255),
           scales=list(),
           colorkey=NULL,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
  Formula to plot the data. Must be of the form z~x*y with x, y and z being names of columns of the data frame data.
}
  \item{data}{
  Data frame to plot as a structure-time diagram. If formula is not provided, it must be a three columns data frame with colomns being x, y and z.
}
  \item{x,y}{Vectors of x and y coordinates in a case of matrix representation.}
  \item{z}{Matrix of size x y giving the number of individuals at coordinate (x,y).}
  \item{main}{
  If provided, puts a title on the structure-time diagram.
}
  \item{xlab,ylab}{
  If provided, puts a label on the X-axis and Y-axis of the structure-time diagram.
}
  \item{log}{
  Boolean to choose between log scaled color gradient (TRUE) or linear (FALSE).}
  \item{zlim}{
  Numeric vector, c(zmin, zmax), specify the range of color to use. If NULL, zlim=range(z).
}
  \item{znb}{
  Integer, specifies the number of colors to use.
}
  \item{color}{
  Character string, type of color gradient to use. can be either one of gray, topo, terrain
  heat, tim, cm or rainbow. default is tim (see \link[fields]{tim.colors} for details).
}
\item{smooth}{Logical. If TRUE, STdiag plots a 2d kernel density plot instead of the number of individuals. Kernel density calculation is based on \link[MASS]{kde2d}.}
\item{sm}{If smooth is TRUE, scalar in [0,1] defining the smoothness of the kernel density estimation. 0 for the raw data, 1 for bandwith in x and y direction given by /link[MASS]{bandwidth.nrd}.}
\item{n}{If smooth is TRUE, number of points on the x y grid.}
\item{probamin}{Numeric setting the identity with 0 when computing the kernel density estimate, for comfort of plotting. Default value is set to 1e-06.}
\item{bgcolor}{
  \link[grDevices]{rgb}, color of the background of the plot.
  }
\item{scales}{
  List of arguments passed to levelplot scales option.
}
\item{colorkey}{
  Logical or list of arguments passed to levelplot colorkey option.
}
\item{...}{
  Further arguments passed to levelplot.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
No values is returned. Only the diagram is displayed. To record the plot, use usual display functions such as \link[grDevices]{pdf} for instance.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Vincent Le Bourlot
}
\note{
For further details on the use of \link[lattice]{levelplot}, please refer to help(levelplot).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{levelplot}, \link[STdiag]{sample}}
}
\examples{
data(sample)

# Diagram with linear grayscale for z axis
STdiag(data=sample,log=FALSE,color="gray")

# Diagram with linear tim color scale for z axis
STdiag(data=sample,log=FALSE,color="tim")

# Diagram with log rainbow color scale for z axis
STdiag(data=sample)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
