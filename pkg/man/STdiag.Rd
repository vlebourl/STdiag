\name{STdiag}
\alias{STdiag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to add specific options to levelplot for plotting structure-time diagram.
}
\description{
This function is based on the lattice function levelplot and is specifically designed to help plot structure-time diagrams. It takes as input a data frame of the form x, y, z, and plots z~x*y.}
\usage{
STdiag(formula=NULL, data, 
    main = NULL, 
    xlab = NULL, ylab = NULL, 
    log = TRUE, 
    zlim = NULL, znb = 50, 
    color = "", bgcolor = rgb(254,254,226,maxColorValue=255),
    scales = list(),
    colorkey = NULL
    ...
    )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
  Formula to plot the data. Must be of the form z~x*y with x, y and z being names of columns of the data frame data.
}
  \item{data}{
  Data frame to plot as a structure-time diagram. If formula is not provided, it must be a three columns data frame with colomns being x, y and z.
}
  \item{main}{
  If provided, puts a title on the structure-time diagram.
}
  \item{xlab}{
  If provided, puts a label on the X-axis of the structure-time diagram.
}
  \item{ylab}{
  If provided, puts a label on the Y-axis of the structure-time diagram.
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
  heat, tim, cm or rainbow. default is rainbow.
}
\item{bgcolor}{
  rgb(), color of the background of the plot.
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
No values is returned. Only the diagram is displayed. To record the plot, use usual display functions such as pdf().}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Vincent Le Bourlot
}
\note{
For further details on the use of levelplot, please refer to help(levelplot).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{levelplot}}
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
