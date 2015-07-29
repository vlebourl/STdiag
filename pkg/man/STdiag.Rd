\name{STdiag}
\alias{STdiag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interface to \link[lattice]{levelplot} for structured time series and rich datasets.
}
\description{
This function is based on the lattice function \link[lattice]{levelplot} and is specifically designed to help plot structured time series.}
\usage{
STdiag(formula,data,
           x,y,z,
           main,
           xlab,ylab,
           log=FALSE,
           zlim,znb=11,
           color="",
           smooth=FALSE,sm,n,probamin = 1e-06,
           interp = FALSE, intervX, intervY,
           bgcolor=rgb(254,254,226,maxColorValue=255),
           scales,
           colorkey,
           contour,cuts,
           panel,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
  Formula to plot the data. Must be of the form z~x*y with x, y and z being names of columns of the data frame data.
}
  \item{data}{
  Data frame to plot as a structure-time diagram. If formula is not provided, it must be a three columns data frame with colomns being x, y and z. The column used for x can be a numeric or a date (\link[base]{Date},\link[base]{DateTimeClasses}).
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
  heat, tim, cm or rainbow. default is  brewer.pal(11, "RdYlBu")[11:1] (see \link[RColorBrewer]{brewer.pal} for details). Custom colormap can be provided with the key word 'custom' in the format {list('custom',col)} where {col} is a vector of colors.
}
\item{smooth}{Logical. If TRUE, STdiag plots a 2d kernel density plot instead of the number of individuals. Kernel density calculation is based on \link[MASS]{kde2d}.}
\item{sm}{If smooth is TRUE, scalar in [0,1] defining the smoothness of the kernel density estimation. 0 for the raw data, 1 for bandwith in x and y direction given by /link[MASS]{bandwidth.nrd}.}
\item{n}{If smooth is TRUE, number of points on the x y grid.}
\item{interp}{Logical. Wether a bilinear interpolation should be applied to the data before plotting.}
\item{intervX,intervY}{Intergers, distance between two points on X and Y for the interpolation.}
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
\item{contour}{
  Logical, wether to add contour on the plot. Default FALSE.
}
\item{cuts}{
  Number of contour lines to plot or a vector specifying where the contours should be plotted.
}
\item{panel}{arguments passed to panel option of levelplot. Default is \code{function(...){levelplot(...)}}}
\item{...}{
  Further arguments passed to levelplot.
}
}

\value{
No values is returned. Only the diagram is displayed. To record the plot, use usual display functions such as \link[grDevices]{pdf} for instance.}

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
