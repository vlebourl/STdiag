\name{Interpolation}
\alias{Interpolation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to do bi-directionnal linear interpolation over x and y. 
}
\description{
Interpolation takes a three columns data frame to do a linear interpolation over X with points seperated by intervX then over Y with points seperated by intervY.
}
\usage{
Interpolation(Tab, intervX = NULL, intervY = NULL, mini=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tab}{
Dataframe to be interpolated. This data frame must be a three columns data frame. Columns must be x, y and z, in that order.
}
  \item{intervX}{
Interval between two interpolated points over X axis.
}
  \item{intervY}{
Interval between two interpolated points over Y axis.
}
\item{mini}{Logical, if TRUE, the return result is subset(Tab,Tab$z>=min(subset(Tab$z,Tab$z>0)))}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a three data frame of the form x, y and z with the same column names as input data frame.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Vincent Le Bourlot
}
\note{
For further details on the use of interp, please refer to help(interp).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# an example using the dataset sample provided

# Load sample
data(sample)

# Plot the data using STdiag
#
STdiag(z~x*y,sample,log=TRUE)

# Interpolate with finner X and Y and plot the new data 
sample.interp=Interpolation(sample,intervX=1,intervY=0.02)
STdiag(z~x*y,sample.interp,log=TRUE)
}