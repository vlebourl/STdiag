\name{Matrix2DataFrame}
\alias{Matrix2DataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to convert a matrix to a data frame.
}
\description{
Function to convert a x*y matrix to a x*y by 3 data frame with columns x, y and as.vector(matrix).
}
\usage{
Matrix2DataFrame(mat,
                 x=NULL,y=NULL,
                 xlab="x",ylab="y",zlab="z")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{Matrix to convert to a data frame.}
  \item{x,y}{Vectors of x and y coordinates. Must be of dimension dim(mat)[1] ou dim(mat)[2]. If NULL, x=0:dim(mat)[1]-1 and y=0:dim(mat)[2]-1.}
  \item{xlab, ylab, zlab}{Character string, labels of the columns of the new data frame.}
}
\value{
Return a data frame of dimension x*y by 3 data frame containing an expansion of mat as a data frame.}
\author{
Vincent Le Bourlot}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
expand.grid}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

# Define a 2 by 3 matrix
mat=matrix(1:6,2,3)

# Convert to a 6 by 3 data frame
Matrix2DataFrame(mat)

# Define values for x and y
Matrix2DataFrame(mat,x=1:2,y=c("a","b","c"))

# If x and y are inverted
Matrix2DataFrame(mat,x=c("a","b","c"),y=1:2)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
