\name{Indiv2DataFrame}
\alias{Indiv2DataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
In case data set is composed of one line per individual, with time and structure in columns. Function to convert individual based data to a data frame that can be plotted with STdiag.
}
\description{
Function to convert an individual based set of measurements into a data frame readable by STdiag. The input table must have one line per individual observed and observations (time, structure, ...) in columns.
}
\usage{
Indiv2DataFrame(tab,
                nbclass=50,
                columns=c(1,2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{Data frame containing the indivudual observations.}
  \item{nbclass}{Integer, number of class to discretize the structuring variable.}
  \item{columns}{Vector of dimension 2 (by default c(1,2)), specifying the number of respectively column x (time) and y (structure).}
}
\value{
Returns a 3 columns data frame containing the aggregated data to be plotted with STdiag.}
\author{
Vincent Le Bourlot}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
expand.grid}
\examples{

# Define a random example data set with one line per individual observed, and time and length in columns
n=20;r=50;
t=round(runif(n=n,min=0,max=100))
example=data.frame(time=sort(rep(t,r)),length=(rnorm(n=n*r,mean=1000,sd=2)))

# Convert into something that STdiag can read
STexample=Indiv2DataFrame(example)

# Plot example
STdiag(data=STexample)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
