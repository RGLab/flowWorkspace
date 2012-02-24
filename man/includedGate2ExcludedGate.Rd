\name{includedGate2ExcludedGate}
\alias{includedGate2ExcludedGate}
\alias{.includedGate2ExcludedGate}
\title{

Convert a set of gates given by their flowJo gate paths to its complement (and excluding Boolean gates), referenced by gate indices recognized by \code{\link[flowStats]{normalizeGatingSet}}.
}
\description{
Convert a set of gates given by their flowJo gate paths to its complement (and excluding Boolean gates), referenced by gate indices recognized by \code{\link[flowStats]{normalizeGatingSet}}.
}

\usage{
includedGate2ExcludedGate(gs, includegates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gs}{
A \code{GatingSet}%%     ~~Describe \code{gs} here~~
}
  \item{includegates}{
A \code{character} vector of flowJo gating paths for the gates in \code{gs}
%%     ~~Describe \code{includegates} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{numeric} vector of gate indices to be used with \code{\link[flowStats]{normalizeGatingSet}}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Greg Finak <gfinak@fhcrc.org>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
