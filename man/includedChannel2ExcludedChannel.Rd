\name{includedChannel2ExcludedChannel}
\alias{includedChannel2ExcludedChannel}
\alias{.includedChannel2ExcludedChannel}
\title{

Convert a list of parameter names in a \code{GatingSet} to its complement.
}
\description{

Convert a list of parameter names in a \code{GatingSet} to its complement.
}
\usage{
includedChannel2ExcludedChannel(gs, includedims = NULL)
}
\arguments{
  \item{gs}{
A \code{GatingSet}.
%%     ~~Describe \code{gs} here~~
}
  \item{includedims}{
A \code{character} vector of channel / parameter names in the gating set.%%     ~~Describe \code{includedims} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A \code{character} vector of channel names. The complement of \code{includedims}.
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
