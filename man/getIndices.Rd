\name{getIndices}
\alias{getIndices}
\alias{getIndices-methods}
\alias{getIndices,GatingHierarchy,character-method}
\title{
Get the membership indices for each event with respect to a particular gate in a GatingHierarchy
}
\description{
Returns a logical vector that describes whether each event in a sample is included or excluded by this gate. 
}
\usage{
\S4method{getIndices}{GatingHierarchy,character}(obj, y)
}
\arguments{
  \item{obj}{
A \code{GatingHierarchy} representing a sample.
}
  \item{y}{
A \code{character} giving the name of the population / node of interest.
}
}
\details{
Returns a logical vector that describes whether each event in the data file is included in the given gate of this \code{GatingHierarchy}. The indices are for all events in the file, and do not reflect the population counts relative to the parent but relative to the root. To get population frequencies relative to the parent one cross-tabulate the  indices of \code{y} with the indices of its parent.
}
\value{
A logical vector of length equal to the number of events in the FCS file that determines whether each event is or is not included in the current gate.
}
\references{
\url{http://www.rglab.org}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
	}
\note{
Generally you should not need to use \code{getIndices} but the more convenient methods \code{getProp} and \code{getPopStats} which return population frequencies relative to the parent node.
}

\section{Warning}{
The indices returned reference all events in the file and are not directly suitable for computing population statistics, unless subsets are taken with respect to the parent populations.
}
\seealso{
See also \code{\link{getProp}}, \code{\link{getPopStats}}.
}
\examples{
\dontrun{
	#G is a gating hierarchy
	#Return the indices for population 5 (topological sort)
	getIndices(G,getNodes(G,tsort=TRUE)[5]);
}
}
\keyword{ manip }
