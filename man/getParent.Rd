\name{getParent}
\alias{getParent}
\alias{getParent-methods}
\alias{getParent,GatingHierarchy,character-method}
\alias{getParent,GatingHierarchy,numeric-method}
\title{
Return the name of the parent population of the current population in the GatingHierarchy
}
\description{
Returns the name of the parent population of the current population in the given \code{GatingHierarchy}
}
\usage{
\S4method{getParent}{GatingHierarchy,character}(obj, y)
\S4method{getParent}{GatingHierarchy,numeric}(obj, y, tsort=FALSE)
}
\arguments{
  \item{obj}{
A \code{GatingHierarchy}}
  \item{y}{
The population whose parent you want to retrieve, either \code{character} or \code{numeric}
}
\item{tsort}{
\code{logical} If TRUE, nodes are ordered by topological sort. Important when comparing across identical trees and using numeric indices. Default FALSE.
}
}
\value{
Returns a \code{character} vector, the name of the parent population.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{getChildren}}
}
\examples{
\dontrun{
	#G is a gatinghierarchy
	#return the name of the parent of the fifth node in the hierarchy.
	getParent(G,getNodes(G,tsort=TRUE)[5])
}
}
\keyword{ manip }
