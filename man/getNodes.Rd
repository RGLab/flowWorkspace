\name{getNodes}
\alias{getNodes}
\alias{getNodes-methods}
\alias{getNodes,GatingHierarchy-method}
\title{
Get the names of all nodes in a gating hierarchy
}
\description{
Returns a character vector of names of the nodes (populations) in the \code{GatingHierarchy}.
}
\usage{
\S4method{getNodes}{GatingHierarchy}(x,tsort=FALSE,...)
}
\arguments{
  \item{x}{
A \code{GatingHierarchy}
}
  \item{tsort}{
	\code{tsort=TRUE|FALSE} returns the nodes in topological sort (TRUE) order.
}
	\item{...}{
	Additional arguments.
	}
}
\value{
a \code{character} vector of node/population names, ordered appropriately.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link[RBGL]{tsort}}
}
\examples{
\dontrun{
	#G is a gating hierarchy
	getNodes(G,tsort=TRUE)#return nodes in topological sort order.
}
}
\keyword{ manip }
