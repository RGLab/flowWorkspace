\name{getProp}
\alias{getProp}
\alias{getProp-methods}
\alias{getProp,GatingHierarchy,character-method}
\alias{getProp,GatingHierarchyInternal,character-method}
\title{
Get the population proportions of a node (population) in a GatingHierarchy
}
\description{
Calculate the population proportion (events in the gate / events in the parent population) associated with a node in the \code{GatingHierarchy}.
}
\usage{
\S4method{getProp}{GatingHierarchy,character}(x, y)
}
\arguments{
  \item{x}{
A \code{GatingHierarchy} object.
}
  \item{y}{
\code{character}
 The name of the node. A list of nodes is accessible via \code{getNodes(x)}.
}
}
\details{
Returns the proportion of cells in the gate, relative to its parent.
}
\value{
Returns a population frequency \code{numeric}. 
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{getPopStats}}
}
\examples{
\dontrun{
	#G is a GatingHierarchy
	#proportion for the fifth population
	getProp(G,getNodes(G)[5])
}
}
\keyword{ manip }
