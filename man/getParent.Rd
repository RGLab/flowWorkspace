\name{getParent}
\alias{getParent}
\alias{getParent-methods}
\alias{getParent,GatingHierarchy,character-method}
\alias{getParent,GatingHierarchy,numeric-method}
\alias{getParent,GatingHierarchyInternal,character-method}
\alias{getParent,GatingHierarchyInternal,numeric-method}
\alias{getChildren}
\alias{getChildren-methods}
\alias{getChildren,GatingHierarchy,character-method}
\alias{getChildren,GatingHierarchyInternal,character-method}
\alias{getChildren,GatingHierarchyInternal,numeric-method}
\title{
Return the name of the parent population or a list of child populations of the current population in the GatingHierarchy

}
\description{
Returns the name of the parent population or a character/numeric vector of all the children of the current population in the given \code{GatingHierarchy}

}
\usage{
\S4method{getParent}{GatingHierarchy,character}(obj, y)
\S4method{getParent}{GatingHierarchy,numeric}(obj, y, tsort=FALSE)
\S4method{getChildren}{GatingHierarchy,character}(obj, y)
\S4method{getChildren}{GatingHierarchyInternal,character, tsort = FALSE}(obj, y)
\S4method{getChildren}{GatingHierarchyInternal,numeric}(obj, y)
}
\arguments{
  \item{obj}{
A \code{GatingHierarchy}}
  \item{y}{
a \code{character/numeric} name or node indices of the node / population.

}
\item{tsort}{
deprecated.
}
}
\value{
\code{getParent} returns a \code{character} vector, the name of the parent population.
\code{getChildren} returns a \code{character} or \code{numeric} vector of the node names or node indices of the child nodes of the current node. An empty vector if the node has no children.

}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{getNodes}}
}
\examples{
\dontrun{
	#G is a gatinghierarchy
	#return the name of the parent of the fifth node in the hierarchy.
	getParent(G,getNodes(G[[1)[5])
	n<-getNodes(G,tsort=T)[4];
	getChildren(G,n);#Get the names of the child nodes of the 4th node in this gating hierarchy.
	getChildren(G,4);#Get the ids of the child nodes 
}
}
\keyword{ manip }
