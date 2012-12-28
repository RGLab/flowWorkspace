\name{getGate}
\alias{getGate}
\alias{getGate,GatingHierarchy,character-method}
\alias{getGate,GatingHierarchyInternal,character-method}
\alias{getGate,GatingHierarchy,numeric-method}
\alias{getGate,GatingHierarchyInternal,numeric-method}
\alias{getGate,GatingSet,numeric-method}
\alias{getGate,GatingSet,character-method}
\alias{getGate,GatingSetInternal,character-method}
\alias{getGate,GatingSetInternal,numeric-method}
\title{
Return the flowCore gate definition associated with a node in a GatingHierarchy/GatingSet.
}
\description{
Return the flowCore gate definition object associated with a node in a \code{GatingHierarchy} or \code{GatingSet} object.
}
\usage{
\S4method{getGate}{GatingHierarchy,character}(obj, y)
\S4method{getGate}{GatingHierarchy,numeric}(obj, y,tsort=FALSE)
\S4method{getGate}{GatingSet,numeric}(obj, y,tsort=FALSE)

}
\arguments{
  \item{obj}{
A \code{GatingHierrarchy} or \code{GatingSet}
}
  \item{y}{
A \code{character} the name of the node of interest.  Or, a \code{numeric} index into the node list of nodes in the \code{GatingHierarchy} or \code{GatingSet}.
}
\item{tsort}{
deprecated.
}
}
\value{
A gate object from \code{flowCore}. Usually a \code{polygonGate}, but may be a \code{rectangleGate}. Boolean gates are represented by a \code{"BooleanGate"} S3 class. This is a list boolean gate definition that references populations in the GatingHierarchy and how they are to be combined logically. If \code{obj} is a \code{GatingSet}, assuming the trees associated with each \code{GatingHierarchy} are identical, then this method will return a list of gates, one for each sample in the \code{GatingSet} corresponding to the same population indexed by \code{y}.
}
\note{
You should not have to deal with boolean gates. It is sufficient to retrieve the contents of a boolean gate node with \code{getData}.
}
\references{
\url{http://www.rglab.org}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{getData}} \code{\link{getNodes}}
}
\examples{
\dontrun{	#gh is a GatingHierarchy
	getGate(gh,5) #return the gate for the fifth node in the tree.
	getGate(gh,getNodes(gh)[5]) #return the gate for the fifth node in the tree, but fetch it by name.
	#G is a GatingSet
	getGate(G,5) #return a list of gates for the fifth node in each tree
}}
\keyword{ manip }
