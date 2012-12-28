\name{getData}
\alias{getData}
\alias{getData-methods}
\alias{getData,GatingHierarchy-method}
\alias{getData,GatingHierarchyInternal-method}
\alias{getData,GatingSet-method}
\alias{getData,GatingSetInternal-method}
\alias{getData,graphNEL-method}



\title{
Return the flowFrame/flowSet associated with a GatingHierarchy/GatingSet
}
\description{
Return the flowFrame/flowSet associated with a GatingHierarchy/GatingSet
}
\usage{
\S4method{getData}{GatingHierarchy}(obj,y=NULL,tsort=FALSE)
\S4method{getData}{GatingSet}(obj,y=NULL,tsort=FALSE)
\S4method{getData}{graphNEL}(obj,y=NULL,tsort=FALSE)
}
\arguments{
  \item{obj}{
A \code{GatingHierarchy}, \code{GatingSet}, or \code{graphNEL} object extracted from a \code{GatingHierarchy} \code{@tree} slot. 
}
  \item{y}{
\code{character} node name or \code{numeric} node index. Default is \code{NULL}. If \code{obj} is a GatingHierarchy or graphNEL, \code{y} is the name of the node in \code{obj} for which you wish to extract the data or a numeric index into \code{getNodes(obj)}. If \code{obj} is a GatingSet, \code{y} is a numeric index into \code{getNodes(obj[[i]])}, where \code{i} is any GatingHierarchy in the GatingSet. The trees represented by the GatingHierarchies are ASSUMED to be the same. Defaults to \code{NULL}, will return the complete flowFrame at the root node.
}
	\item{tsort}{
	deprecated.
	}
}
\details{
Returns a flowFrame/flowSet containing the events in the gate defined at node \code{y}. 
Subset membership can be obtained using \code{getIndices}. 
Population statistics can be obtained using \code{getPop} and \code{getPopStats}. 
When calling \code{getData} on a GatingSet,the trees representing the GatingHierarchy for each sample in the GaingSet are presumed to have the same structure.}
\value{
A \code{flowFrame} object if \code{obj} is a GatingHierarchy or graphNEL. A \code{flowSet} if a \code{GatingSet}.
A \code{ncdfFlowSet} if the node name/index (\code{y}) is NULL and the flow data is stored as cdf. 
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}
\note{
The argument \code{tsort} ensures that the nodes are ordered in topological sort order. This is useful if you are using numeric node indices to access data across two or more identical trees (GatingHierarchies) with different node names (population names).

WARNING
The trees in a GatingSet are assumed to have the same structure, such that a topological sort of the nodes in any tree will return the populations in the same order.

}


\seealso{
\code{\link{getIndices}} \code{\link{getProp}} \code{\link{getPopStats}}
}

\examples{
\dontrun{
	#G is a GatingSet
	geData(G,3) #get a flowSet constructed from the third node / population in the tree.
	
	#G is a GatingHierarchy
	getData(G,)
	
}
}
\keyword{ manip }
