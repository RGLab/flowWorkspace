\name{getNodes}
\alias{getNodes}
\alias{getNodes-methods}
\alias{getNodes,GatingHierarchy-method}
\alias{getNodes,GatingHierarchyInternal-method}
\alias{setNode}
\title{
Get the names of all nodes in a gating hierarchy.
Update the name of one node in a gating hierarchy/GatingSet.
}
\description{
\code{getNodes} returns a character vector of names of the nodes (populations) in the \code{GatingHierarchy}.
\code{setNode} update the name of one node in a gating hierarchy/GatingSet.

}
\usage{
\S4method{getNodes}{GatingHierarchy}(x,y=NULL,tsort=FALSE,isPath=FALSE,...)
\S4method{getNodes}{GatingHierarchyInternal}(x,y=NULL,order="regular",isPath=FALSE,prefix=FALSE,...)

\S4method{setNode}{"GatingHierarchy","numeric","character"}(x,y,value,...)
\S4method{setNode}{"GatingHierarchy","character","character"}(x,y,value,...)
\S4method{setNode}{"GatingSet","numeric","character"}(x,y,value,...)
\S4method{setNode}{"GatingSet","character","character"}(x,y,value,...)
}
\arguments{
  \item{x}{
A \code{GatingHierarchy}
}
\item{y}{
A \code{character} the name of the node of interest.  Or, a \code{numeric} index into the node list of nodes in the \code{GatingHierarchy}.
}
  \item{order}{
	\code{order=c("regular","tsort","bfs")} returns the nodes in regular, topological or breadth-first sort order.
		"regular" is default.
		}
	\item{isPath}{ A \code{logical} scalar to tell the method whether to return the full gating path or just terminal node name
		}
	\item{prefix}{ A \code{logical} scalar to tell the method whether to add internal node index as the prefix to the node name}
	\item{tsort}{ A \code{logical} scalar to tell the method whether to return the nodes in topological sort order.It is replaced by \code{order} argument for \code{GatingHierarchyInternal} class.}

	\item{...}{
	Additional arguments.
	}
\item{value}{
A \code{character} the name of the node.
}
	
}
\value{
getNodes returns a \code{character} vector of node/population names, ordered appropriately.
setNodes update the node name without returning any thing.
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
	getNodes(G[[1]])#return node names
	getNodes(G[[1]],isPath=TRUE)#return the full path
	setNode(G,"L","lymph")
}
}
\keyword{ manip }
