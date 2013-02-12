\name{getNodes}
\alias{getNodes}
\alias{getNodes-methods}
\alias{getNodes,GatingHierarchy-method}
\alias{getNodes,GatingHierarchyInternal-method}
\title{
Get the names of all nodes in a gating hierarchy
}
\description{
Returns a character vector of names of the nodes (populations) in the \code{GatingHierarchy}.
}
\usage{
\S4method{getNodes}{GatingHierarchy}(x,y=NULL,tsort=FALSE,isPath=FALSE,...)
\S4method{getNodes}{GatingHierarchyInternal}(x,y=NULL,order="regular",isPath=FALSE,prefix=FALSE,...)
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
	getNodes(G[[1]])#return node names
	getNodes(G[[1]],isPath=TRUE)#return the full path
}
}
\keyword{ manip }
