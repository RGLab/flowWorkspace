\name{getData}
\alias{getData}
\alias{getData-methods}
\alias{getData,GatingHierarchy,ANY-method}
\alias{getData,GatingHierarchyInternal,missing-method}
\alias{getData,GatingHierarchyInternal,numeric-method}
\alias{getData,GatingHierarchyInternal,character-method}
\alias{getData,GatingSet,ANY-method}
\alias{getData,GatingSetInternal,missing-method}
\alias{getData,GatingSetInternal,numeric-method}
\alias{getData,GatingSetInternal,character-method}
\alias{getData,graphNEL,ANY-method}



\title{
Return the flowFrame/flowSet associated with a GatingHierarchy/GatingSet
}
\description{
Return the flowFrame/flowSet associated with a GatingHierarchy/GatingSet
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "GatingHierarchy", y = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "GatingHierarchyInternal", y = "character")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "GatingHierarchyInternal", y = "missing")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "GatingHierarchyInternal", y = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "GatingSet", y = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "GatingSetInternal", y = "character")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "GatingSetInternal", y = "missing")}}{
%%  ~~describe this method here~~
}


\item{\code{signature(obj = "GatingSetInternal", y = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "graphNEL", y = "ANY")}}{
%%  ~~describe this method here~~
}
}
}
\arguments{
  \item{obj}{
A \code{GatingHierarchy}, \code{GatingSet}, or \code{graphNEL} object extracted from a \code{GatingHierarchy} \code{@tree} slot. 
}
  \item{y}{
\code{character}  the node name or full(/partial) gating path or \code{numeric} node index. If \code{obj} is a GatingHierarchy or graphNEL, \code{y} is the name of the node in \code{obj} for which you wish to extract the data or a numeric index into \code{getNodes(obj)}. If \code{obj} is a GatingSet, \code{y} is a numeric index into \code{getNodes(obj[[i]])}, where \code{i} is any GatingHierarchy in the GatingSet. The trees represented by the GatingHierarchies are ASSUMED to be the same. 
		If not specified, will return the complete flowFrame/flowSet at the root node.
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
	geData(G,"cd4")
	
	#gh is a GatingHierarchy
	getData(gh)
	
}
}
\keyword{ manip }
