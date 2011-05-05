\name{getBoundaries}
\alias{getBoundaries}
\alias{getBoundaries-methods}
\alias{getBoundaries,GatingHierarchy,character-method}
\title{
Get the boundaries of a flowJo gate
}
\description{
Get the boundaries (vertices) of a flowJo gate, on the transformed scale. 
}
\usage{
\S4method{getBoundaries}{GatingHierarchy,character}(obj, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
A \code{GatingHierarchy}
}
  \item{y}{
A \code{character}, the name of the node / gate / population of interest whose gate boundaries you wish to return.
}
}
\details{
Each node in a GatingHierarchy represents a population. That population is defined by a gate. \code{getBoundaries} will return the vertices of the gate. 
}
\value{
A \code{matrix} with column names corresponding to channels / dimensions, and rows to \code{x,y} tuples of vertices for polygon gates in these dimensions. }
\references{
\url{http://www.rglab.org}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}


\seealso{
\code{\link{getGate}}
}
\examples{
\dontrun{
	file<-"myworkspace.xml"
	ws<-openWorkspace(file)
	G<-parseWorkspace(ws,execute=TRUE,path=".")
	n<-getNodes(G[[1]],tsort=TRUE)[3] #get the third node in the first gating hierarchy (topological sort order)
	getGate(G[[1]],n); #return the gate for that node.	
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
