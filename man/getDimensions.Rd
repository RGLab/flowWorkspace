\name{getDimensions}
\alias{getDimensions}
\alias{getDimensions-methods}
\alias{getDimensions,GatingHierarchy,character-method}
\title{Get the boundaries of a gate or dimensions on which a gate is applied within a GatingHierarchy}
\arguments{
  \item{obj}{A \code{GatingHierarchy}}

  \item{y}{A \code{character}, the name of the node / gate
  / population of interest .}

  \item{index}{\code{TRUE | FALSE} a logical indicating
  whether we should return the names of the dimensions
  (FALSE, default) or the indices of the dimensions (TRUE)}
}
\value{
  \code{getBoundaries} returns a \code{matrix} with column
  names corresponding to channels / dimensions, and rows to
  \code{x,y} tuples of vertices for polygon gates in these
  dimensions.  \code{getDimensions} returns a
  \code{character} vector of dimension names on which the
  gate is applied (when index=FALSE), or a \code{numeric}
  vector of the indices of the dimensions on which the gate
  is applied (when index=TRUE).
}
\description{
  Get the boundaries (vertices) of a flowJo gate on the
  transformed scale or the dimension names on which a gate
  in a \code{GatingHierarchy} is applied.
}
\details{
  Each node in a GatingHierarchy represents a population.
  That population is defined by a gate.
  \code{getBoundaries} will return the vertices of the
  gate.
}
\examples{
dontrun{
    file<-"myworkspace.xml"
    ws<-openWorkspace(file)
    G<-parseWorkspace(ws,execute=TRUE,path=".")
    n<-getNodes(G[[1]],tsort=TRUE)[3] #get the third node in the first gating hierarchy (topological sort order)
    getGate(G[[1]],n); #return the gate for that node.
    #Fetch the dimensions for the fifth population in the hierarchy.
    getDimensions(G,getNodes(G)[5],index=FALSE)
    getBoundaries(G,getNodes(G)[5])
  }
}
\seealso{
  \code{\link{getGate}},\code{\link{getNodes}}
}

