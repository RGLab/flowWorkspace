\name{setNode}
\alias{setNode}
\title{Update the name of one node in a gating hierarchy/GatingSet.}
\arguments{
  \item{value}{A \code{character} the name of the node.}
}
\description{
  \code{setNode} update the name of one node in a gating
  hierarchy/GatingSet.
}
\examples{
\dontrun{
    #G is a gating hierarchy
    getNodes(G[[1]])#return node names
    getNodes(G[[1]],isPath=TRUE)#return the full path
    setNode(G,"L","lymph")
  }
}

