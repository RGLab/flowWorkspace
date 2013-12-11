\name{setNode}
\alias{setNode}
\alias{setNode,GatingHierarchy,character,character-method}
\alias{setNode,GatingHierarchy,character,logical-method}
\alias{setNode,GatingHierarchy,numeric,character-method}
\alias{setNode,GatingHierarchy,numeric,logical-method}
\alias{setNode,GatingSet,character,ANY-method}
\alias{setNode,GatingSet,numeric,ANY-method}
\title{Update the name of one node in a gating hierarchy/GatingSet.}
\arguments{
  \item{value}{A \code{character} the name of the node.}

  \item{x}{\code{GatingHierarchy} object}

  \item{y}{\code{numeric} node index}

  \item{value}{\code{logical} whether to hide a node}
}
\description{
  \code{setNode} update the name of one node in a gating
  hierarchy/GatingSet.

  hide/unhide a node
}
\examples{
\dontrun{
    #G is a gating hierarchy
    getNodes(G[[1]])#return node names
    setNode(G,"L","lymph")
  }
\dontrun{
     setNode(gh, 4, FALSE) # hide a node
     setNode(gh, 4, TRUE) # unhide a node
}
}

