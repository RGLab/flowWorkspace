\name{getNodes}
\alias{getNodes}
\alias{getNodes-methods}
\alias{getNodes,GatingHierarchy-method}
\title{Get the names of all nodes in a gating hierarchy.}
\arguments{
  \item{x}{A \code{GatingHierarchy}}

  \item{y}{A \code{character} the name or full(/partial)
  gating path of the population node of interest.  Or, a
  \code{numeric} index into the node list of nodes in the
  \code{GatingHierarchy}.}

  \item{order}{\code{order=c("regular","tsort","bfs")}
  returns the nodes in regular, topological or
  breadth-first sort order.  "regular" is default.}

  \item{isPath}{A \code{logical} scalar to tell the method
  whether to return the full gating path or just terminal
  node name}

  \item{prefix}{A \code{logical} scalar to tell the method
  whether to add internal node index as the prefix to the
  node name}

  \item{...}{Additional arguments.}
}
\value{
  getNodes returns a \code{character} vector of
  node/population names, ordered appropriately.
}
\description{
  \code{getNodes} returns a character vector of names of
  the nodes (populations) in the \code{GatingHierarchy}.
}
\details{
  integer indices of nodes are based on regular order,so
  whenver need to map from character node name to integer
  node ID,make sure to use default order which is regular.
}
\examples{
\dontrun{
    #G is a gating hierarchy
    getNodes(G[[1], isPath = FALSE])#return node names
    getNodes(G[[1]],isPath = TRUE)#return the full path
    setNode(G,"L","lymph")
  }
}

