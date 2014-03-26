\name{getNodes}
\alias{getNodes}
\alias{getNodes-methods}
\alias{getNodes,GatingSet-method}
\title{Get the names of all nodes from a gating hierarchy.}
\arguments{
  \item{x}{A \code{GatingSet} Assuming the gating hierarchy
  are identical within the \code{GatingSet}, the Gating
  tree of the first sample is used to query the node
  information.}

  \item{y}{A \code{character} the name or full(/partial)
  gating path of the population node of interest.  Or, a
  \code{numeric} index into the node list of nodes in the
  \code{GatingHierarchy}.}

  \item{order}{\code{order=c("regular","tsort","bfs")}
  returns the nodes in regular, topological or
  breadth-first sort order.  "regular" is default.}

  \item{isPath}{(Deprecated by 'path') A \code{logical}
  scalar to tell the method whether to return the full
  gating path or just terminal node name}

  \item{path}{A \code{character} or \code{numeric} scalar.
  when \code{numeric}, it specifies the fixed length of
  gating path (length 1 displays terminal name).  When
  \code{character}, it can be either 'full' (full path,
  which is default) or 'auto' (display the shortest unique
  gating path from the bottom of gating tree).}

  \item{prefix}{A \code{character} scalar to tell the
  method whether to add internal node index as the prefix
  to the node name (only valid when 'path' is set to 1).
  the valid values are "auto", "none", "all".}

  \item{...}{Additional arguments.}
}
\value{
  getNodes returns a \code{character} vector of
  node/population names, ordered appropriately.
}
\description{
  \code{getNodes} returns a character vector of names of
  the nodes (populations) in the \code{GatingSet}.
}
\details{
  integer indices of nodes are based on regular order,so
  whenver need to map from character node name to integer
  node ID,make sure to use default order which is regular.
}
\examples{
\dontrun{
    #G is a gating hierarchy
    getNodes(G, path = 1])#return node names (without prefix)
    getNodes(G, path = 1, prefix = "all"])#return node names with unqiue ID
    getNodes(G, path = 1, prefix = "auto"])#prepend unqiue ID as needed
    getNodes(G,path = "full")#return the full path
    getNodes(G,path = 2)#return the path as length of two
    getNodes(G,path = "auto)#automatically determine the length of path
    setNode(G,"L","lymph")
  }
}

