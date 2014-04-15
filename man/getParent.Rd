\name{getParent}
\alias{getChildren}
\alias{getChildren-methods}
\alias{getChildren,GatingSet,character-method}
\alias{getChildren,GatingSet,numeric-method}
\alias{getParent}
\alias{getParent-methods}
\alias{getParent,GatingSet,character-method}
\alias{getParent,GatingSet,numeric-method}
\title{Return the name of the parent population or a list of child populations of the current population in the GatingHierarchy}
\arguments{
  \item{obj}{A \code{GatingHierarchy}}

  \item{y}{a \code{character/numeric} the name or
  full(/partial) gating path or node indices of the node /
  population.}

  \item{...}{other arguments passed to \link{getNodes}
  methods}
}
\value{
  \code{getParent} returns a \code{character} vector, the
  name of the parent population.  \code{getChildren}
  returns a \code{character} or \code{numeric} vector of
  the node names or node indices of the child nodes of the
  current node. An empty vector if the node has no
  children.
}
\description{
  Returns the name of the parent population or a
  character/numeric vector of all the children of the
  current population in the given \code{GatingHierarchy}
}
\examples{
\dontrun{
    #G is a gatinghierarchy
    #return the name of the parent of the fifth node in the hierarchy.
    getParent(G,getNodes(G[[1)[5])
    n<-getNodes(G,tsort=T)[4];
    getChildren(G,n);#Get the names of the child nodes of the 4th node in this gating hierarchy.
    getChildren(G,4);#Get the ids of the child nodes
  }
}
\seealso{
  \code{\link{getNodes}}
}

