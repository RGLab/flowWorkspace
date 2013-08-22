\name{getData}
\alias{getData}
\alias{getData,GatingHierarchy,character-method}
\alias{getData,GatingHierarchy,missing-method}
\alias{getData,GatingHierarchy,numeric-method}
\alias{getData,GatingSet,ANY-method}
\alias{getData,GatingSet,character-method}
\alias{getData,GatingSet,missing-method}
\alias{getData,GatingSet,numeric-method}
\title{get gated flow data from a GatingHierarchy/GatingSet}
\arguments{
  \item{obj}{A \code{GatingHierarchy}, \code{GatingSet}
  object.}

  \item{y}{\code{character} the node name or full(/partial)
  gating path or \code{numeric} node index. If \code{obj}
  is a GatingHierarchy or graphNEL, \code{y} is the name of
  the node in \code{obj} for which you wish to extract the
  data or a numeric index into \code{getNodes(obj)}. If
  \code{obj} is a GatingSet, \code{y} is a numeric index
  into \code{getNodes(obj[[i]])}, where \code{i} is any
  GatingHierarchy in the GatingSet. The trees represented
  by the GatingHierarchies are ASSUMED to be the same.  If
  not specified, will return the complete flowFrame/flowSet
  at the root node. once the R paser is deprecated,it can
  be safely removed as well}

  \item{...}{arguments passed to ncdfFlow::[[ j a
  \code{numeric} or \code{character} used as channel index}
}
\value{
  A \code{flowFrame} object if \code{obj} is a
  GatingHierarchy or graphNEL. A \code{flowSet} if a
  \code{GatingSet}. A \code{ncdfFlowSet} if the node
  name/index (\code{y}) is NULL and the flow data is stored
  as cdf.
}
\description{
  get gated flow data from a GatingHierarchy/GatingSet
}
\details{
  Returns a flowFrame/flowSet containing the events in the
  gate defined at node \code{y}. Subset membership can be
  obtained using \code{getIndices}. Population statistics
  can be obtained using \code{getPop} and
  \code{getPopStats}. When calling \code{getData} on a
  GatingSet,the trees representing the GatingHierarchy for
  each sample in the GaingSet are presumed to have the same
  structure.
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
\seealso{
  \code{\link{getIndices}} \code{\link{getProp}}
  \code{\link{getPopStats}}
}

