\name{getData}
\alias{getData}
\alias{getData,GatingHierarchy,character-method}
\alias{getData,GatingHierarchy,missing-method}
\alias{getData,GatingHierarchy,numeric-method}
\alias{getData,GatingSet,ANY-method}
\alias{getData,GatingSet,character-method}
\alias{getData,GatingSet,missing-method}
\alias{getData,GatingSet,numeric-method}
\alias{getData,GatingSetList,character-method}
\alias{getData,GatingSetList,missing-method}
\alias{getData,GatingSetList,numeric-method}
\title{get gated flow data from a GatingHierarchy/GatingSet}
\arguments{
  \item{obj}{A \code{GatingHierarchy}, \code{GatingSet} or
  \code{GatingSetList} object.}

  \item{y}{\code{character} the node name or full(/partial)
  gating path or \code{numeric} node index.  If not
  specified, will return the complete flowFrame/flowSet at
  the root node.}

  \item{...}{arguments passed to ncdfFlow::[[ j a
  \code{numeric} or \code{character} used as channel index}

  \item{max}{\code{numeric} The maximum number of samples
  to be returned. It is used as a threshold to prevent huge
  memory consumption due to the coersion from ncdfFlowSet
  to flowSet}
}
\value{
  A \code{flowFrame} object if \code{obj} is a
  GatingHierarchy. A \code{flowSet} or \code{ncdfFlowSet}
  if a \code{GatingSet}. A \code{flowSet} if a
  \code{GatingSetList}.
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

