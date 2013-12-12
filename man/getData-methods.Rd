\name{getData}
\alias{GatingSetList,name-method}
\alias{getData}
\alias{getData,GatingHierarchy,character-method}
\alias{getData,GatingHierarchy,missing-method}
\alias{getData,GatingHierarchy,numeric-method}
\alias{getData,GatingSet,ANY-method}
\alias{getData,GatingSet,character-method}
\alias{getData,GatingSet,missing-method}
\alias{getData,GatingSet,name-method}
\alias{getData,GatingSet,numeric-method}
\alias{getData,GatingSetList,ANY-method}
\alias{getData,GatingSetList,name-method}
\title{get gated flow data from a GatingHierarchy/GatingSet/GatingSetList}
\arguments{
  \item{obj}{A \code{GatingHierarchy}, \code{GatingSet} or
  \code{GatingSetList} object.}

  \item{y}{\code{character} the node name or full(/partial)
  gating path or \code{numeric} node index.  If not
  specified, will return the complete flowFrame/flowSet at
  the root node.}

  \item{...}{arguments passed to ncdfFlow::[[}

  \item{obj}{A \code{GatingSet} object .}

  \item{y}{\code{name} boolean expression specifying the
  boolean combination of different cell populations}
}
\value{
  A \code{flowFrame} object if \code{obj} is a
  GatingHierarchy. A \code{flowSet} or \code{ncdfFlowSet}
  if a \code{GatingSet}. A \code{ncdfFlowList} if a
  \code{GatingSetList}.

  A \code{list} of \code{numerci matrices}
}
\description{
  get gated flow data from a
  GatingHierarchy/GatingSet/GatingSetList

  Returns a flowSet containing the events defined at by
  boolean expression \code{y}.
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
\dontrun{
  #G is a GatingSet
	geData(G,3)
	res <- getData(gs[1],quote(`4+/TNFa+|4+/IL2+`))
	res[[1]]
}
}
\author{
  Mike Jiang \email{wjiang2@fhcrc.org}
}
\seealso{
  \code{\link{getIndices}} \code{\link{getProp}}
  \code{\link{getPopStats}}

  \code{\link{getIndices}} \code{\link{getProp}}
  \code{\link{getPopStats}}
}

