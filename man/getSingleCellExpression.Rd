\name{getSingleCellExpression}
\alias{getData,GatingSet,name-method}
\alias{getData,GatingSetList,name-method}
\alias{getSingleCellExpression}
\alias{getSingleCellExpression,GatingSet,character-method}
\alias{getSingleCellExpression,GatingSetList,character-method}
\title{Return the cell events data that express in any of the single populations defined in \code{y}}
\arguments{
  \item{x}{A \code{GatingSet} or \code{GatingSetList}
  object .}

  \item{nodes}{\code{character} vector specifying different
  cell populations}

  \item{map}{mapping node names (as specified in the gating
  hierarchy of the gating set) to channel names (as
  specified in either the \code{desc} or \code{name}
  columns of the parameters of the associated
  \code{flowFrame}s in the \code{GatingSet}).}
}
\value{
  A \code{list} of \code{numerci matrices}
}
\description{
  Returns a list of matrix containing the events that
  expressed in any one of the populations efined in
  \code{y}
}
\examples{
\dontrun{
  #G is a GatingSet
	geData(G,3)
	res <- getSingleCellExpression(gs[1], c("4+/TNFa+", "4+/IL2+"))
	res[[1]]
	res <- getSingleCellExpression(gs[1], c("4+/TNFa+", "4+/IL2+") , list("4+/TNFa+" = "TNFa", "4+/IL2+" = "IL2"))
}
}
\author{
  Mike Jiang \email{wjiang2@fhcrc.org}
}
\seealso{
  \code{\link{getIndices}} \code{\link{getProp}}
  \code{\link{getPopStats}}
}

