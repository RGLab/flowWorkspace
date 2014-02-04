\name{getIndices}
\alias{getIndices}
\alias{getIndices-methods}
\alias{getIndices,GatingHierarchy,character-method}
\alias{getIndices,GatingHierarchy,numeric-method}
\alias{getIndices,GatingSet,name-method}
\title{Get the membership indices for each event with respect to a particular gate in a GatingHierarchy}
\arguments{
  \item{obj}{A \code{GatingHierarchy} representing a
  sample.}

  \item{y}{A \code{character} or \code{numeric} giving the
  name or full(/partial) gating path or index of the
  population / node of interest.}

  \item{y}{a quoted expression.}
}
\value{
  A logical vector of length equal to the number of events
  in the FCS file that determines whether each event is or
  is not included in the current gate.
}
\description{
  Returns a logical vector that describes whether each
  event in a sample is included or excluded by this gate.

  It adds the boolean gates and does the gating on the fly,
  and return the indices associated with that bool gate,
  and remove the bool gate the typical use case would be
  extracting any-cytokine-expressed cells
}
\details{
  Returns a logical vector that describes whether each
  event in the data file is included in the given gate of
  this \code{GatingHierarchy}. The indices are for all
  events in the file, and do not reflect the population
  counts relative to the parent but relative to the root.
  To get population frequencies relative to the parent one
  cross-tabulate the indices of \code{y} with the indices
  of its parent.
}
\note{
  Generally you should not need to use \code{getIndices}
  but the more convenient methods \code{getProp} and
  \code{getPopStats} which return population frequencies
  relative to the parent node. The indices returned
  reference all events in the file and are not directly
  suitable for computing population statistics, unless
  subsets are taken with respect to the parent populations.
}
\examples{
\dontrun{
    #G is a gating hierarchy
    #Return the indices for population 5 (topological sort)
    getIndices(G,getNodes(G,tsort=TRUE)[5]);
}
\dontrun{

	getIndices(gs,quote(`4+/TNFa+|4+/IL2+`))

}
}
\seealso{
  \code{\link{getProp}}, \code{\link{getPopStats}}
}

