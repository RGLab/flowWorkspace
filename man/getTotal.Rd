\name{getTotal}
\alias{getTotal}
\alias{getTotal-methods}
\alias{getTotal,GatingHierarchy,character-method}
\alias{getTotal,GatingHierarchyInternal,character-method}

\title{
Fetch the total number of events in a gate defined in a GatingHierarchy
}
\description{
Returns the total number of events in the gate defined in the GatingHierarchy object
}
\usage{
\S4method{getTotal}{GatingHierarchy,character}(x, y)
}
\arguments{
  \item{x}{
The \code{GatingHierarchy}
}
  \item{y}{
A \code{character} name of the gate / population
}
}
\details{
Will return the total number of events included in this gate. The contents of "thisTot" variable in the "metadata" environment of the \code{nodeData} element associated with the gating tree and gate / population.
}
\value{
A \code{numeric} value of the total number of elements in the population.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}


\seealso{
\code{\link{getProp}} \code{\link{getPopStats}}
}
\examples{
\dontrun{
	#G is a gating hierarchy
	#Fifth node total.
	getTotal(G,getNodes(G,tsort=T)[5])
}
}
\keyword{ manip }
