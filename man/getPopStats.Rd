\name{getPopStats}
\alias{getPopStats}
\alias{getPopStats-methods}
\alias{getPopStats,GatingHierarchy-method}
\alias{getPopStats,GatingHierarchyInternal-method}
\alias{getPopStats,GatingSet-method}
\alias{getPopStats,GatingSetInternal-method}
\title{
Return a table of population statistics for all populations in a GatingHierarchy/GatingSet
}
\description{
More useful than getPop. Returns a table of population statistics for all populations in a \code{GatingHierarchy}/\code{GatingSet}. Includes the flowJo counts, flowCore counts and frequencies.
}
\usage{
\S4method{getPopStats}{GatingHierarchy}(x,...)
}
\arguments{
  \item{x}{
A \code{GatingHierarchy} or \code{GatingSet}
}  \item{...}{
Additional arguments
}
}
\details{
Returns a table population statistics for all populations in the gating hierarchy. The output is useful for verifying that the import was successful, if the flowJo and flowCore derived counts don't differ much (i.e. if they have a small coefficient of variation.) for a GatingSet, returns a matrix of proportions for all populations and all samples
}
\value{
A \code{data.frame} with columns for the population name, flowJo derived counts, flowCore derived counts, and the population proportions (relative to their parent pouplation). 
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{getProp}}
}

\examples{
\dontrun{
	#If G is a GatingHierarchy
	getPopStats(G);
}}
\keyword{ manip }
