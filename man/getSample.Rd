\name{getSample}
\alias{getSample}
\alias{getSample-method}
\alias{getSample,GatingHierarchy-method}
\alias{getSample,graphNEL-method}
\title{
Get the sample name associated with a GatingHierarchy
}
\description{
Return  the sample name
}
\usage{
\S4method{getSample}{GatingHierarchy}(x,isFullPath=FALSE)
\S4method{getSample}{graphNEL}(x)
}
\arguments{
  \item{x}{
A \code{GatingHierarchy} or a \code{graphNEL} object from the \code{@tree} slot of a GatingHierarchy
}

\item{isFullPath}{
\code{isFullPath} is a logical value indicating whether the full path of the sample FCS file is returned.Default is FALSE.
}

}
\details{
Returns the name of the sample, or the path to the FCS file.
}
\value{
A "character" vector of length 1. Either the sample name or the path to the FCS file.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Mike Jiang \email{wjiang2@fhcrc.org}
}
\examples{
\dontrun{
	#G is  a GatingHierarhcy
	getSample(G)
	getSample(G@tree);
}
}
\keyword{ manip }
