\name{getCompensationMatrices}
\alias{getCompensationMatrices}
\alias{getCompensationMatrices-methods}
\alias{getCompensationMatrices,flowJoWorkspace-method}
\alias{getCompensationMatrices,GatingHierarchy-method}
\alias{getCompensationMatrices,GatingHierarchyInternal-method}

\title{
Retrieve the compensation matrices from a flowJo Workspace or GatingHierarchy
}
\description{
Retrieve the compensation matrices from a flowJo workspace or GatingHierarchy.
}
\usage{
\S4method{getCompensationMatrices}{flowJoWorkspace}(x)
\S4method{getCompensationMatrices}{GatingHierarchy}(x)
}
\arguments{
  \item{x}{
A \code{flowJoWorkspace} or \code{GatingHierarchy} object.
}
}
\details{
Return all the compensation matrices in a flowJoWorkspace object or a compensation matrix in a GatingHierarchy.
}
\value{
A list of \code{matrix} representing the spillover matrices in the \code{flowJoWorkspace}
or a spillover matrix in \code{GatingHierarchy}

}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{openWorkspace}}
}

\examples{
\dontrun{
	#ws is a flowJoWorkspace
	file<-"myworkspace.xml"
	ws<-openWorkspace(file)
	getCompensationMatrices(ws);
}
}
\keyword{ manip }
