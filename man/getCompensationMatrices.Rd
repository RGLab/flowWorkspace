\name{getCompensationMatrices}
\alias{getCompensationMatrices}
\alias{getCompensationMatrices-methods}
\alias{getCompensationMatrices,flowJoWorkspace-method}
\title{
Retrieve the compensation matrices from a flowJo Workspace
}
\description{
Retrieve all the compensation matrices from a flowJo workspace
}
\usage{
\S4method{getCompensationMatrices}{flowJoWorkspace}(x)
}
\arguments{
  \item{x}{
A \code{flowJoWorkspace} object.
}
}
\details{
Return all the compensation matrices in a flowJoWorkspace object.
}
\value{
A list of \code{matrix} representing the spillover matrices in the \code{flowJoWorkspace}
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
