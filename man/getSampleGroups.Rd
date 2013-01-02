\name{getSampleGroups}
\alias{getSampleGroups}
\alias{getSampleGroups-methods}
\alias{getSampleGroups,flowJoWorkspace-method}
\title{
	Get a table of sample groups from a flowJo workspace
}
\description{
Return a data frame of sample group information from a flowJo workspace
}
\usage{
\S4method{getSampleGroups}{flowJoWorkspace}(x)
}
\arguments{
  \item{x}{
A \code{flowJoWorkspace} object.
}
}
\details{
Returns a table of samples and groups defined in the flowJo workspace
}
\value{
A \code{data.frame} containing the \code{groupName}, \code{groupID}, and \code{sampleID} for each sample in the workspace. Each sample may be associated with multiple groups.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{flowJoWorkspace-class}} \code{\link{openWorkspace}}
}
\examples{
\dontrun{
	#ws is a flowJoWorkspace
	getSampleGroups(ws);
}
}
\keyword{ manip }
