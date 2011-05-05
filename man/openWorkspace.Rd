\name{openWorkspace}
\alias{openWorkspace}
\alias{openWorkspace-methods}
\alias{openWorkspace,character-method}
\title{
Open a flowJo workspace
}
\description{
Open a flowJo workspace and return a \code{flowJoWorkspace} object.
}
\usage{
\S4method{openWorkspace}{character}(file)
}
\arguments{
  \item{file}{
	Full path to the XML flowJo workspace file.
}
}
\details{
	Open an XML flowJo workspace file and return a \code{flowJoWorkspace} object. The workspace is represented using a \code{XMLInternalDocument} object.
}
\value{
Returns a \code{flowJoWorkspace} object.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
See Also as \code{\link{xmlTreeParse}}
}
\examples{
\dontrun{
	file<-"myworkspace.xml"
	ws<-openWorkspace(file);
	class(ws); #flowJoWorkspace
}
}
\keyword{ manip }
