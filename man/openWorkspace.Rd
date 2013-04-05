\name{openWorkspace}
\alias{openWorkspace}
\alias{openWorkspace-methods}
\alias{openWorkspace,character-method}
\alias{closeWorkspace}
\alias{closeWorkspace-methods}
\alias{closeWorkspace,flowJoWorkspace-method}
\title{
Open/Close a flowJo workspace
}
\description{
Open a flowJo workspace and return a \code{flowJoWorkspace} object.
Close a flowJoWorkspace, destroying the internal representation of the XML document, and freeing the associated memory.

}
\usage{
\S4method{openWorkspace}{character}(file, options =0, ...)
\S4method{closeWorkspace}{flowJoWorkspace}(workspace)

}
\arguments{
  \item{file}{
	Full path to the XML flowJo workspace file.
}
\item{options}{
	xml parsing options passed to \code{\link{xmlTreeParse}}
}
\item{...}{
	other arguments passed to \code{\link{xmlTreeParse}}
}
  \item{workspace}{
A \code{flowJoWorkspace}
}
}
\details{
	Open an XML flowJo workspace file and return a \code{flowJoWorkspace} object. The workspace is represented using a \code{XMLInternalDocument} object.
	Close a flowJoWorkpsace after finishing with it. This is necessary to explicitly clean up the C-based representation of the XML tree. (See the XML package).
	
}
\value{
openWorkspace returns a \code{flowJoWorkspace} object.
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
	closeWorkspace(ws);
	
}
}
\keyword{ manip }
