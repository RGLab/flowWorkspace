\name{closeWorkspace}
\alias{closeWorkspace}
\alias{closeWorkspace-methods}
\alias{closeWorkspace,flowJoWorkspace-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Close a flowJoWorkspace
}
\description{
Close a flowJoWorkspace, destroying the internal representation of the XML document, and freeing the associated memory.
}
\usage{
\S4method{closeWorkspace}{flowJoWorkspace}(workspace)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{workspace}{
A \code{flowJoWorkspace}
}
}
\details{*
Close a flowJoWorkpsace after finishing with it. This is necessary to explicitly clean up the C-based representation of the XML tree. (See the XML package).
}
\value{
This function doesn't return anything.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link[XML]{xmlTreeParse}}
}
\examples{
\dontrun{
	ws<-openWorkspace("myworkspace.xml");
	G<-parseWorkspace(ws,path=".") #path to fcs files, will search recursively
	closeWorkspace(ws);
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
