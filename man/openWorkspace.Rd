\name{openWorkspace}
\alias{closeWorkspace}
\alias{closeWorkspace-methods}
\alias{closeWorkspace,flowJoWorkspace-method}
\alias{openWorkspace}
\alias{openWorkspace-methods}
\alias{openWorkspace,character-method}
\title{Open/Close a flowJo workspace}
\arguments{
  \item{file}{Full path to the XML flowJo workspace file.}

  \item{options}{xml parsing options passed to
  \code{\link{xmlTreeParse}}}

  \item{...}{other arguments passed to
  \code{\link{xmlTreeParse}}}

  \item{workspace}{A \code{flowJoWorkspace}}
}
\value{
  a \code{flowJoWorkspace} object.
}
\description{
  Open a flowJo workspace and return a
  \code{flowJoWorkspace} object. Close a flowJoWorkspace,
  destroying the internal representation of the XML
  document, and freeing the associated memory.
}
\details{
  Open an XML flowJo workspace file and return a
  \code{flowJoWorkspace} object. The workspace is
  represented using a \code{XMLInternalDocument} object.
  Close a flowJoWorkpsace after finishing with it. This is
  necessary to explicitly clean up the C-based
  representation of the XML tree. (See the XML package).
}
\examples{
\dontrun{
	file<-"myworkspace.xml"
	ws<-openWorkspace(file);
	class(ws); #flowJoWorkspace
	closeWorkspace(ws);
}
}

