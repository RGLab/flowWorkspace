\name{flowJoWorkspace-class}
\Rdversion{1.1}
\docType{class}
\alias{flowJoWorkspace-class}
% \alias{closeWorkspace,flowJoWorkspace-method}
% \alias{flowWorkspace2flowCore,flowJoWorkspace-method}
% \alias{getCompensationMatrices,flowJoWorkspace-method}
% \alias{getKeywords,flowJoWorkspace,character-method}
% \alias{getSampleGroups,flowJoWorkspace-method}
% \alias{getSamples,flowJoWorkspace-method}
% \alias{getTransformations,flowJoWorkspace-method}
% \alias{parseWorkspace,flowJoWorkspace-method}
% \alias{show,flowJoWorkspace-method}
% \alias{summary,flowJoWorkspace-method}

\title{Class \code{"flowJoWorkspace"}}
\description{
An R representation of a flowJo workspace.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("flowJoWorkspace.xml", ...)}.
}
\section{Slots}{
  \describe{
  \item{\code{version}:}{Object of class \code{"character"}. The version of the XML workspace. }
  \item{\code{file}:}{Object of class \code{"character"}. The file name. }
  \item{\code{.cache}:}{Object of class \code{"environment"}. An environment for internal use.  }
	\item{\code{path}:}{Object of class \code{"character"}. The path to the file. }
  \item{\code{doc}:}{Object of class \code{"XMLInternalDocument"}. The XML document object. }
  \item{\code{options}:}{Object of class \code{"integer"}. The XML parsing options passed to \code{\link{xmlTreeParse}}. }
  }
}
\section{Methods}{
  \describe{
    \item{closeWorkspace}{\code{signature(workspace = "flowJoWorkspace")}: Close the workspace file and delete the C representation of the XML document, freeing memory. }
    \item{flowWorkspace2flowCore}{\code{signature(obj = "flowJoWorkspace")}: Convert a flowJo workspace to a flowCore workflow }
    \item{getCompensationMatrices}{\code{signature(x = "flowJoWorkspace")}: Retrieve the compensation matrices in the flowJo workspace. }
    \item{getKeywords}{\code{signature(obj = "flowJoWorkspace", y = "character")}: Get the keywords for sample y from the flowJo workspace }
    \item{getSampleGroups}{\code{signature(x = "flowJoWorkspace")}: Get the sample groups defined in the flowJo workspace. }
    \item{getSamples}{\code{signature(x = "flowJoWorkspace")}: Get the samples listed in the flowJo workspace. }
    \item{getTransformations}{\code{signature(x = "flowJoWorkspace")}: Get the data transformations listed in the flowJo workspace }
    \item{parseWorkspace}{\code{signature(obj = "flowJoWorkspace")}: Parse a workspace, creating a GatingSet. }
    \item{show}{\code{signature(object = "flowJoWorkspace")}: Print information about a workspace }
    \item{summary}{\code{signature(object = "flowJoWorkspace")}: Summarize }
	 }
}
\references{
http://www.rglab.org/
}
\author{
Greg Finak <gfinak@fhcrc.org>
}

\seealso{
\code{\linkS4class{GatingSet}} 
\code{\linkS4class{GatingHierarchy}}
}
\examples{
require(flowWorkspaceData)
d<-system.file("extdata",package="flowWorkspaceData")
wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
ws <- openWorkspace(wsfile);
summary(ws)
getSamples(ws)
}
\keyword{classes}
