\name{GatingSet-class}
\alias{GatingSet-class}
\title{Class \code{"GatingSet"}}
\description{
  GatingSet holds a set of \code{GatingHierarchy} objects,
  representing a set of samples and the gating scheme
  associated with each.
}
\details{
  Objects store a collection of GatingHierarchies and
  represent a group in a flowJo workspace.

  \section{Slots}{ \describe{ \item{\code{set}:}{Object of
  class \code{"list"}. A list of GatingHierarchy objects }
  \item{\code{data}:}{Object of class \code{"flowSet"}.
  flow data associated with this GatingHierarchy }
  \item{\code{pointer}:}{Object of class
  \code{"externalptr"}. points to the gating hierarchy
  stored in C data structure.} \item{\code{guid}:}{Object
  of class \code{"character"}. the unique identifier for
  GatingSet object.} } }
}
\examples{
require(flowWorkspaceData)
  d<-system.file("extdata",package="flowWorkspaceData")
  wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
  ws <- openWorkspace(wsfile);
  G<-try(parseWorkspace(ws,execute=TRUE,path=d,name=1));
  plotPopCV(G);
}
\seealso{
  \code{\linkS4class{GatingHierarchy}}
  \code{\linkS4class{flowJoWorkspace}}
}

