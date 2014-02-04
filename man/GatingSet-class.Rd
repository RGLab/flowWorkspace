\name{GatingSet-class}
\alias{[}
\alias{[,GatingSet,ANY-method}
\alias{[,GatingSetList,ANY-method}
\alias{[[}
\alias{[[,GatingSet,character-method}
\alias{[[,GatingSet,logical-method}
\alias{[[,GatingSet,numeric-method}
\alias{GatingSet-class}
\alias{GatingSet,flowSet-method}
\alias{rbind2,GatingSet,GatingSet-method}
\alias{show,GatingSet-method}
\title{Class \code{"GatingSet"}}
\arguments{
  \item{x}{\code{GatingSet} or \code{GatingSetList}}

  \item{i}{\code{numeric} or \code{logical} or
  \code{character} used as sample index}

  \item{j}{not used}

  \item{drop}{not used}

  \item{...}{not used}
}
\description{
  GatingSet holds a set of \code{GatingHierarchy} objects,
  representing a set of samples and the gating scheme
  associated with each.

  \code{[} subsets a \code{GatingSet} or
  \code{GatingSetList} using the familiar bracket notation

  \code{[[} extract a \code{GatingHierarchy} object from a
  \code{GatingSet} or \code{GatingSetList}
}
\details{
  Objects stores a collection of GatingHierarchies and
  represent a group in a flowJo workspace. A GatingSet can
  have two ``states''. After a call to
  parseWorkspace(...,execute=FALSE) , the workspace is
  imported but the data is not. Setting \code{execute} to
  \code{TRUE} is needed in order to load, transform,
  compensate, and gate the associated data. Whether or not
  a GatingHierarchy has been applied to data is encoded in
  the \code{flag} slot. Some methods will warn the user, or
  may not function correctly if the GatingHierarchy has not
  been executed. This mechanism is in place, largely for
  the purpose of speed when working with larger workspaces.
  It allows the use to load a workspace and subset desired
  samples before proceeding to load the data.
}
\section{Slots}{
  \describe{ \item{\code{FCSPath}:}{Object of class
  \code{"character"}. A path to the fcs files associated
  with this GatingSet } \item{\code{data}:}{Object of class
  \code{"flowSet"}. flow data associated with this
  GatingSet } \item{\code{flag}:}{Object of class
  \code{"logical"}. A flag indicating whether the gates,
  transformations, and compensation matrices have been
  applied to data, or simply imported.}
  \item{\code{pointer}:}{Object of class
  \code{"externalptr"}. points to the gating hierarchy
  stored in C data structure.} \item{\code{guid}:}{Object
  of class \code{"character"}. the unique identifier for
  GatingSet object.} }
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
  \code{\link{parseWorkspace}}
}

