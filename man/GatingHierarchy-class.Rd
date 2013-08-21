\name{GatingHierarchy-class}
\alias{GatingHierarchy-class}
\alias{show}
\alias{show,GatingHierarchy}
\title{Class GatingHierarchy}
\description{
  GatingHierarchy is a class for representing the gating
  hierarchy,which can be either imported from a flowJo
  workspace or constructed in R.
}
\details{
  There is a one-to-one correspondence between
  GatingHierarchy objects and FCS files in the flowJo
  workspace. Each sample (FCS file) is associated with it's
  own GatingHierarchy. This is different from the workflow
  representation used in flowCore.  It is also more space
  efficient by storing gating results as logical/bit vector
  instead of copying the raw data.  A GatingHierarchy can
  have two ``states''. After a call to
  parseWorkspace(...,execute=FALSE), the workspace is
  imported but the data is not. A call to execute() is
  needed in order to load, transform, compensate, and gate
  the associated data. Alternately, one may call
  parseWorkspace(...,execute=TRUE). Whether or not a
  GatingHierarchy has been applied to data is encoded in
  the \code{flag} slot. Some methods will warn the user, or
  may not function correctly if the GatingHierarchy has not
  been execute()d.This mechanism is in place, largely for
  the purpose of memory efficiency when working with larger
  workspaces. It allows the use to load a workspace and
  subset desired samples before proceeding to load the
  data. If one has netCDF 4 library installed, then memory
  is no longer an issue.  Given a GatingHierarchy, one can
  extract the data associated with any subpopulation,
  extract gates, plot gates, and extract population
  proportions. This facilitates the comparison of manual
  gating methods with automated gating algorithms.

  \section{Slots}{ \describe{ \item{\code{name}:}{Object of
  class \code{"character"}. The name of the sample. Usually
  the FCS filename, but it depends on how it was defined in
  the flowJo workspace. } \item{\code{flag}:}{Object of
  class \code{"logical"}. A flag indicating whether the
  gates, transformations, and compensation matrices have
  been applied to data, or simply imported.}
  \item{\code{dataPath}:}{Object of class
  \code{"character"}. A path to the fcs file associated
  with this GatingHierarchy } \item{\code{pointer}:}{Object
  of class \code{"externalptr"}. points to the gating
  hierarchy stored in C data structure.} } }
}
\examples{
require(flowWorkspaceData)
	d<-system.file("extdata",package="flowWorkspaceData")
	wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
	ws <- openWorkspace(wsfile);
	G<-try(parseWorkspace(ws,path=d,name=1));
 gh <- G[[1]]
	getPopStats(gh);
	plotPopCV(gh)
 nodes <- getNodes(gh)
 thisNode <- nodes[4]
	plotGate(gh,thisNode);
	getGate(gh,thisNode);
	getData(gh,thisNode)
}
\seealso{
  \code{\link{parseWorkspace}}
  \code{\linkS4class{GatingSet}}
}

