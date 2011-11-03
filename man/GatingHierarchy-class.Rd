\name{GatingHierarchy-class}
\Rdversion{1.1}
\docType{class}
\alias{GatingHierarchy-class}
%\alias{[[<-,GatingSet,ANY,ANY,GatingHierarchy-method}
%\alias{execute,GatingHierarchy-method}
%\alias{flowWorkspace2flowCore,GatingHierarchy-method}
%\alias{getBoundaries,GatingHierarchy,character-method}
%\alias{getChildren,GatingHierarchy,character-method}
%\alias{getData,GatingHierarchy-method}
%\alias{getDimensions,GatingHierarchy,character-method}
%\alias{getGate,GatingHierarchy,character-method}
%\alias{getGate,GatingHierarchy,numeric-method}
%\alias{getIndices,GatingHierarchy,character-method}
%\alias{getKeywords,GatingHierarchy,missing-method}
%\alias{getNodes,GatingHierarchy-method}
%\alias{getParent,GatingHierarchy,character-method}
%\alias{getParent,GatingHierarchy,numeric-method}
%\alias{getPopStats,GatingHierarchy-method}
%\alias{getProp,GatingHierarchy,character-method}
%\alias{getSample,GatingHierarchy-method}
%\alias{getTotal,GatingHierarchy,character-method}
%\alias{keyword,GatingHierarchy,character-method}
%\alias{plot,GatingHierarchy,missing-method}
%\alias{plotGate,GatingHierarchy,character-method}
%\alias{plotGate,GatingHierarchy,numeric-method}
%\alias{plotPopCV,GatingHierarchy-method}
%\alias{show,GatingHierarchy-method}

\title{Class \code{"GatingHierarchy"}}
\description{
GatingHierarchy is a class for representing the gating hierarchy imported from a flowJo workspace. 
}
\section{Details}{
There is a one-to-one correspondence between GatingHierarchy objects and FCS files in the flowJo workspace. Each sample (FCS file) is associated with it's own GatingHierarchy. This is different from the workflow representation used in flowCore.

A GatingHierarchy can have two ``states''. After a call to parseWorkspace(...,execute=FALSE), the workspace is imported but the data is not. A call to execute() is needed in order to load, transform, compensate, and gate the associated data. Alternately, one may call parseWorkspace(...,execute=TRUE). Whether or not a GatingHierarchy has been applied to data is encoded in the \code{flag} slot. Some methods will warn the user, or may not function correctly if the GatingHierarchy has not been execute()d.This mechanism is in place, largely for the purpose of memory efficiency when working with larger workspaces. It allows the use to load a workspace and subset desired samples before proceeding to load the data. If one has netCDF 4 library installed, then memory is no longer an issue.

Given a GatingHierarchy, one can extract the data associated with any subpopulation, extract gates, plot gates, and extract population proportions. This facilitates the comparison of manual gating methods with automated gating algorithms.

GatingHierarchy objects can be converted to workflows.
}
\section{Objects from the Class}{
GatingHierarchy objects are elements of a GatingSet, which is returned by a call to \code{parseWorkspace()}.
}
\section{Slots}{
  \describe{
    \item{\code{tree}:}{Object of class \code{"graphNEL"} representing the tree-structured gating hierarchy.}
    \item{\code{nodes}:}{Object of class \code{"character"}. A vector of node names representing the populations/gates in the tree. }
    \item{\code{name}:}{Object of class \code{"character"}. The name of the sample. Usually the FCS filename, but it depends on how it was defined in the flowJo workspace. }
    \item{\code{flag}:}{Object of class \code{"logical"}. A flag indicating whether the gates, transformations, and compensation matrices have been applied to data, or simply imported.}
    \item{\code{transformations}:}{Object of class \code{"list"}. The list of transformations applied to each dimension of the data.}
    \item{\code{compensation}:}{Object of class \code{"matrix"}. The compensation matrix applied to the data}
    \item{\code{dataPath}:}{Object of class \code{"character"}. A path to the fcs file associated with this GatingHierarchy }
    \item{\code{isNcdf}:}{Flag indicating whether ncdf is used to store data and gating indices}  
}
}
\section{Methods}{
  \describe{
    \item{[[<-}{\code{signature(x = "GatingSet", i = "ANY", j = "ANY", value = "GatingHierarchy")}: replacement method for GatingHierarchy objects within a GatingSet}
    \item{execute}{\code{signature(hierarchy = "GatingHierarchy")}: Apply the compensation, transformation, and gating of a GatingHierarchy to its associated fcs file. }
    \item{flowWorkspace2flowCore}{\code{signature(obj = "GatingHierarchy")}: convert a GatingHierarchy to a flowCore workflow. }
    \item{getBoundaries}{\code{signature(obj = "GatingHierarchy", y = "character")}: Get the vertices of gate y in GatingHierarchy obj. }
    \item{getChildren}{\code{signature(obj = "GatingHierarchy", y = "character")}: Get the child nodes of population y in obj. }
    \item{getData}{\code{signature(obj = "GatingHierarchy")}: Return a flowFrame for population y in GatingHierarchy obj. }
    \item{getDimensions}{\code{signature(obj = "GatingHierarchy", y = "character")}: Get the dimensions for the gate of population y}
    \item{getGate}{\code{signature(obj = "GatingHierarchy", y = "character")}: Return the gate for population y. }
    \item{getGate}{\code{signature(obj = "GatingHierarchy", y = "numeric")}: REturn the gate for population y, by index rather than name. }
    \item{getIndices}{\code{signature(obj = "GatingHierarchy", y = "character")}: Return the event membership indices for population y.}
    \item{getKeywords}{\code{signature(obj = "GatingHierarchy", y = "missing")}: Return the keywords for the GatingHierarchy }
    \item{getNodes}{\code{signature(x = "GatingHierarchy")}: Return the node list for the gating hierarchy. }
    \item{getParent}{\code{signature(obj = "GatingHierarchy", y = "character")}: Get the parent node of a population. }
    \item{getParent}{\code{signature(obj = "GatingHierarchy", y = "numeric")}:  Get the parent node of a population, by index.}
    \item{getPopStats}{\code{signature(x = "GatingHierarchy")}: Return a table of population statistics (proportions and counts) for a gating hierarchy }
    \item{getProp}{\code{signature(x = "GatingHierarchy", y = "character")}: return the population proportion for population y. }
    \item{getSample}{\code{signature(x = "GatingHierarchy")}: Return the sample name of the gating hierarchy }
    \item{getTotal}{\code{signature(x = "GatingHierarchy", y = "character")}: get the total number of events in population y }
    \item{keyword}{\code{signature(object = "GatingHierarchy", keyword = "character")}: get a specific keyword from the gating hierarchy }
    \item{plot}{\code{signature(x = "GatingHierarchy", y = "missing")}: plot a gating hierarchy graph }
    \item{plotGate}{\code{signature(x = "GatingHierarchy", y = "character")}: plot a manual gate for the population over the parent data. }
    \item{plotGate}{\code{signature(x = "GatingHierarchy", y = "numeric")}: plot a manual gate for a population referenced by index, over the parent data }
    \item{plotPopCV}{\code{signature(x = "GatingHierarchy")}: plot the coefficient of variation for all populations, between flowCore and flowJo counts. }
    \item{show}{\code{signature(object = "GatingHierarchy")}:  Summarize a Gating Hierarchy}
	 }
}
\references{
http://www.rglab.org/import-flowjo-workspaces-into-r-bioconductor/
}
\author{
Greg Finak <gfinak@fhcrc.org>
}

\seealso{
\code{\link{parseWorkspace}}
\code{\linkS4class{GatingSet}}
}
\examples{
	require(flowWorkspaceData)
	d<-system.file("extdata",package="flowWorkspaceData")
	wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
	ws <- openWorkspace(wsfile);
	G<-try(parseWorkspace(ws,execute=TRUE,path=d,name=1));
	getPopStats(G[[1]]);
	plotPopCV(G[[1]])
	plotGate(G[[1]],getNodes(G[[1]])[4]);
	getGate(G[[1]],getNodes(G[[1]])[4]);
	getBoundaries(G[[1]],getNodes(G[[1]])[4])
	getData(G[[1]],getNodes(G[[1]])[4])
}
\keyword{classes}
