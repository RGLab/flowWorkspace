\name{GatingSet-class}
\docType{class}
\alias{GatingSet-class}
\alias{GatingSetInternal-class}
\alias{[,GatingSet,ANY-method}
\alias{[,GatingSetInternal,ANY-method}
\alias{[[,GatingSet,ANY-method}
\alias{[[,GatingSetInternal,ANY-method}
\alias{rbind2,GatingSet,GatingSet-method}
\alias{GatingSet}
\alias{GatingSet,flowSet-method}
\alias{GatingSet,flowSet,ANY-method}
\alias{GatingSet,character,character-method}
\alias{GatingSet,GatingHierarchyInternal,character-method}
\alias{show,GatingSetInternal-method}
\alias{show,GatingHierarchyInternal-method}
\title{Class \code{"GatingSet"}}
\description{
GatingSet holds a set of \code{GatingHierarchy} objects, representing a set of samples and the gating scheme associated with each.
GatingSetInternal is the derived class that holds \code{GatingHierarchyInternal} objects.
}
\section{Objects from the Class}{
Objects can be created by a call to parseWorkspace().
The annotated data frame can be populated with the keywords from each sample.
Or by:
GatingSet(x,dMode=0,...),x is a flowSet/ncdfFlowSet. It creates an empty gating tree with only root node associated with flow data where \code{\link{add}} methods can be used to add gates. 
or by 
GatingSet(x,y,path=".",isNcdf=FALSE,dMode=1,...),x is an existing gating hierarchy(used as a gating template) and y is the flow data to be gated (flowSet/ncdfFlowSet).
It creates the GatingSet from the FCS files and copies the gates defined in the existing gating hierarchy to the new GatingSet.
\code{dMode} specifies the level of print-out that is used for the debug purpose.

\code{...} include \code{compensation} argument that can provide the customized compensation object if specified and
other arguments passed to \link{read.flowSet} or \link{read.ncdfFlowSet} functions.
	
}
\section{Description}{
Objects store a collection of GatingHierarchies and represent a group in a flowJo workspace.
}
\section{Slots}{
  \describe{
    \item{\code{set}:}{Object of class \code{"list"}. A list of GatingHierarchy objects }
    \item{\code{metadata}:}{Object of class \code{"AnnotatedDataFrame"}. Stores the metadata associated with this set of FCS samples.  }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "GatingSet", i = "ANY")}: Subset a GatingSet using the familiar bracket notation }
    \item{[<-}{\code{signature(x = "GatingSet", i = "ANY", j = "ANY", value = "GatingSet")}: Replace elements of a GatingSet. }
    \item{[[}{\code{signature(x = "GatingSet", i = "ANY")}: Extract a GatingHierarchy from a GatingSet }
    \item{[[<-}{\code{signature(x = "GatingSet", i = "ANY", j = "ANY", value = "GatingHierarchy")}: Replace a GatingHierarchy in a GatingSet }
    \item{flowWorkspace2flowCore}{\code{signature(obj = "GatingSet")}: Convert a GatingSet to a single workflow (if they have a common set of gates) or list of workflows if the GatingHierarchies differ. }
    \item{getData}{\code{signature(obj = "GatingSet")}: Return a flowSet for the GatingSet }
    \item{getGate}{\code{signature(obj = "GatingSet", y = "numeric")}: Return a flowSet for a subpopulation of each GatingHierarchy, numerically indexed. }
    \item{getKeywords}{\code{signature(obj = "GatingSet", y = "character")}: Get the keywords associated with sample y }
    \item{getKeywords}{\code{signature(obj = "GatingSet", y = "numeric")}: Get the keywords associated with sample y, numerical index. }
    \item{getSamples}{\code{signature(x = "GatingSet")}: Get the sample names of the GatingHierarchies in this GatingSet }
    \item{keyword}{\code{signature(object = "GatingSet", keyword = "character")}: Get the specific keyword for all samples in this GatingSet.}
    \item{lapply}{\code{signature(X = "GatingSet")}: lapply method for GatingSet }
    \item{length}{\code{signature(x = "GatingSet")}: Return the length of the GatingSet, number of GatingHierarhcy objects }
    \item{plotPopCV}{\code{signature(x = "GatingSet")}: plot the population coefficients of variation between flowJo and flowCore for all populations and all samples }
    \item{show}{\code{signature(object = "GatingSet")}: Print information about the GatingSet. }
     \item{rbind2}{\code{signature(x = "GatingSetInternal",y = "GatingSetInternal")}:combine two GatingSets into one. }
	 }
}
\references{
http://www.rglab.org/import-flowjo-workspaces-into-r-bioconductor/
}
\author{
Greg Finak <gfinak@fhcrc.org>
}

\seealso{
\code{\linkS4class{AnnotatedDataFrame}}
\code{\linkS4class{GatingHierarchy}}
\code{\linkS4class{flowJoWorkspace}}
}
\examples{
	require(flowWorkspaceData)
	d<-system.file("extdata",package="flowWorkspaceData")
	wsfile<-list.files(d,pattern="A2004Analysis.xml",full=TRUE)
	ws <- openWorkspace(wsfile);
	G<-try(parseWorkspace(ws,execute=TRUE,path=d,name=1));
	plotPopCV(G);
}
\keyword{classes}
