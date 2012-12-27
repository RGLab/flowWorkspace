\name{GatingSetList-class}
\Rdversion{1.1}
\docType{class}
\alias{GatingSetList-class}
\alias{GatingSetInternal-class}
\alias{GatingSetList}
\alias{show,GatingSetList-method}
\alias{getSamples,GatingSetList-method}
\alias{rbind2,GatingSetList,missing-method}


\title{Class \code{"GatingSetList"}}
\description{
 A list of of \code{GatingSet} objects. This class exists for method dispatching.}
\section{Objects from the Class}{
Objects can be created by the constructor to GatingSetList(x).
x is a list of GatingSets.
}
\section{Description}{
Objects store a collection of GatingSets,which usually has the same gating trees and markers.
}

\section{Methods}{
  \describe{
    \item{getSamples}{\code{signature(x = "GatingSetList")}: Get the sample names of all the GatingSets in this list }
     \item{rbind2}{\code{signature(x = "GatingSetList")}:combine all the GatingSets from the list into one GatingSet object. }
    \item{show}{\code{signature(object = "GatingSetList")}: Print information about the GatingSetList. }
	 }
}
\references{
http://www.rglab.org/import-flowjo-workspaces-into-r-bioconductor/
}
\author{
Greg Finak <gfinak@fhcrc.org>
}

\seealso{
\code{\linkS4class{GatingSet}}
\code{\linkS4class{GatingHierarchy}}
}
\examples{
	\dontrun{
	#G1,G2,G3 is a GatingSet
	GL<-GatingSetList(list(G1,G3,G3))
	show(GL)
	getSamples(GL)
	G<-rbind2(GL)
	}
}
\keyword{classes}
