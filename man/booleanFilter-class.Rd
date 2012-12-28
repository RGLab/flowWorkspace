\name{booleanFilter-class}
\docType{class}
\alias{booleanFilter-class}
\alias{booleanFilter}
\alias{char2booleanFilter}
\alias{show,booleanFilter-method}

\title{Class \code{"booleanFilter"}}
\description{
A class describing logical operation (& or |) of the reference populations 
}
\section{Objects from the Class}{
Objects can be created by the constructors:
booleanFilter(expr, ..., filterId="defaultBooleanFilter")
char2booleanFilter(expr, ..., filterId="defaultBooleanFilter") 
expr is an expression or a character string.
}
\section{Description}{
It inherits class \code{\link{flowCore::expressionFilter}} and exists for the purpose of methods dispatching.
}

\section{Methods}{
  \describe{  
     \item{add}{\code{signature(wf = "GatingHierarchyInternal",action="booleanFilter")}:add a boolean gate to a GatingHierarchyInternal object. }
    \item{show}{\code{signature(object = "booleanFilter")}: Print information about the booleanFilter. }
	 }
}
\references{
http://www.rglab.org/import-flowjo-workspaces-into-r-bioconductor/
}
\author{
Mike jiang<wjiang2@fhcrc.org>
}

\seealso{
\code{\link{add}}
\code{\linkS4class{GatingHierarchy}}
}

\keyword{classes}
