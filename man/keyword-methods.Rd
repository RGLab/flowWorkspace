\name{keyword-methods}
\docType{methods}
\alias{keyword}
\alias{keyword,GatingHierarchy,character-method}
\alias{keyword,GatingHierarchy,character}
\alias{keyword,GatingSet,character-method}
\alias{keyword,GatingSet,character}
\title{ Methods to retrieve keywords associated with an FCS sample}
\description{
 See Methods for function \code{keyword} in Package `flowCore'
}
\section{Methods}{
\describe{
Retrieve a specific keyword for a specific sample in a \code{GatingHierarchy} or or set of samples in a \code{GatingSet}
\item{\code{signature(object = "GatingHierarchy", keyword = "character")}}{
	Retrieve a  keyword for the \code{GatingHierarchy}.
}
\item{\code{signature(object="GatingSet",keyword="character")}}{
	Retrieve a specific keyword for each sample in the \code{GatingSet}
}}
}
\keyword{methods}
\seealso{
	\code{\link[flowCore]{keyword-methods}}
}