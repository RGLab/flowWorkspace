\name{getDimensions}
\alias{getDimensions}
\alias{getDimensions-methods}
\alias{getDimensions,GatingHierarchy,character-method}

\title{
Return the dimensions on which a gate is applied within a GatingHierarchy
}
\description{
Return the dimension names on which a gate in a \code{GatingHierarchy} is applied.
}
\usage{
\S4method{getDimensions}{GatingHierarchy,character}(obj, y, index=FALSE)
}
\arguments{
  \item{obj}{
A \code{GatingHierarchy}
}
  \item{y}{
The name of the population (node) in the \code{GatingHierarchy} for which you want the dimension names
}	
	\item{index}{
	\code{TRUE | FALSE} a logical indicating whether we should return the names of the dimensions (FALSE, default) or the indices of the dimensions (TRUE)
	}
}
\value{
A \code{character} vector of dimension names on which the gate is applied (when index=FALSE), or a \code{numeric} vector of the indices of the dimensions on which the gate is applied (when index=TRUE). 
}
\references{
\url{http://www.rglab.org}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{getNodes}}
}
\examples{
\dontrun{
	#G is a GatingHierarchy
	#Fetch the dimensions for the fifth population in the hierarchy.
	getDimensions(G,getNodes(G)[5],index=FALSE)
}}
\keyword{ manip }
