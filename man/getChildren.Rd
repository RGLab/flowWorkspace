\name{getChildren}
\alias{getChildren}
\alias{getChildren-methods}
\alias{getChildren,GatingHierarchy,character-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a list of child populations of the current node in the \code{GatingHierarchy}
}
\description{
Returns a character vector of all the children of the current node in the \code{GatingHierarchy}
}
\usage{
\S4method{getChildren}{GatingHierarchy,character}(obj, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
a \code{GatingHierarchy}
}
  \item{y}{
a \code{character} name of the node / population.
}
}
\details{
Get the child nodes / populations of the given node / population, \code{y} in the GatingHierarchy \code{obj}
}
\value{
A \code{character} vector of the names of the child nodes of the current node. An empty vector if the node has no children.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\examples{
\dontrun{
	#G is a GatingHierarchy
	n<-getNodes(G,tsort=T)[4];
	getChildren(G,n);#Get the names of the child nodes of the 4th node in this gating hierarchy.
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
