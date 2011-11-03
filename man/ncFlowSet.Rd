\name{ncFlowSet}
\alias{ncFlowSet<-}
\alias{ncFlowSet}
\alias{ncFlowSet-methods}
\alias{ncFlowSet,GatingHierarchy-method}
\alias{ncFlowSet,GatingSet-method}
\alias{ncFlowSet<-,GatingHierarchy-method}
\alias{ncFlowSet<-,GatingSet-method}
\title{
Fetch or replace the ncdfFlowSet object in a GatingSet or GatingHierarchy
}
\description{
Accessor method that sets or replaces the ncdfFlowSet object in a GatingSet or GatingHierarchy
}
\usage{
\S4method{ncFlowSet}{GatingHierarchy}(x)
\S4method{ncFlowSet}{GatingSet}(x)
\S4method{ncFlowSet}{GatingSet}(x)<-value
\S4method{ncFlowSet}{GatingSet}(x)<-value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{GatingHierarchy} or \code{GatingSet}
}
  \item{value}{
The replacement \code{ncdfFlowSet}
}
}
\details{
Accessor method that sets or replaces the ncdfFlowSet object in the GatingSet or GatingHierarchy.
}
\value{
Returns the object with the new ncdfFlowSet in place.
}
\author{
Greg Finak <gfinak@fhcrc.org>
}
\keyword{internal}