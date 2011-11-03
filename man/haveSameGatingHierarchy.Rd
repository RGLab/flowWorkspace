\name{haveSameGatingHierarchy}
\alias{haveSameGatingHierarchy}
\alias{haveSameGatingHierarchy-methods}
\alias{haveSameGatingHierarchy,GatingHierarchy,GatingHierarchy-method}
\alias{haveSameGatingHierarchy,GatingHierarchy,GatingSet-method}
\alias{haveSameGatingHierarchy,GatingSet,GatingHierarchy-method}
\alias{haveSameGatingHierarchy,GatingSet,GatingSet-method}
\alias{haveSameGatingHierarchy,GatingSet,missing-method}
\alias{haveSameGatingHierarchy,list,missing-method}
\title{
Tests whether two GatingHierarchy or GatingSet objects have the same gating structure.
}
\description{
Tests whether GatingHierarchy, or GatingSet objects have the same gating structure.
}
\usage{
    \S4method{haveSameGatingHierarchy}{GatingHierarchy,GatingHierarchy}(object1, object2)
    \S4method{haveSameGatingHierarchy}{GatingHierarchy,GatingSet}(object1, object2)
    \S4method{haveSameGatingHierarchy}{GatingSet,GatingHierarchy}(object1, object2)
    \S4method{haveSameGatingHierarchy}{GatingSet,GatingSet}(object1, object2)
    \S4method{haveSameGatingHierarchy}{GatingSet,missing}(object1, object2)
    \S4method{haveSameGatingHierarchy}{list,missing}(object1, object2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object1}{
A \code{GatingHierarchy}, \code{GatingSet}, or \code{list} of GatingHierarchy objects.
}
  \item{object2}{
A \code{GatingHierarchy}, \code{GatingSet}, or \code{missing}.
}
}
\details{
Compares the gating tree of the two objects, or of the GatingHierarchies in a single GatingSet.
}
\value{
A \code{logical}, \code{TRUE} if the gating structures are the same, or \code{FALSE} if they differ.
}
\author{
    Greg Finak <gfinak@fhcrc.org>
}
\keyword{manip}