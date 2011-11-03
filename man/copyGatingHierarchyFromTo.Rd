\name{copyGatingHierarchyFromTo}
\alias{copyGatingHierarchyFromTo}
\alias{copyGatingHierarchyFromTo-methods}
\alias{copyGatingHierarchyFromTo,GatingHierarchy,GatingSet-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Copy the gating tree from a GatingHierarchy to a GatingSet
}
\description{
Copies the gating structure from a GatingHierarchy to a (potentially empty or ungated) GatingSet.
}
\usage{
copyGatingHierarchyFromTo(a, b, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
A \code{GatingHierarchy}
}
  \item{b}{
A \code{GatingSet}
}
  \item{\dots}{
Additional arguments (currently not used).
}
}
\details{
This function will copy the gating tree and gates from one GatingHierarchy to all the GatingHierarchy objects in a GatingSet. Can be useful if the GatingSet contains ungated samples.
}
\value{
Returns a \code{GatingSet} with the new gates.
}
\author{
Greg Finak <gfinak@fhcrc.org>
}

\keyword{internal}