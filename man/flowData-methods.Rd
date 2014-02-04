\name{flowData}
\alias{flowData}
\alias{flowData-methods}
\alias{flowData,GatingSet-method}
\alias{flowData<-}
\alias{flowData<-,GatingSet-method}
\title{Fetch or replace the flowData object associated with a GatingSet .}
\arguments{
  \item{x}{A \code{GatingSet}}

  \item{value}{The replacement \code{flowSet} or
  \code{ncdfFlowSet} object}
}
\value{
  the object with the new flowSet in place.
}
\description{
  Accessor method that gets or replaces the
  flowset/ncdfFlowSet object in a GatingSet or
  GatingHierarchy
}
\details{
  Accessor method that sets or replaces the ncdfFlowSet
  object in the GatingSet or GatingHierarchy.
}

