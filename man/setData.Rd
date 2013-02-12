\name{setData}
\alias{setData}
\alias{setData-methods}
\alias{setData,GatingHierarchy,flowFrame-method}
\alias{setData,GatingHierarchy,flowSet-method}
\title{
Set the data (either flowSet or ncdfFlowSet) used by a GatingHierarchy
}
\description{
Set the data object used by a GatingHierarchy.
}
\usage{
setData(this, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{this}{
A \code{GatingHierarchy}
}
  \item{value}{
A \code{flowFrame} or \code{ncdfFlowSet}.
}
}
\details{
Used internally for editing GatingHierarchy or GatingSet objects. Not meant for user consumption at the moment. This function will set the data object used to back a GatingHierarchy. 
}
\value{
Nothing is returned.
}
\author{
Greg Finak <gfinak@fhcrc.org>
}
\keyword{manip}
\keyword{internal}
