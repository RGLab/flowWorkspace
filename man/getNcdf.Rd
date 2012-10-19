\name{getNcdf}
\alias{getNcdf}
\alias{getNcdf,GatingHierarchy-method}
\alias{getNcdf,GatingSet-method}
\title{
Return the ncdfFlow object storing the data.Deprecated. use getData instead.
}
\description{
Returns the ncdfFlowSet object that stores the data
}
\usage{
\S4method{getNcdf}{GatingHierarchy}(obj)
\S4method{getNcdf}{GatingSet}(obj)
}
\arguments{
  \item{obj}{
	A GatingHierarchy.
}
}
\details{
If a netcdfFlow object is storing the data in this GatingHierarchy, the function will return it.
}
\value{
Returns a netcdfFlowSet if one is availble. Null otherwise
}
\author{
Greg Finak <gfinak@fhcrc.org>
}
\seealso{
See also \code{\link{getData}}
}
