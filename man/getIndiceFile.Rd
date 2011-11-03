\name{getIndiceFile}
\alias{getIndiceFile}
\alias{getIndiceFile-methods}
\alias{getIndiceFile,GatingHierarchy-method}
\title{
Returns the .nc file used to store the gating indices for the GatingHierarchy
}
\description{
If netCDF is being used to store the data, then this function returns the .nc file used to store the  gating indices for the GatingHierarchy.
}
\usage{
getIndiceFile(obj)
}
\arguments{
  \item{obj}{
A \code{GatingHierarchy}
}
}
\details{
Returns the .nc file used to store the gate indices for this GatingHierarchy. Used internally, not meant for user consumption at this time.
}
\value{
A \code{character} vector, the name of the .nc file.
}
\author{
Greg Finak <gfinak@fhcrc.org>
}
\keyword{manip}