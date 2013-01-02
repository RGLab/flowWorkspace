\name{saveNcdf}
\alias{saveNcdf}
\title{
Save the netcdf file of a GatingSet or GatingHierarchy to a permanent location.
}
\description{
When creating a new GatingSet or GatingHierarchy using netcdf backed storage, the nc files are written to the TEMP directory. This will move those files to a more permanent location, allowing the object to be saved and persist after restarting R and relaoding the workspace.
}
\usage{
saveNcdf(objName, path = getwd())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{objName}{
The name of the GatingSet or GatingHierarchy. A \code{character} vector.
}
  \item{path}{
      A \code{character} vector: the path where the ncdf files should be saved.
}
}
\details{
The function will modify the GatingSet or GatingHierarchy to reflect the new location of the ncdf files. Do not pass the object directly via objName, but rather pass the name of the variable holding the GatingSet or GatingHierarchy as a \code{character} vector.
}
\value{
Does not return anything. Modifes the object in the parent environment.
}
\author{
Greg Finak <gfinak@fhcrc.org>
}
\keyword{manip}