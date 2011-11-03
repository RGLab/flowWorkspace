\name{recomputeGatingSet}
\alias{recomputeGatingSet}
\title{
Recalculates all the gating in the GatingSet if netCDF storage is used.
}
\description{
Re--runs all the gating in a GatingSet object if netCDF storage is used.
}
\usage{
recomputeGatingSet(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A \code{GatingSet} object.
}
}
\details{
This function wil recalculate all the gates in a gating set object. Useful if you've run some normalization and need to recalcualte the gates based on the adjusted data. Only if netCDF storage is used.
}
\value{
    Returns \code{NULL}. The gates are written directly to the netCDF file and reflected in the parent frame.
}
\author{
Greg Finak <gfinak@fhcrc.org>
}
