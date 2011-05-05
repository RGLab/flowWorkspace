\name{getTransformations}
\alias{getTransformations}
\alias{getTransformations-methods}
\alias{getTransformations,flowJoWorkspace-method}
\title{
Return a list of transformations in a flowJo workspace
}
\description{
Return a list of all the transformations in a flowJo workspace
}
\usage{
\S4method{getTransformations}{flowJoWorkspace}(x)
}
\arguments{
  \item{x}{
A \code{flowJoWorkspace} object
}
}
\details{
Returns a list of the transformations in the flowJo workspace. The list is of length \code{L}, where \code{L} is the number of distinct transformations applied to samples in the \code{flowJoWorkspace}. Each element of \code{L} is itself a \code{list} of length \code{M}, where \code{M} is the number of parameters that were transformed for a sample or group of samples in a \code{flowJoWorkspace}. For example, if a sample has 10 parameters, and 5 are transformed during analysis, using two different sets of transformations, then L will be of length 2, and each element of L will be of length 5. The elements of \code{L} represent channel- or parameter-specific transformation functions that map from raw intensity values to channel-space used by flowJo.

}
\value{
\item{comp 1 }{The first transformaton in the workspace.}
\item{comp 2 }{The second transformation in the workspace.}
Comp 1 .. Comp \code{L} are themselves lists of functions, with each element of the list representing a transformation applied to a specific channel/parameter of a sample. 
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}
\note{
This representation will likely be changed in the future to use the \code{flowCore} internal \code{transformation} classes.
}

\seealso{
\code{link}{names}
}

\examples{
\dontrun{
	#Assume f is a flowJoWorkspace
	getTransformations(f);
}
}
\keyword{ manip }
