\name{clone}
\alias{clone}
\alias{clone-methods}
\alias{clone,GatingSet-method}
\alias{clone,GatingSetInternal-method}
\title{
clone a GatingSet
}
\description{
clone a GatingSet
}
\usage{
\S4method{getSamples}{GatingSet}(x,...)
}
\arguments{
  \item{x}{
A \code{GatingSet}
}

}
\details{
Note that the regular R assignment operation on a \code{GatingSet} object does not return the copy as
one would normally expect because the \code{GatingSet} contains environment slots (and external pointer for \code{GatingSetInternal}),
which require deep-copying. So make sure to use this clone method in order to make a copy of existing object.
}
\value{
A copy of a given \code{GatingSet}.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}
\examples{
\dontrun{
	#G is  a GatingSet
	G1<-clone(G)

}
}
\keyword{ manip }
