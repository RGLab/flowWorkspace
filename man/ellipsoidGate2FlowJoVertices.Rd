\name{ellipsoidGate2FlowJoVertices}
\alias{ellipsoidGate2FlowJoVertices}
\alias{ellipsoidGate2FlowJoVertices-methods}
\alias{ellipsoidGate2FlowJoVertices,ellipsoidGate-method}
\title{
Get the vertices of an ellipsoid gate needed to export it to flowJo
}
\description{
Fetches the top, bottom, right, and leftmost points of an ellipsoid gate (represented by a covariance matrix), for export to flowJo.
}
\usage{
\S4method{ellipsoidGate2FlowJoVertices}{ellipsoidGate}(gate, level = 0.95,...)
}
\arguments{
  \item{gate}{
\code{ellipsoidGate} object.
}
  \item{level}{
\code{numeric}. The quantile of the ellipse to be retrieved. Defaults to 0.95 (95\%)
}
  \item{...}{
Additional arguments. Currently not used.
}
}
\details{
flowJo's XML representation of ellipsoid gates uses the top, bottom,right, and leftmost points of the ellipse. This function retrieves those vertices given an \code{ellipsoidGate} flowCore object.
}
\value{
A \code{matrix} with two columns. Rows are the top, bottom, right, and leftmost points on the ellipse (rotated). 
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhrcr.org}
}
\note{
The implementation of this function is based on the \code{ellipse} function in the \code{ellipse} package.
}
\seealso{
\code{\link[ellipse]{ellipse}}
}
\examples{
require(flowCore)
e<-ellipsoidGate(.gate={d<-diag(2);colnames(d)<-c("A","B");d},mean=c(2,2))
ellipsoidGate2FlowJoVertices(e);
}
\keyword{ manip }
