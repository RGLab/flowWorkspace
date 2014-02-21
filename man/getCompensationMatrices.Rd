\name{getCompensationMatrices}
\alias{getCompensationMatrices}
\alias{getCompensationMatrices-methods}
\alias{getCompensationMatrices,GatingHierarchy-method}
\title{Retrieve the compensation matrices from a GatingHierarchy}
\arguments{
  \item{x}{A \code{GatingHierarchy} object.}
}
\value{
  A list of \code{matrix} representing the spillover matrix
  in \code{GatingHierarchy}
}
\description{
  Retrieve the compensation matrices from a
  GatingHierarchy.
}
\details{
  Return all the compensation matrices in a
  GatingHierarchy.
}
\examples{
\dontrun{
	#Assume gh is a GatingHierarchy
  getCompensationMatrices(gh);
}
}

