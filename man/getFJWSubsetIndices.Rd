\name{getFJWSubsetIndices}
\alias{getFJWSubsetIndices}
\title{
Fetch the indices for a subset of samples in a flowJo workspace, based on a keyword value pair
}
\description{
Returns an index vector into the samples in a flowJo workspace for use with parseWorkspace(subset=), based on a keyword/value filter in a specific group of samples.
}
\usage{
getFJWSubsetIndices(ws, key, value, group)
}
\arguments{
  \item{ws}{
	The flowJoWorkspace object
}
  \item{key}{
	The name of the keyword. Type "character"
}
  \item{value}{
	The value of the keyword. Type "character"
}
  \item{group}{
	The group of samples to subset. Type numeric.
}
}
\details{
This function will calculate the indices of a subset of samples in a flowJoWorkspace, based on a keyword/value filter. It is applied to a specific group of samples in the workspace. It is meant to be passed to the subset= argument of parseWorkspace.
}
\value{
A numeric vector of indices.
}
\author{
Greg Finak <gfinak@fhcrc.org>
}

\seealso{
\code{\link{parseWorkspace}}
}
\examples{

}
