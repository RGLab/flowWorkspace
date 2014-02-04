\name{getSample}
\alias{getSample}
\alias{getSample-method}
\alias{getSample,GatingHierarchy-method}
\title{Get the sample name associated with a GatingHierarchy}
\arguments{
  \item{x}{A \code{GatingHierarchy}}

  \item{isFullPath}{\code{isFullPath} is a logical value
  indicating whether the full path of the sample FCS file
  is returned.Default is FALSE.}
}
\value{
  A "character" vector of length 1. Either the sample name
  or the path to the FCS file.
}
\description{
  Return the sample name
}
\details{
  Returns the name of the sample, or the path to the FCS
  file.
}
\examples{
\dontrun{
    #G is  a GatingHierarhcy
    getSample(G)
  }
}

