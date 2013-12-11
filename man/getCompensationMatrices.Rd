\name{getCompensationMatrices}
\alias{getCompensationMatrices}
\alias{getCompensationMatrices-methods}
\alias{getCompensationMatrices,flowJoWorkspace-method}
\alias{getCompensationMatrices,GatingHierarchy-method}
\title{Retrieve the compensation matrices from a flowJo Workspace or GatingHierarchy}
\arguments{
  \item{x}{A \code{flowJoWorkspace} or
  \code{GatingHierarchy} object.}
}
\value{
  A list of \code{matrix} representing the spillover
  matrices in the \code{flowJoWorkspace} or a spillover
  matrix in \code{GatingHierarchy}
}
\description{
  Retrieve the compensation matrices from a flowJo
  workspace or GatingHierarchy.
}
\details{
  Return all the compensation matrices in a flowJoWorkspace
  object or a compensation matrix in a GatingHierarchy.
}
\examples{
\dontrun{
    #ws is a flowJoWorkspace
  file<-"myworkspace.xml"
  ws<-openWorkspace(file)
  getCompensationMatrices(ws);
}
}
\seealso{
  \code{\link{openWorkspace}}
}

