\name{plotPopCV}
\alias{plotPopCV}
\alias{plotPopCV-methods}
\alias{plotPopCV,GatingHierarchy-method}
\alias{plotPopCV,GatingSet-method}
\title{Plot the coefficient of variation between flowJo and flowCore population statistics for each population in a gating hierarchy.}
\arguments{
  \item{x}{A \code{GatingHierarchy} from a
  \code{flowJoWorkspace}, or a \code{GatingSet}.}

  \item{m}{\code{numeric} The number of rows in the panel
  plot. Now deprecated, uses lattice.}

  \item{n}{\code{numeric} The number of columns in the
  panel plot. Now deprecated, uses lattice.}

  \item{\dots}{Additional arguments to the \code{barplot}
  methods.}
}
\value{
  Nothing is returned.
}
\description{
  This function plots the coefficient of variation
  calculated between the flowJo population statistics and
  the flowCore population statistics for each population in
  a gating hierarchy extracted from a flowJoWorkspace.
}
\details{
  The CVs are plotted as barplots across panels on a grid
  of size \code{m} by \code{n}.
}
\examples{
\dontrun{
    #G is a GatingHierarchy
    plotPopCV(G,4,4);
  }
}
\seealso{
  \code{\link{getPopStats}}
}

