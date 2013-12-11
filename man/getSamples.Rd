\name{getSamples}
\alias{getSamples}
\alias{getSamples-methods}
\alias{getSamples,flowJoWorkspace-method}
\alias{getSamples,GatingSet-method}
\title{Get a list of samples from a flowJo workspace}
\arguments{
  \item{x}{A \code{flowJoWorkspace}}
}
\value{
  A \code{data.frame} with columns \code{sampleID},
  \code{name}, and \code{compID} if \code{x} is a
  \code{flowJoWorkspace}.
}
\description{
  Return a data frame of samples contained in a flowJo
  workspace
}
\details{
  Returns a \code{data.frame} of samples in the
  \code{flowJoWorkspace}, including their \code{sampleID},
  \code{name}, and \code{compID} (compensation matrix ID).
}
\examples{
\dontrun{
        #ws is a flowJoWorkspace
        getSamples(ws);
      }
}

