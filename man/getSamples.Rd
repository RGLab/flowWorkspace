\name{getSamples}
\alias{getSamples}
\alias{getSamples-methods}
\alias{getSamples,flowJoWorkspace-method}
\alias{getSamples,GatingSet-method}
\title{Get a list of samples in a flowJo workspace or a GatingSet}
\arguments{
  \item{x}{A \code{flowJoWorkspace} or a \code{GatingSet}}

  \item{isFullPath}{\code{isFullPath} is a logical value
  indicating whether the full path of the sample file is
  returned.Default is FALSE.}
}
\value{
  A \code{data.frame} with columns \code{sampleID},
  \code{name}, and \code{compID} if \code{x} is a
  \code{flowJoWorkspace}. A character vector of sample
  names if \code{x} is a \code{GatingSet}.
}
\description{
  Return a data frame of samples contained in a flowJo
  workspace or a GatingSet
}
\details{
  Returns a \code{data.frame} of samples in the
  \code{flowJoWorkspace}, including their \code{sampleID},
  \code{name}, and \code{compID} (compensation matrix ID).
  If \code{x} is a \code{GatingSet}, returns a character
  vector of sample names. The sample names comes from pdata
  of fs.
}
\examples{
\dontrun{
        #G is  a GatingSet
        getSamples(G)
        #f is a flowJoWorkspace
        getSamples(f);
      }
}

