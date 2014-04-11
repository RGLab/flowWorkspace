\name{sampleNames}
\alias{sampleNames}
\alias{sampleNames,GatingSet-method}
\alias{sampleNames<-,GatingSet-method}
\title{Get/update sample names in a GatingSet}
\arguments{
  \item{object}{or a \code{GatingSet}}

  \item{value}{\code{character} new sample names}
}
\value{
  A character vector of sample names
}
\description{
  Return a sample names contained in a GatingSet
}
\details{
  The sample names comes from pdata of fs.
}
\examples{
\dontrun{
        #G is  a GatingSet
        sampleNames(G)
      }
}

