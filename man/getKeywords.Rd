\name{getKeywords}
\alias{getKeywords}
\alias{getKeywords-methods}
\alias{getKeywords,flowJoWorkspace,character-method}
\alias{getKeywords,GatingHierarchy,missing-method}
\alias{getKeywords,GatingSet,character-method}
\alias{getKeywords,GatingSet,numeric-method}
\title{Get List of Keywords for a Flow Sample}
\arguments{
  \item{obj}{A \code{flowJoWorkspace}, \code{GatingSet}, or
  \code{GatingHierarchy}}

  \item{y}{can be omitted if \code{obj} is a
  \code{GatingHierarchy}. A \code{character}, or
  \code{numeric} if \code{obj} is a \code{GatingSet}. A
  \code{character} if \code{obj} is a
  \code{flowJoWorkspace}}
}
\value{
  A list of keyword - value pairs.
}
\description{
  Retrieve the list of keywords associated with a sample
}
\details{
  Retrieve a list of keywords from a
  \code{flowJoWorkspace}, \code{GatingSet}, or
  \code{GatingHierarchy} for a particular sample. The
  sample is specified via \code{y}, either a numeric index
  into a \code{GatingSet}, or a sample name
  (\code{character}) for all other types of \code{obj}.
}
\examples{
dontrun{
      #G is a GatingHierarchy
      getKeywords(G);
      #G is a GatingSet
      getKeywords(G[[1]])
      getKeywords(G,1)
    }
}

