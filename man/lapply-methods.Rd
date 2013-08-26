\name{lapply}
\alias{lapply}
\title{apply \code{FUN} to each sample (i.e. \code{GatingHierarchy})}
\arguments{
  \item{X}{\code{GatingSet} or \code{GatingSetList} object}

  \item{FUN}{\code{function} to apply}

  \item{level}{\code{numeric}. When \code{X} is a
  \code{GatingSetList}, \code{level} 2 (default value)
  \code{FUN} is applied to each individual
  \code{GatingHierarchy}. When it is set to 1, \code{FUN}
  is applied to each \code{GatingSet}}
}
\description{
  sample names are used for names of the returned list
}

