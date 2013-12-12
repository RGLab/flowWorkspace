\name{pData}
\alias{pData}
\alias{pData,GatingSet-method}
\alias{pData<-,GatingSet,data.frame-method}
\alias{pData<-,GatingSetList,data.frame-method}
\title{read/set pData of flow data associated with \code{GatingSet} or \code{GatingSetList}}
\arguments{
  \item{object}{\code{GatingSet} or \code{GatingSetList}}

  \item{value}{\code{data.frame} The replacement of pData
  for \code{flowSet} or \code{ncdfFlowSet} object}
}
\value{
  a \code{data.frame}
}
\description{
  Accessor method that gets or replaces the pData of the
  flowset/ncdfFlowSet object in a GatingSet or
  GatingSetList
}

