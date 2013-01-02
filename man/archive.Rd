\name{archive}
\alias{archive}
\alias{unarchive}

\title{
Archive/unarchive a GatingSetInternal to/from tar files
}
\description{
Save/load a GatingSetInternal which is the gated flow data including gates and populations to/from the disk. 
The \code{GatingSetInternal} object The internal C data structure (gating tree),\code{ncdfFlowSet} object(if applicable) 
}
\usage{
archive(G,file=tempfile()) 
unarchive(file,path=tempdir())
}
\arguments{
  
  \item{G}{ A \code{GatingSetInternal}}
  
  \item{file}{A character scalar giving the output file name.}
  
  \item{path}{A character scalar giving the folder where to store cdf file(if the raw data within GatingSet is stored as ncdfFlowSet object) extracted from the archive.}
    					
}

\value{
  
  \code{unarchive} returns a GatingSetInternal object
  
}

\author{
Mike Jiang,Greg Finak,N.Gopalakrishnan


Maintainer: Mike Jiang <wjiang2@fhcrc.org>
}

\seealso{\code{\link{GatingSetInternal-class}}}

\examples{
\dontrun{
	#G is a GatingSet
	archive(G,file="gs.tar")
	G1<-unarchive(file="gs.tar")
}

}
\keyword{IO}

