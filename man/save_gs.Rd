\name{save_gs}
\alias{save_gs}
\alias{load_gs}
\alias{save_gslist}
\alias{load_gslist}

\title{
save/load a GatingSetInternal/GatingSetList to/from disk.
}
\description{
Save/load a GatingSetInternal/GatingSetList which is the gated flow data including gates and populations to/from the disk. 
The \code{GatingSetInternal} object The internal C data structure (gating tree),\code{ncdfFlowSet} object(if applicable) 
}
\usage{
save_gs(G,path,overwrite = FALSE,...) 
load_gs(path)

save_gslist(gslist,path,overwrite = FALSE,...)
load_gslist(path)
}
\arguments{
  
  \item{G}{ A \code{GatingSetInternal}}
  
  \item{gslist}{ A \code{GatingSetList}}
  
  \item{path}{A character scalar giving the path to save/load the GatingSet to/from.}

  \item{overwrite}{A logical scalar specifying whether to overwrite the existing folder.}
   
   \item{...}{other arguments:
   
   			 cdf: a character scalar. The valid options are :"copy","move","skip","symlink","link" specifying what to do with the cdf data file. 
   			 				 Sometime it is more efficient to move or create a link of the existing cdf file to the archived folder.
   			 				 It is useful to "skip" archiving cdf file if raw data has not been changed.  
  
   				}    					
}

\value{
  
  \code{load_gs} returns a GatingSetInternal object
  \code{load_gslist} returns a GatingSetList object
  
}

\author{
Mike Jiang


Maintainer: Mike Jiang <wjiang2@fhcrc.org>
}

\seealso{
\code{\link{GatingSetInternal-class}},\code{\link{GatingSetList-class}}
}

\examples{
\dontrun{
	#G is a GatingSet
	save_gs(G,path="tempFolder")
	G1<-load_gs(path="tempFolder")
	
	#G is a GatingSet
	
	save_gslist(gslist1,path="tempFolder")
	gslist2<-load_gslist(path="tempFolder")
}

}
\keyword{IO}

