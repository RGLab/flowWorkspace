\name{parseWorkspace}
\alias{parseWorkspace}
\alias{parseWorkspace-methods}
\alias{parseWorkspace,flowJoWorkspace-method}
\title{
Parse a flowJo Workspace
}
\description{
Function to parse a flowJo Workspace, generate a \code{GatingHierarchy} or \code{GatingSet} object, and associated flowCore gates. The data are not loaded or acted upon until an explicit call to \code{execute()} is made on the \code{GatingHierarchy} objects in the \code{GatingSet}.
}
\usage{
\S4method{parseWorkspace}{flowJoWorkspace}(obj,name=NULL,execute=FALSE,isNcdf=FALSE,subset=NULL,nslaves=4,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
A \code{flowJoWorkspace} to be parsed.
}
	\item{name}{
	\code{numeric} or \code{character}. The name or index of the group of samples to be imported. If \code{NULL}, the groups are printed to the screen and one can be selected interactively. Usually, multiple groups are defined in the flowJo workspace file.
	}
	\item{execute}{
	\code{TRUE|FALSE} a logical specifying if the gates, transformations, and compensation should be immediately calculated after the flowJo workspace have been imported. You should probably set this to TRUE.
	}
	\item{isNcdf}{
	\code{TRUE|FALSE} logical specifying if you would like to use netcdf to store the data, or if you would like to keep all the flowFrames in memory. For a small data set, you can safely set this to FALSE, but for larger data, we suggest using netcdf. You will need the netcdf C library installed.
	}
	\item{subset}{
	\code{numeric} vector specifying the subset of samples in a group to import.
	}
	\item{nslaves}{
	\code{numeric} number of slave processes for executing the gating under Rmpi
	}
	\item{...}{
	Additional arguments.
	\code{path="character"}{
	The path to the fcs files that are to be imported. The code will search recursively, so you can point it to a location above the files. This argument is mandatory.
	}
	}
}
\details{
A flowJoWorkspace is generated with a call to \code{openWorkspace()}, passing the name of the xml workspace file. This returns a \code{flowJoWorkspace}, which can be parsed using the \code{parseWorkspace()} method. The function can be called non-interactively by passing the index or name of the group of samples to be imported via \code{parseWorkspace(obj,name=x)}, where \code{x} is either the numeric index, or the name.
}
\value{
Returns a \code{GatingSet}, which is a wrapper around a list of \code{GatingHierarchy} objects, each representing a single sample in the workspace. The \code{GatingHierarchy} objects contain \code{graphNEL} trees that  represent the gating hierarchy of each sample. Each node in the \code{GatingHierarchy} has associated data, including the population counts from flowJo, the parent population counts, the \code{flowCore} gates generated from the flowJo workspace gate definitions. Data are not yet loaded or acted upon at this stage. To execute the gating of each data file, a call to \code{execute()} must be made on each \code{GatingHierarchy} object in the \code{GatingSet}. These can be iterated over using \code{lapply()}. Options to \code{execute()} allow the user to specify whether compensated and transformed data should be kept in memory for each sample, whether indices specifying the population membership of each event at each node in the \code{GatingHierarchy} should be kept in memory, or whether these should be deleted after each node is processed and only summary statistics kept associated with each population. The latter option forgoes the ability to plot the data, and is more useful in a high--throughput, large scale setting where keeping the data around in memory can be inefficient.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link{getSampleGroups}}
}
\examples{
\dontrun{
	#f is a xml file name of a flowJo workspace
	ws<-openWorkspace(f)
	G<-parseWorkspace(ws,execute=TRUE,isNcdf=FALSE,path="."); #assume that the fcs files are below the current directory.
	#G is a GatingSet.
}}
\keyword{ manip }
