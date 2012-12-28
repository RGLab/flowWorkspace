\name{plot}
\alias{plot}
\alias{plot-methods}
\alias{plot,GatingHierarchy,missing-method}
\alias{plot,GatingHierarchyInternal,missing-method}

\title{Plot a GatingHierarchy}
\description{
Plot a tree representing the GatingHierarchy
}
\usage{
\S4method{plot}{GatingHierarchy,missing}(x,y,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,...)
}
\arguments{
  \item{x}{
	 The \code{GatingHierarchy to be plotted}
	}
	\item{y}{
	missing.
	}
	\item{layout}{
	See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
	}
	\item{width}{
	See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
	
	}
	\item{height}{
	See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
	}
	\item{fontsize}{
	See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
	}
	\item{labelfontsize}{
	See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
	
	}
	\item{fixedsize}{
	See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
	
	}
	\item{boolean}{
	\code{TRUE|FALSE} logical specifying whether to plot boolean gate nodes. Defaults to FALSE.
	}
	\item{...}{
	Additional arguments passed to plot in Rgraphviz
	}
}
\details{
	Plot a GatingHierarchy object using the Rgraphviz plot function.
}
\value{
	Nothing to return
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}

\seealso{
\code{\link[Rgraphviz]{layoutGraph}}
}
\examples{
\dontrun{
	#G is a GatingHierarchy
	plot(G);
}
}
\keyword{ hplot }
