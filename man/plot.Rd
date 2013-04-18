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
\S4method{plot}{GatingHierarchy,missing}(x,y,...)
}
\arguments{
  \item{x}{
	 The \code{GatingHierarchy to be plotted}
	}
	\item{y}{
	missing.
	}
	\item{...}{ other arguments:
	
		layout:
		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
		
		width:
		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
		
		
		height
		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
		
		fontsize
		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
		
		labelfontsize
		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
		
		
		fixedsize
		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
		
		
		boolean
		\code{TRUE|FALSE} logical specifying whether to plot boolean gate nodes. Defaults to FALSE.
		
		...
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
