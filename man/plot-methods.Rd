\name{plot}
\alias{plot}
\alias{plot,GatingHierarchy,character-method}
\alias{plot,GatingHierarchy,missing-method}
\alias{plot,GatingHierarchy,numeric-method}
\title{plot a GatingHierarchy}
\arguments{
  \item{x}{\code{GatingHierarchy} to be plotted}

  \item{y}{\code{missing}.}

  \item{...}{other arguments: \itemize{ \item{layout}: See
  \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
  \item{width}: See \code{\link[Rgraphviz]{layoutGraph}} in
  package Rgraphviz \item{height}: See
  \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
  \item{fontsize}: See \code{\link[Rgraphviz]{layoutGraph}}
  in package Rgraphviz \item{labelfontsize}: See
  \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
  \item{fixedsize}: See
  \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
  }}

  \item{boolean}{\code{TRUE|FALSE} logical specifying
  whether to plot boolean gate nodes. Defaults to FALSE.}
}
\description{
  Plot a tree/graph representing the GatingHierarchy

  plot a subgraph
}
\examples{
\dontrun{
	#G is a GatingHierarchy
	plot(G);
}
}

