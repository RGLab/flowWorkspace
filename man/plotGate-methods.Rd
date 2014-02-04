\docType{methods}
\name{plotGate}
\alias{plotGate}
\alias{plotGate-methods}
\alias{plotGate,GatingHierarchy,character-method}
\alias{plotGate,GatingHierarchy,missing-method}
\alias{plotGate,GatingHierarchy,numeric-method}
\alias{plotGate,GatingSet,character-method}
\alias{plotGate,GatingSet,missing-method}
\alias{plotGate,GatingSet,numeric-method}
\alias{plotGate,GatingSetList,character-method}
\alias{plotGate,GatingSetList,numeric-method}
\title{Plot gates and associated cell population contained in a \code{GatingHierarchy} or \code{GatingSet}}
\arguments{
  \item{x}{\code{\linkS4class{GatingSet}} or
  \code{\linkS4class{GatingHierarchy}}object}

  \item{y}{\code{character} the node name or full(/partial)
  gating path or \code{numeric} representing the node index
  in the \code{GatingHierarchy}.  or \code{missing} which
  will plot all gates and one gate per page. It is useful
  for generating plots in a multi-page pdf.  Nodes can be
  accessed with \code{\link{getNodes}}.}

  \item{...}{\itemize{ \item{bool}{\code{logical}
  specifying whether to plot boolean gates.}
  \item{arrange.main}{\code{character} The title of the
  main page of the plot. Default is the sample name. Only
  valid when \code{x} is GatingHierarchy}
  \item{arrange}{\code{logical} indicating whether to
  arrange different populations/nodes on the same page via
  \code{grid.arrange} call.} \item{merge}{\code{logical}
  indicating whether to draw multiple gates on the same
  plot if these gates share the same parent population and
  same x,y dimensions/parameters;}
  \item{par.settings}{\code{list} of graphical parameters
  passed to \code{\link{lattice}};} \item{gpar}{\code{list}
  of grid parameters passed to \code{\link{grid.layout}};}
  \item{lattice}{\code{logical} deprecated;}
  \item{formula}{\code{formula} a formula passed to
  \code{xyplot} function of \code{flowViz}, by default it
  is NULL, which means the formula is generated according
  to the x,y parameters associated with gate.}
  \item{cond}{\code{character} the conditioning variable to
  be passed to lattice plot.} \item{overlay}{\code{numeric}
  scalar indicating the index of a gate/populationwithin
  the \code{GatingHierarchy} or a \code{logical} vector
  that indicates the cell event indices representing a
  sub-cell population. This cell population is going to be
  plotted on top of the existing gates(defined by \code{y}
  argument) as an overlay.}
  \item{default.y}{\code{character} specifiying y channel
  for xyplot when plotting a 1d gate. Default is "SSC-A".}
  \item{type}{\code{character} either "xyplot" or
  "densityplot". Default is "xyplot"} \item{fitGate}{used
  to disable behavior of plotting the gate region in 1d
  densityplot} \item{strip}{\code{ligcal} specifies whether
  to show pop name in strip box,only valid when x is
  \code{GatingHierarchy}} \item{marker.only}{\code{ligcal}
  specifies whether to show both channel and marker names }
  \item{...}{The other additional arguments to be passed to
  \link[flowViz]{xyplot}.} }}
}
\value{
  a \code{trellis} object if \code{arrange} is
  \code{FALSE},
}
\description{
  When applied to a \code{GatingHierarchy},\code{arrange}
  is set as TRUE, then all the gates associated with it are
  plotted as different panel on the same page. If
  \code{arrange} is FALSE, then it plots one gate at a
  time. By default ,\code{merge} is set as TRUE, plot
  multiple gates on the same plot when they share common
  parent population and axis. When applied to a
  \code{GatingSet}, if lattice is TRUE,it plots one gate
  (multiple samples) per page , otherwise, one sample (with
  multiple gates) per page.
}
\examples{
\dontrun{
	#G is a GatingHierarchy
	plotGate(G,getNodes(G)[5]);#plot the gate for the  fifth node
}
}
\references{
  \url{http://www.rglab.org/}
}

