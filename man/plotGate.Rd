\name{plotGate}
\alias{plotGate}
\alias{plotGate-methods}
\alias{plotGate,GatingHierarchy,character-method}
\alias{plotGate,GatingHierarchy,numeric-method}
\title{
Plot a flowJo Gate and Cell Population
}
\description{
Plots a flowJo gate and associated cell population using it's \code{flowCore} definition contained in a \code{GatingHierarchy}
}
\usage{
\S4method{plotGate}{GatingHierarchy,character}(x, y, add=FALSE,border="red",tsort=FALSE,smooth=FALSE,fast=FALSE,...)
}
\arguments{
  \item{x}{
 A \code{GatingHierarchy}
}
  \item{y}{
A \code{character} or \code{numeric} representing the node in the \code{GatingHierarchy}. Nodes can be accessed with \code{getNodes(GatingHierarchy)}.
}
  \item{add}{
\code{TRUE|FALSE} logical specifying whether to add the gate to the current plot.
}
\item{border}{
\code{character}, The color to plot the border of the gate. Default is "red".
}
\item{tsort}{
\code{TRUE|FALSE} logical indicating if nodes should be referenced in topological sort order when \code{y="numeric"};
}
\item{smooth}{
\code{TRUE|FALSE} logical indicating whether a smoothed 2D scatterplot should be drawn;
}
\item{fast}{
\code{TRUE|FALSE} logical indicating whether or not to use hexbin plotting routines, which are a bit faster.
}
\item{...}{
Additional arguments to the \code{plot} function.
}

}
\details{
The function will plot the gate if the gating hierarchy represented by \code{x} has been \code{execute()}'d. That is to say, the associated data has been loaded, compensated, transformed, and had the gates applied to it. If the data has not been gated, \code{plotGate} will print a message, and return without plotting anything. 
}
\value{
If the data has been gated, the function will plot the gate. If it has not been gated, the function will print a message and return nothing. 
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}
\examples{
\dontrun{
	#G is a GatingHierarchy
	plotGate(G,getNodes(G)[5]);#plot the gate for the  fifth node
}
}
\keyword{ hplot }
