\name{plotGate}
\alias{plotGate}
\alias{plotGate-methods}
\alias{plotGate,GatingHierarchy,character-method}
\alias{plotGate,GatingHierarchy,numeric-method}
\alias{plotGate,GatingHierarchy,missing-method}
\alias{plotGate,GatingSet,numeric-method}
\alias{plotGate,GatingSet,character-method}
\alias{plotGate,GatingSet,missing-method}
\alias{plotGate_labkey}

\title{
Plot a Gate and Cell Population
}
\description{
Plots a flowJo gate and associated cell population using it's \code{flowCore} definition contained in a \code{GatingHierarchy}
}
\usage{
\S4method{plotGate}{GatingHierarchy,character}(x, y,...)
\S4method{plotGate}{GatingHierarchy,numeric}(x, y, bool=FALSE,main=getSample(x),arrange=TRUE,merge=TRUE,...)
\S4method{plotGate}{GatingSet,character}(x, y, lattice=TRUE,bool=FALSE,merge=TRUE,...)
}
\arguments{
  \item{x}{
 A \code{GatingHierarchy}
}
  \item{y}{
A \code{character} or \code{numeric} representing the node in the \code{GatingHierarchy}. Nodes can be accessed with \code{getNodes(GatingHierarchy)}.
}
  \item{bool}{
\code{TRUE|FALSE} logical specifying whether to plot boolean gates.
}
\item{main}{
\code{character}, The main title of the plot. Default is the sample name.
}
\item{arrange}{
\code{TRUE|FALSE} logical indicating whether to arrange different populations/nodes on the same page via \code{grid.arrange} call.
}
\item{merge}{
\code{TRUE|FALSE} logical indicating whether to draw multiple gates on the same plot if these gates share the same parent population and same x,y dimensions/parameters;
}

\item{lattice}{
\code{TRUE|FALSE} logical indicating whether to draw one node/gate on multiple samples on the same page through lattice plot;

}


\item{...}{
formula 
	\code{formula} a formula passed to \code{xyplot} function of \code{flowViz}, by default it is NULL, which means the formula is generated according to the x,y parameters associated with gate.

cond 
	\code{character} the conditioning variable to be passed to lattice plot.

The other additional arguments can also to be passed to \code{xyplot} function of \code{flowViz}.
}

}
\details{
The function will plot the gate if the gating hierarchy represented by \code{x} has been \code{execute()}'d. That is to say, the associated data has been loaded, compensated, transformed, and had the gates applied to it. If the data has not been gated, \code{plotGate} will print a message, and return without plotting anything. 
When applied to a \code{GatingHierarchy},\code{arrange} is set as TRUE, then all the gates associated with it are plotted as different panel on the same page.
If \code{arrange} is FALSE, then it plots one gate at a time.
By default ,\code{merge} is set as TRUE, plot multiple gates on the same plot when they share common parent population and axis.
When applied to a \code{GatingSet}, if lattice is TRUE,it plots one gate (multiple samples) per page , otherwise, one sample (with multiple gates) per page.   


}
\value{
return a trellis object if arrange is FALSE, 
 
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
