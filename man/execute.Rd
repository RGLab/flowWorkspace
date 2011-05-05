\name{execute}
\alias{execute}
\alias{execute-methods}
\alias{execute,GatingHierarchy-method}
\title{
Apply the GatingHierarchy to data, computing population statistics along the way.
}
\description{
A GatingHierarchy is associated with an fcs file. Calling \code{execute} on the GatingHierarchy will load the fcs file, perform compensation and transformation, and calculate the gates as described in the flowJoWorkspace. The method is not meant to be called by the user, but is used internally by flowWorkspace.
}
\usage{
\S4method{execute}{GatingHierarchy}(hierarchy, cleanup=FALSE,keep.indices=TRUE,isNcdf=FALSE,ncfs=NULL,dataEnvironment=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hierarchy}{
A GatingHierarchy object
}
  \item{cleanup}{
\code{cleanup=TRUE|FALSE} {
When not using netcdf, this logical flag indicates whether the data should be retained in memory after processing or whether it should be scrapped to save RAM, keeping only population statistics. If you are loading a large data set, you may want to consider using netcdf, or setting this to TRUE. However, you will not be able to visualize the results. 
}
}
\item{keep.indices}{
\code{keep.indices=TRUE|FALSE}{
Logical indicating whether the indices calculated from gating should be stored, or deleted, leaving just their counts.
}
}

\item{isNcdf}{
\code{TRUE|FALSE} {
a logical flag indicating whether the data (FlowFrame) is saved on the disc in netCDF format.
}
}
\item{ncfs}{
\code{ncdfFlowSet} {
a ncdfFlowSet object created by parseWorkspace(when isNcdf is set as TRUE) which contains information of fcs metaData and netCDF file that stores the real data .
}
}
\item{dataEnvironment}{
\code{environment} for storing ncdfFlowSet common to all GatingHierarchies in a GatingSet.
}
\item{...}{Additional arguments.

\code{path="character"}{
A file path to the fcs file or files.
}
}
}
\details{
This method is not meant to be called by the user. Rather, passing \code{execute=TRUE} to \code{parseWorkspace} will execute the gating scheme after the flowJo workspace has been loaded. Cleanup is FALSE by default. This may lead to memory issues when you have lots of data, but it is necessary to visualize the analysis. Netcdf is strongly recommended.
}
\value{
Returns a GatingHierarchy with calculated population statistics and gate indices.
}
\references{
\url{http://www.rglab.org/}
}
\author{
Greg Finak \email{gfinak@fhcrc.org}
}
\note{
This function is not meant to be called by the user. Gating of samples in a flowJo workspace can be invoked by passing \code{execute=TRUE} to \code{parseWorkspace}.
}


\seealso{
\code{\link{parseWorkspace}}
}
\examples{
\dontrun{
	ws<-openWorkspace("myworkspace.xml");
	G<-parseWorkspace(ws,execute=TRUE,isNcdf=FALSE);
}
}
\keyword{ manip }
