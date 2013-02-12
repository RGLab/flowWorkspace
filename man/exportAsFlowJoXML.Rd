\name{exportAsFlowJoXML}
\alias{exportAsFlowJoXML}
\alias{exportAsFlowJoXML-methods}
\alias{exportAsFlowJoXML,workFlow-method}
\alias{exportAsFlowJoXML,list-method}
\alias{exportAsFlowJoXML,ellipsoidGate-method}
\alias{exportAsFlowJoXML,polygonGate-method}
\alias{exportAsFlowJoXML,rectangleGate-method}
\alias{exportAsFlowJoXML,intersectFilter-method}

\title{
Export a workFlow to FlowJo XML
}
\description{
Converts a workFlow and associated metadata to a FlowJo 9.2 OSX compatible workspace.
}
\usage{
\S4method{exportAsFlowJoXML}{workFlow}(obj, file=NULL,...)
\S4method{exportAsFlowJoXML}{list}(obj, file=NULL,...)
\S4method{exportAsFlowJoXML}{ellipsoidGate}(obj,transforms,...)
\S4method{exportAsFlowJoXML}{polygonGate}(obj,transforms,...)
\S4method{exportAsFlowJoXML}{rectangleGate}(obj,transforms,...)
\S4method{exportAsFlowJoXML}{intersectFilter}(obj,transforms,gate_view,workflow)
}
\arguments{
  \item{obj}{\code{workFlow} workFlow to be exported.}
  \item{list}{\code{list} list of workFlows to be exported.}
  \item{file}{\code{character} the name of the XML output file.}
  \item{transforms}{\code{function} the transform from raw scale to channel space}
  \item{gate_view}{\code{gate_view} the view from the flowCore workflow referencing the gates to be combined}
  \item{workflow}{\code{workflow} the flowCore workFlow this filter is used in}
  \item{...}{Additional arguments used by some S4 methods.}
}
\details{
Exports a \code{flowCore} workFlow object to an XML workspace readable by FlowJo.
}
\value{
If \code{file} is \code{NULL} it will return the string for the XML output. Otherwise it returns the file name.

}
\references{
\url{http://www.rglab.org/}
}
\author{
Mose Andre \email{mandre@fhcrc.org}
}

\seealso{
\code{\link{workFlow-class}}
}
\examples{
\dontrun{
	#Assume w is a workFlow object
	exportAsFlowJoXML(w, "export.xml")
}
}
\keyword{ manip }
