\name{plotWf}
\alias{plotWf}
\title{
plot a workflow
}
\description{
Plots a workflow that contains one \code{GatingHierarchy}
}
\usage{
plotWf(x,...)
}
\arguments{
  \item{x}{
 A \code{workflow}
 }
 \item{\dots}{
Additional arguments to the \code{plot} function.
}

}
\author{
Mike Jiang
}

\examples{
\dontrun{dataDir <- system.file("extdata", package = "flowWorkspace")
wsfile<-list.files(dataDir,pattern="xml",full=TRUE)[1]

##open workspace xml file
ws<-openWorkspace(wsfile)

##Convert a flowWorkspace to workFlows 
wfs<-flowWorkspace2flowCore(ws,groupId=1,path=dataDir)
plotWf(wfs[[1]])}
}

\keyword{hplot}