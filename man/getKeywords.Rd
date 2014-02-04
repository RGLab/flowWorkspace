\name{getKeywords}
\alias{getKeywords}
\alias{getKeywords-methods}
\alias{getKeywords,flowJoWorkspace,character-method}
\alias{getKeywords,GatingHierarchy,missing-method}
\alias{getKeywords,GatingSet,character-method}
\alias{getKeywords,GatingSet,numeric-method}
\title{Get Keywords}
\arguments{
  \item{obj}{A \code{flowJoWorkspace}}

  \item{y}{c\code{character} specifying the sample names}
}
\value{
  A list of keyword - value pairs.
}
\description{
  Retrieve keywords associated with a workspace
}
\details{
  Retrieve a list of keywords from a \code{flowJoWorkspace}
}
\examples{
require(flowWorkspaceData)
  d<-system.file("extdata",package="flowWorkspaceData")
  wsfile<-list.files(d,pattern="manual.xml",full=TRUE)
  ws <- openWorkspace(wsfile);

  getSamples(ws)
  getKeywords(ws,"CytoTrol_CytoTrol_1.fcs")
}

