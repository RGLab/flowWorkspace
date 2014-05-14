\name{getPopStats}
\alias{getPopStats}
\alias{getPopStats-methods}
\alias{getPopStats,GatingHierarchy-method}
\alias{getPopStats,GatingSet-method}
\alias{getProp}
\alias{getProp-methods}
\alias{getProp,GatingHierarchy,character-method}
\alias{getTotal}
\alias{getTotal-methods}
\alias{getTotal,GatingHierarchy,character-method}
\title{Return a table of population statistics for all populations in a GatingHierarchy/GatingSet
  or the population proportions or the total number of events of a node (population) in a GatingHierarchy}
\arguments{
  \item{x}{A \code{GatingHierarchy} or \code{GatingSet}}

  \item{statistic}{\code{character} specifies the type of
  population statistics to extract. Either "freq" or
  "count" is currently supported.}

  \item{flowJo}{\code{logical} indicating whether the
  statistics come from FlowJo (if parsed from xml
  workspace) or from flowCore.}

  \item{...}{Additional arguments passed to
  \link{getNodes}}
}
\value{
  getPopStats returns a \code{data.frame} with columns for
  the population name, flowJo derived counts, flowCore
  derived counts, and the population proportions (relative
  to their parent pouplation). getProp returns a population
  frequency \code{numeric}. getTotal returns a
  \code{numeric} value of the total number of elements in
  the population.
}
\description{
  getProp calculates the population proportion (events in
  the gate / events in the parent population) associated
  with a node in the \code{GatingHierarchy}. getPopStats is
  more useful than getPop. Returns a table of population
  statistics for all populations in a
  \code{GatingHierarchy}/\code{GatingSet}. Includes the
  flowJo counts, flowCore counts and frequencies. getTotal
  returns the total number of events in the gate defined in
  the GatingHierarchy object
}
\details{
  getPopStats returns a table population statistics for all
  populations in the gating hierarchy. The output is useful
  for verifying that the import was successful, if the
  flowJo and flowCore derived counts don't differ much
  (i.e. if they have a small coefficient of variation.) for
  a GatingSet, returns a matrix of proportions for all
  populations and all samples getProp returns the
  proportion of cells in the gate, relative to its parent.
  getTotal returns the total number of events included in
  this gate. The contents of "thisTot" variable in the
  "metadata" environment of the \code{nodeData} element
  associated with the gating tree and gate / population.
}
\examples{
\dontrun{
        #If gh is a GatingHierarchy
        getPopStats(gh);
        #proportion for the fifth population
        getProp(G,getNodes(gh)[5])
        getTotal(G,getNodes(G,tsort=T)[5])

        }
}
\seealso{
  \code{\link{getNodes}}
}

