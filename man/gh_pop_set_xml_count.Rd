% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{gh_pop_set_xml_count}
\alias{gh_pop_set_xml_count}
\title{save the event counts parsed from xml into c++ tree structure}
\usage{
gh_pop_set_xml_count(gh, node, count)
}
\arguments{
\item{gh}{GatingHierarchy}

\item{node}{the unique gating path that uniquely identifies a population node}

\item{count}{integer number that is events count for the respective gating node directly parsed from xml file}
}
\description{
It is for internal use by the diva parser
}
\examples{
\dontrun{
gh_pop_set_xml_count(gh, "CD3", 10000)
}
}
