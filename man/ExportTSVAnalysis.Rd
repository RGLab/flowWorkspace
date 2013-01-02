\name{ExportTSVAnalysis}
\alias{ExportTSVAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export a GatingSet as a set of TSV (tab separated value) files for statistics, gates, and plots.
}
\description{
This function exports a GatingSet as a set of TSV files for import by other tools such as LabKey.
}
\usage{
ExportTSVAnalysis(x = NULL, Keywords = NULL, EXPORT = "export")
}
\arguments{
  \item{x}{
A \code{GatingSet} to be exported.
}
  \item{Keywords}{
A \code{character} vector with a set of keywords used to annotate the GatingSet. (currently not used)
}
  \item{EXPORT}{
A \code{character} vector. The directory into which the exported files will be placed.
}
}
\details{
The function generates two tsv files. The first statistics.tsv has the sample names, population names, sample counts and fraction of parent population for all samples and populations in the GatingSet. The second gates.tsv has the population name, sample name, and path to the plots for each gate/population of each sample in the GatingSet. Inside the EXPORT directory it creates a subdirectory for each sample, named after that sample. The subdirectories hold png plots of the gates / populations defined for those samples. The names of the png files are based on the md5 hash of the png file itself. File names are in the graphs.tsv file.
}
\value{
The function doesn't return anything.
}
\author{
Greg Finak <gfinak@fhcrc.org>
}

\seealso{
\code{\link{GatingSet-class}}
}
\keyword{manip}