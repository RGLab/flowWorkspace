# copied from plyr to avoid the dependency on plyr
compact <- function (l)
  Filter(Negate(is.null), l)

LdFlags <- function(lib=c("pb", "flowWorkspace", "all")){
  lib <- match.arg(lib)
  libs <- c("/libflowWorkspace.a", "/libprotobuf.a")
  if(lib == "pb")
    lib <- libs[2]
  else if(lib == "flowWorkspace")
    lib <- libs[1]
  else
    lib <- libs

  libpaths <- paste0("lib", Sys.getenv("R_ARCH"), lib)
  libpaths <- lapply(libpaths, function(libpath)tools::file_path_as_absolute(base::system.file(libpath, package = "flowWorkspace" )))
  cat(paste(libpaths, collapse = " "))
}


#' Get Cell Population Statistics and Sample Metadata
#'
#' @param object a \code{GatingSet} or \code{GatingSetList}
#' @param ... additional arguments passed to \code{getPopStats}
#'
#' @return a \code{data.table} of merged population statistics with sample metadata.
#' @export
#' @importFrom dplyr inner_join
#' @examples
#'  \dontrun{
#'     #G is a GatingSetList
#'     stats = getMergedStats(G)
#'   }
getMergedStats = function(object,...){
	if(!inherits(object,"GatingSet")&!inherits(object,"GatingSetList")){
		stop("object must be a GatingSet or GatingSetList")
	}
	stats = getPopStats(object,...)
	#process name column so that it contains XXX.fcs
	message("Processing sample names..")
	stats[, sampleName:=name]
	stats[,name := gsub("(fcs).*","\\1",name)]
	pd = pData(object)
	message("merging..")
	ret = inner_join(stats,pd,by="name")
	message("Done")
	return(ret)
}

