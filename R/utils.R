#' get all the leaf nodes
#' @param x GatingHierarchy/GatingSet object
#' @param ... arguments passed to 'getNodes" method
#' @return the leaf nodes
#' @export 
get_leaf_nodes <- function(x, ...){
  res <- getNodes(x, ...)
  ind <- sapply(res, function(i)length(getChildren(x, i)) == 0, simplify = TRUE)
  res[ind]
}


# copied from plyr to avoid the dependency on plyr
compact <- function (l)
  Filter(Negate(is.null), l)

LdFlags <- function(){
 
  lib <- "/libflowWorkspace.a"
    
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

#' save the event counts parsed from xml into c++ tree structure
#'
#' It is for internal use by the diva parser
#'
#' @param gh GatingHierarchy
#' @param node the unique gating path that uniquely identifies a population node
#' @param count integer number that is events count for the respective gating node directly parsed from xml file
#' @export
#' @examples
#' \dontrun{
#' set.count.xml(gh, "CD3", 10000)
#' }
set.count.xml <- function(gh, node, count){
  .set.count.xml(gh@pointer, sampleNames(gh), node, count)
}