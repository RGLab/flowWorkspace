# write out refresh gating set folder by invoking open source serialization methods
# to be consumed by third party software to test the compatibility/readability (this gating set format)
generate_gatingset_example_archive <- function(){
  dataDir <- system.file("extdata",package="flowWorkspaceData")
  gs_dir <- list.files(dataDir, pattern = "gs_manual",full = TRUE)
  gs <- load_gs(gs_dir)
  tmp_local = tempfile(tmpdir = "/tmp")
  save_gs(gs, tmp_local)
  writeLines(tmp_local)
}

#' get all the leaf nodes
#' @name gs_get_leaf_nodes
#' @aliases gh_get_leaf_nodes get_leaf_nodes
#' @param x GatingHierarchy/GatingSet object
#' @param ancestor ancestor node where the leaf nodes descend from. Default is 'root'.
#' @param ... arguments passed to 'gs_get_pop_paths" method
#' @return the leaf nodes
#' @export 
gs_get_leaf_nodes <- function(x, ancestor = "root", ...){
  if(ancestor == "root")
    res <- gs_get_pop_paths(x, ...)
  else
  {
    res <- gh_pop_get_descendants(x[[1]],  ancestor, ...)
  }
  ind <- sapply(res, function(i)length(cpp_getChildren(x@pointer,sampleNames(x)[1], i, T)) == 0, simplify = TRUE)
  res[ind]
}
#' @export 
get_leaf_nodes <- function(...){
  .Deprecated("gs_get_leaf_nodes")
  gs_get_leaf_nodes(...)
}
#' @export 
#' @rdname gs_get_leaf_nodes
gh_get_leaf_nodes <- gs_get_leaf_nodes

# copied from plyr to avoid the dependency on plyr
compact <- function (l)
  Filter(Negate(is.null), l)

LdFlags <- function(){
 
  lib <- "/libflowWorkspace.a"
    
  libpaths <- paste0("lib", Sys.getenv("R_ARCH"), lib)
  libpaths <- lapply(libpaths, function(libpath)tools::file_path_as_absolute(base::system.file(libpath, package = "flowWorkspace" )))
  cat(paste(libpaths, collapse = " "))
}


#' @templateVar old getMergedStats
#' @templateVar new gs_pop_get_count_with_meta
#' @template template-depr_pkg
NULL
#' Get Cell Population Statistics and Sample Metadata
#'
#' @param x a \code{GatingSet} or \code{GatingSetList}
#' @param ... additional arguments passed to \code{gs_pop_get_count_fast}
#'
#' @return a \code{data.table} of merged population statistics with sample metadata.
#' @export
#' @importFrom dplyr inner_join
#' @examples
#'  \dontrun{
#'     #G is a GatingSetList
#'     stats = gs_pop_get_count_with_meta(G)
#'   }
#' @rdname gs_pop_get_count_fast
gs_pop_get_count_with_meta = function(x,...){
	if(!inherits(x,"GatingSet")&!inherits(x,"GatingSetList")){
		stop("x must be a GatingSet or GatingSetList")
	}
	stats = gs_pop_get_count_fast(x,...)
	#process name column so that it contains XXX.fcs
	message("Processing sample names..")
	stats[, sampleName:=name]
	stats[,name := gsub("(fcs).*","\\1",name)]
	pd = pData(x)
	message("merging..")
	ret = inner_join(stats,pd,by="name")
	message("Done")
	return(ret)
}
getMergedStats = function(...){
  .Deprecated("gs_pop_get_count_with_meta")
  gs_pop_get_count_with_meta(...)
}
#' @templateVar old set.count.xml
#' @templateVar new gh_pop_set_xml_count
#' @template template-depr_pkg
NULL
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
#' gh_pop_set_xml_count(gh, "CD3", 10000)
#' }
gh_pop_set_xml_count <- function(gh, node, count){
  setCounts_cpp(gh@pointer, sampleNames(gh), node, count)
}