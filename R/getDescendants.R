#' @templateVar old getDescendants
#' @templateVar new gh_pop_get_descendants
#' @template template-depr_pkg
NULL
#' get all the descendant nodes for the given ancester
#' 
#' @name gh_pop_get_descendants
#' @aliases getDescendants getDescendants,GatingHierarchy-method
#' @param gh GatingHierarchy
#' @param node the node path
#' @param showHidden whether show hidden nodes
#' @param ... passed to \code{getNode} call
#' @export
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh_pop_get_descendants(gs[[1]], "CD4")
#' gh_pop_get_descendants(gs[[1]], "CD8", path = "auto")
gh_pop_get_descendants <- function(gh, node, showHidden = TRUE, ...){
 descendants.id <- getDescendants_cpp(gh@pointer, sampleNames(gh), node)
 res <- gs_get_pop_paths(gh, showHidden = T, ...)[descendants.id+1]
 if(!showHidden)
   res <- Filter(function(i)!gh_pop_is_hidden(gh, i), res)
 res
}

#' @export
getDescendants <- function(...){
  .Deprecated("gh_pop_get_descendants")
  gh_pop_get_descendants(...)
} 