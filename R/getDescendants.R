#' @templateVar old getDescendants
#' @templateVar new gh_pop_get_descendants
#' @template template-depr_pkg
NULL
#' get all the descendant nodes for the given ancester
#'
#' @param gh GatingHierarchy
#' @param node the node path
#' @param ... passed to \code{getNode} call
#' @export
#' @rdname gh_pop_get_descendants
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh_pop_get_descendants(gs[[1]], "CD4")
#' gh_pop_get_descendants(gs[[1]], "CD8", path = "auto")
gh_pop_get_descendants <- function(gh, node, ...){
 descendants.id <- .getDescendants(gh@pointer, sampleNames(gh), node)
 gs_get_pop_paths(gh, showHidden = T, ...)[descendants.id+1]

}

#' @export
#' @rdname gh_pop_get_descendants
getDescendants <- function(...){
  .Deprecated("gh_pop_get_descendants")
  gh_pop_get_descendants(...)
} 