#' @templateVar old moveNode
#' @templateVar new gh_move_node
#' @template template-depr_pkg
NULL
#' move a node along with all of its descendant nodes to the given ancester
#'
#' @param gh GatingHierarchy
#' @param node the node to be moved
#' @param to the new parent node under which the \code{node} will be moved to
#' @export
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh <- gs[[1]]
#' old.parent <- gs_get_parent(gh, "CD4")
#' new.parent <- "singlets"
#' gh_move_node(gh, "CD4", new.parent)
#' gs_get_parent(gh, "CD4")
#' @rdname gh_move_node
#' @export
moveNode <- function(...){
  .Deprecated("gh_move_node")
  gh_move_node(...)
}
#' @rdname gh_move_node
#' @export
gh_move_node <- function(gh, node, to){
  
  .moveNode(gh@pointer, sampleNames(gh), node, to)
  recompute(gh, to)

}