#' move a node along with all of its descendant nodes to the given ancester
#' 
#' @name gh_pop_move
#' @aliases moveNode
#' @param gh GatingHierarchy
#' @param node the node to be moved
#' @param to the new parent node under which the \code{node} will be moved to
#' @param recompute whether to recompute the gates after the node is moved. Default is TRUE.
#' @export
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh <- gs[[1]]
#' old.parent <- gs_pop_get_parent(gh, "CD4")
#' new.parent <- "singlets"
#' gh_pop_move(gh, "CD4", new.parent)
#' gs_pop_get_parent(gh, "CD4")
#' @export
gh_pop_move <- function(gh, node, to, recompute = TRUE){
  
  moveNode(gh@pointer, sampleNames(gh), node, to)
  if(recompute)
    recompute(gh, to)

}