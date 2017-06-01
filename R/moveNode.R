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
#' old.parent <- getParent(gh, "CD4")
#' new.parent <- "singlets"
#' moveNode(gh, "CD4", new.parent)
#' getParent(gh, "CD4")
moveNode <- function(gh, node, to){

  .moveNode(gh@pointer, sampleNames(gh), node, to)
  recompute(gh, to)

}