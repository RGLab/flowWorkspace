#' get all the descendant nodes for the given ancester
#'
#' @param gh GatingHierarchy
#' @param node the node path
#' @param ... passed to \code{getNode} call
#' @export
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' getDescendants(gs[[1]], "CD4")
#' getDescendants(gs[[1]], "CD8", path = "auto")
getDescendants <- function(gh, node, ...){
 descendants.id <- .getDescendants(gh@pointer, sampleNames(gh), node)
 getNodes(gh, showHidden = T, ...)[descendants.id+1]

}