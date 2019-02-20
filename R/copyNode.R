#' Copy a node along with all of its descendant nodes to the given ancestor
#'
#' @param gh GatingHierarchy
#' @param node the node to be copied
#' @param to the new parent node under which the \code{node} will be copied
#' @export
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh <- gs[[1]]
#' old.parent <- getParent(gh, "CD4")
#' new.parent <- "singlets"
#' copyNode(gh, "CD4", new.parent)
copyNode <- function(gh, node, to){
  all_paths <- getNodes(gh, showHidden = TRUE, path = "full")
  to_path <- getFullNodePath(gh, to)
  node_path <- getFullNodePath(gh, node)
  if(to_path == node_path){
    stop("Can't copy the node to itself!")
  }
  if(to_path %in% getDescendants(gh, node_path, path = "full")){
    stop("Can't copy the node to its descendants!")
  }
  children <- getChildren(gh, node, path = "full", showHidden = TRUE)
  node_name <- basename(node_path)
  add(gh, action = getGate(gh, node_path), negated = isNegated(gh, node_path), 
      parent = to_path, name = node_name, recompute = TRUE)
  added_path <- setdiff(getNodes(gh, showHidden = TRUE, path = "full"), all_paths)
  if(length(children)){
    lapply(children, function(x) copyNode(gh, x, added_path))
  }
  # Hide nodes appropriately on the way back up
  if(isHidden(gh, node_path)){
    setNode(gh, added_path, FALSE)
  }
  invisible(node)
}