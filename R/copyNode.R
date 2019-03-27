#' @templateVar old copyNode
#' @templateVar new gh_copy_gate
#' @template template-depr_pkg
NULL
#' Copy a node along with all of its descendant nodes to the given ancestor
#'
#' @param gh GatingHierarchy
#' @param node the node to be copied
#' @param to the new parent node under which the \code{node} will be copied
#' @export
#' @rdname copyNode
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh <- gs[[1]]
#' old.parent <- gs_get_parent(gh, "CD4")
#' new.parent <- "singlets"
#' copyNode(gh, "CD4", new.parent)
#' gs_get_pop_paths(gh)
copyNode <- function(gh, node, to){
  .Defunct("gh_copy_gate")
}
#' @export
#' @rdname copyNode
gh_copy_gate<- function(gh, node, to){
  node <- gh_convert_node_full_path(gh, node)
  to <- gh_convert_node_full_path(gh, to)
  if(to == node){
    stop("Can't copy the node to itself!")
  }
  if(to %in% getDescendants(gh, node, path = "full")){
    stop("Can't copy the node to its descendants!")
  }
  .copyNode(gh, node, to)
  recompute(gh, to)
  invisible(node)
}

.copyNode <- function(gh, node, to){
  children <- gs_get_children(gh, node, path = "full", showHidden = TRUE)
  node_name <- basename(node)
  gh_add_gate(gh, action = getGate(gh, node), negated = isNegated(gh, node), 
      parent = to, name = node_name, recompute = FALSE)
  added <- paste(to, node_name, sep = "/")
  if(isHidden(gh, node)){
    setNode(gh, added, FALSE)
  }
  if(length(children)){
    lapply(children, function(x) .copyNode(gh, x, added))
  }
}
