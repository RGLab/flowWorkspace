#' @templateVar old copyNode
#' @templateVar new gh_copy_gate
#' @template template-depr_pkg
NULL

#' @export
copyNode <- function(gh, node, to){
  .Defunct("gh_copy_gate")
}

#' Copy a node along with all of its descendant nodes to the given ancestor
#' 
#' @name gh_copy_gate
#' @aliases copyNode
#' @param gh GatingHierarchy
#' @param node the node to be copied
#' @param to the new parent node under which the \code{node} will be copied
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh <- gs[[1]]
#' old.parent <- gs_pop_get_parent(gh, "CD4")
#' new.parent <- "singlets"
#' gh_copy_gate(gh, "CD4", new.parent)
#' gs_get_pop_paths(gh)
#' @export
gh_copy_gate<- function(gh, node, to){
  node <- gh_pop_get_full_path(gh, node)
  to <- gh_pop_get_full_path(gh, to)
  if(to == node){
    stop("Can't copy the node to itself!")
  }
  if(to %in% gh_pop_get_descendants(gh, node, path = "full")){
    stop("Can't copy the node to its descendants!")
  }
  .copyNode(gh, node, to)
  recompute(gh, to)
  invisible(node)
}

.copyNode <- function(gh, node, to){
  children <- gs_pop_get_children(gh, node, path = "full", showHidden = TRUE)
  node_name <- basename(node)
  pop_add(gh_pop_get_gate(gh, node), gh, negated = gh_pop_is_negated(gh, node), 
      parent = to, name = node_name, recompute = FALSE)
  added <- paste(to, node_name, sep = "/")
  if(gh_pop_is_hidden(gh, node)){
	  gh_pop_set_visibility(gh, added, FALSE)
  }
  if(length(children)){
    lapply(children, function(x) .copyNode(gh, x, added))
  }
}
