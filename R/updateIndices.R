#' @templateVar old updateIndices
#' @templateVar new gh_pop_set_indices
#' @template template-depr_pkg
NULL

#' @export
setMethod("updateIndices",
          signature=signature(obj="GatingHierarchy",y="character",z="logical"),
          definition=function(obj,y,z)
          {
            .Deprecated("gh_pop_set_indices")
            gh_pop_set_indices(obj, y, z)

})

#' directly update event indices without changing gates
#'
#' It is useful when we want to alter the popluation at events level yet
#' without removing or adding the existing gates.
#'
#' @name gh_pop_set_indices
#' @aliases updateIndices updateIndices,GatingHierarchy,character,logical-method
#' @param obj \code{GatingHierarchy} object
#' @param y \code{character} node name or path
#' @param z \code{logical} vector as local event indices relative to node \code{y}
#' @export
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh <- gs[[1]]
#' #get pop counts
#' pop.stats <- gh_pop_get_stats(gh, nodes = c("CD3+", "CD4", "CD8"))
#' pop.stats
#'
#' # subsample 30% cell events at CD3+ node
#' total <- gh_pop_get_count(gh, "root")
#' gInd <- seq_len(total) #create integer index for cd3
#' gInd <- sample.int(total, size = total * 0.3) #randomly select 30%
#' #convert it to logicle index
#' gInd.logical <- rep(FALSE, total)
#' gInd.logical[gInd] <- TRUE
#' #replace the original index stored at GatingHierarchy
#' gh_pop_set_indices(gh, "CD3+", gInd.logical)
#' #check the updated pop counts
#' gh_pop_get_stats(gs[[1]], nodes = c("CD3+", "CD4", "CD8")) #note that CD4, CD8 are not updated
#' #update all the descendants of CD3+
#' nodes <- gh_pop_get_descendants(gh, "CD3+")
#' for (node in nodes) suppressMessages(recompute(gh, node))
#' gh_pop_get_stats(gs[[1]], nodes = c("CD3+", "CD4", "CD8")) #now all are update to date
#' @export
gh_pop_set_indices <- function(obj,y,z)
          {
            parent <- gh_pop_get_parent(obj, y)
  
            idx <- gh_pop_normalize_idx(obj, parent, z)
            nodeID <- .getNodeInd(obj, y)
            .gh_pop_set_indices(obj, nodeID, idx)
            
          }

.gh_pop_set_indices <- function(obj, nodeID, idx)
{
  #added it to gating tree
  sn <- sampleNames(obj)
  ptr <- obj@pointer

  cpp_setIndices(ptr, sn, nodeID-1, idx)
  
}


#' convert idx into global one when needed
#' @noRd
gh_pop_normalize_idx <- function(gh, parent, idx)
{
  
  #convert to global one by combining it with parent indice
  pInd.logical <- gh_pop_get_indices(gh, parent)
  # browser()
  #convert it to  global ind 
  if(length(idx) < length(pInd.logical))
  {
    pInd.int <- which(pInd.logical)
    if(length(idx) != length(pInd.int))
      stop("the length of  the logical indices ", length(idx), " does not match to the parent events number ", length(pInd.int))
    pInd.logical[pInd.int] <- idx
    idx <- pInd.logical
  }
  idx
}