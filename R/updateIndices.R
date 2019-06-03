#' @templateVar old updateIndices
#' @templateVar new gh_pop_set_indices
#' @template template-depr_pkg
NULL
#' directly update event indices without changing gates
#'
#' It is useful when we want to alter the popluation at events level yet
#' without removing or adding the existing gates.
#'
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
#' total.cd3 <- pop.stats[pop == "CD3+", count]
#' gInd <- seq_len(total.cd3) #create integer index for cd3
#' gInd <- sample.int(total.cd3, size = total.cd3 * 0.3) #randomly select 30%
#' #convert it to logicle index
#' gInd.logical <- rep(FALSE, total.cd3)
#' gInd.logical[gInd] <- TRUE
#' #replace the original index stored at GatingHierarchy
#' gh_pop_set_indices(gh, "CD3+", gInd.logical)
#' #check the updated pop counts
#' gh_pop_get_stats(gs[[1]], nodes = c("CD3+", "CD4", "CD8")) #note that CD4, CD8 are not updated
#' #update all the descendants of CD3+
#' nodes <- gh_pop_get_descendants(gh, "CD3+")
#' for (node in nodes) suppressMessages(recompute(gh, node))
#' gh_pop_get_stats(gs[[1]], nodes = c("CD3+", "CD4", "CD8")) #now all are update to date
#' @rdname gh_pop_set_indices
#' @export
setMethod("updateIndices",
          signature=signature(obj="GatingHierarchy",y="character",z="logical"),
          definition=function(obj,y,z)
          {
            .Deprecated("gh_pop_set_indices")
            gh_pop_set_indices(obj, y, z)

})
#' @rdname gh_pop_set_indices
#' @export
gh_pop_set_indices <- function(obj,y,z)
          {
            nodeID <- .getNodeInd(obj, y)
            #get original indices
            pInd <- gh_pop_get_indices(obj, y)
            #update it with the new one
            #convert to global one by combining it with parent indice
            pInd[which(pInd)] <- z
            #added it to gating tree
            sn <- sampleNames(obj)
            ptr <- obj@pointer
            .cpp_setIndices(ptr, sn, nodeID-1, pInd)
          }

