#' directly update event indices without changing gates
#'
#' It is useful when we want to alter the popluation at events level yet
#' without removing or adding the existing gates.
#'
#' @param obj \code{GatingHierarchy} object
#' @param y \code{character} node name or path
#' @param z \code{logical} vector as local event indices relative to node \code{y}
#' @export
#' @importFrom ncdfFlow updateIndices
#' @examples
#' library(flowWorkspace)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#' gh <- gs[[1]]
#' #get pop counts
#' pop.stats <- getStats(gh, nodes = c("CD3+", "CD4", "CD8"))
#' pop.stats
#'
#' # subsample 30% cell events at CD3+ node
#' total.cd3 <- pop.stats[pop == "CD3+", count]
#' gInd <- seq_len(total.cd3) #create integer index for cd3
#' gInd <- sample.int(total.cd3, size = total.cd3 * 0.3) #randomly select 30%
#' #convert it to logicle index
#' gInd.logical <- rep(F, total.cd3)
#' gInd.logical[gInd] <- TRUE
#' #replace the original index stored at GatingHierarchy
#' updateIndices(gh, "CD3+", gInd.logical)
#' #check the updated pop counts
#' getStats(gs[[1]], nodes = c("CD3+", "CD4", "CD8")) #note that CD4, CD8 are not updated
#' #update all the descendants of CD3+
#' nodes <- getDescendants(gh, "CD3+")
#' for (node in nodes) suppressMessages(recompute(gh, node))
#' getStats(gs[[1]], nodes = c("CD3+", "CD4", "CD8")) #now all are update to date
setMethod("updateIndices",
          signature=signature(obj="GatingHierarchy",y="character",z="logical"),
          definition=function(obj,y,z)
          {

            nodeID <- flowWorkspace:::.getNodeInd(obj, y)
            #get original indices
            pInd <- getIndices(obj, y)
            #update it with the new one
            #convert to global one by combining it with parent indice
            pInd[which(pInd)] <- z
            #added it to gating tree
            sn <- sampleNames(obj)
            ptr <- obj@pointer
            flowWorkspace:::.cpp_setIndices(ptr, sn, nodeID-1, pInd)
          })

