#' @name standardize-GatingSet
#' 
#' @title 
#' The tools to standardize the tree structures and channel names.
#'
#' @description 
#' gs_split_by_tree(x)
#' 
#' gs_split_by_channels(x)
#' 
#' gs_check_redundant_nodes(x)
#' 
#' gs_remove_redundant_nodes(x, toRemove)
#' 
#' gs_remove_redundant_channels(gs)
#' 
#' gs_update_channels(gs, map, all = TRUE)
#' 
#' gh_pop_move(gh, node, to)
#' 
#' gs_pop_set_visibility(x, y, FALSE)
#' 
#' @details 
#' In order to merge multiple GatingSets into single \link{GatingSetList}, the gating trees and channel names must be
#' consistent. These functions help removing the discrepancies and standardize the GatingSets so that they are mergable.
#' 
#' \link{gs_split_by_tree} splits the GatingSets into groups based on the gating tree structures.
#' 
#' \link{gs_split_by_channels} split GatingSets into groups based on their flow channels.
#' 
#' \link{gs_check_redundant_nodes} returns the terminal(or leaf) nodes that makes the gating trees to be different among GatingSets and thus can be considered to remove as redundant nodes.
#' 
#' \link{gs_remove_redundant_nodes} removes the terminal(or leaf) nodes that are detected as redundant by \code{gs_check_redundant_nodes}.
#' 
#' \link{gs_remove_redundant_channels} remove the redundant channels that are not used by any gate defined in the GatingSet.
#' 
#' \link{gs_update_channels} modifies the channel names in place. (Usually used to standardize the channels among GatingSets due to the letter case discrepancies or typo).
#' 
#' \link{gh_pop_move} inserts a dummy gate to the GatingSet. Is is useful trick to deal with the extra non-leaf node in some GatingSets that can not be simply removed by \code{gs_remove_redundant_nodes}
#' 
#' \link{gs_pop_set_visibility} hide a node/gate in a GatingSet. It is useful to deal with the non-leaf node that causes the tree structure discrepancy.
#' 
#' @rdname standardize-GatingSet
#' @aliases merge-GatingSet
#' 
NULL

#' @templateVar old groupByTree
#' @templateVar new gs_split_by_tree
#' @template template-depr_pkg
NULL

#' @export 
groupByTree <- function(x){
  .Deprecated("gs_split_by_tree")
  gs_split_by_tree(x)
}

#' split GatingSets into groups based on their gating schemes
#' Be careful that the splitted resluts still points to the original data set!!
#' 
#' It allows isomorphism in Gating tree and ignore difference in hidden nodes
#' i.e. tree is considered to be the same
#' as long as gs_get_pop_paths(gh, path = "auto", showHidden = F) returns the same set
#' 
#' @name gs_split_by_tree
#' @aliases groupByTree
#' @param x a list of GatingSets or one GatingSet
#' @return 
#' when x is a GatingSet, this function returns a list of sub-GatingSets
#' When x is a list of GatingSets, it returns a list of list, each list itself is a list of GatingSets, which share the same gating tree. 
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- gs_split_by_tree(gslist)
#' }
#' @export 
gs_split_by_tree <- function(x){
  message("Grouping by Gating tree...")
  node_seq <-unlist(lapply(x,function(this_gs){
            this_gh <- this_gs[[1]]
            this_nodes <- gs_get_pop_paths(this_gh, path = "auto")
            #sort it alphabetically
            this_nodes <- sort(this_nodes)
            paste0(this_nodes, collapse = "|")
          }))
  unname(split(x,node_seq))#I am glad that split.default also works for GatingSet object out-of-box
}


#' @templateVar old groupByChannels
#' @templateVar new gs_split_by_channels
#' @template template-depr_pkg
NULL

#' @export
groupByChannels <- function(x){
  .Deprecated("gs_split_by_channels")
  gs_split_by_channels(x)
}

#' split GatingSets into groups based on their flow channels
#' 
#' Sometime it is gates are defined on the different dimensions
#' across different GatingSets, (e.g. `FSC-W` or `SSC-H` may be used for Y axis for cytokines)
#' These difference in dimensions may not be critical since they are usually just used for visualization(istead of thresholding events)
#' But this prevents the gs from merging because they may not be collected across batces
#' Thus we have to separate them if we want to visualize the gates.
#' 
#' @name gs_split_by_channels
#' @aliases groupByChannels
#' @param x a list of GatingSets
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- gs_split_by_channels(gslist)
#' }
#' @export
gs_split_by_channels <- function(x){
  message("Grouping by channels...")
  key <- unlist(lapply(x,function(this_gs){
            this_gh <- this_gs[[1]]
            cols <- flowCore::colnames(gs_pop_get_data(this_gs))
            #reorder it alphabetically
            cols <- sort(cols)
            paste0(cols, collapse = "|")
          }))
  split(x,key)
}

#' visualize the tree structure differnece among the GatingSets
#' 
#' @param x \code{list} of groups(each group is a list of 'GatingSet`). it is usually the outcome from \link{gs_split_by_tree}.
#' @param path passed to \code{getNodes}
#' @param ... passed to \code{getNodes}
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- gs_split_by_tree(gslist)
#' gs_plot_diff_tree(gs_groups)
#' }
#' @export 
gs_plot_diff_tree <- function(x, path = "auto", ...){
  nodeSet <- lapply(x,function(thisObj){
    
    if(class(thisObj) == "list")
    {
      gh <- thisObj[[1]][[1]]
    }else if(class(thisObj) == "GatingSet")
    {
      gh <- thisObj[[1]]
    }else if(class(thisObj) == "GatingHierarchy")
    {
      gh <- thisObj
    }else{
      stop("invalid x!")
    }
    
    gs_get_pop_paths(gh, path = path, ...)
  })
  
  message("Comparing the tree structures")
  commonNodes <- Reduce(intersect, nodeSet)
  
  nGrps <- length(x)
  # par(mfrow = c(1,nGrps))
  
  for(grpid in seq_len(nGrps))
  {
    message("processing group: ", grpid)
    thisNodeSet <- nodeSet[[grpid]]
    thisObj <- x[[grpid]]
    if(class(thisObj) == "list")
    {
      gh <- thisObj[[1]][[1]]
    }else if(class(thisObj) == "GatingSet")
    {
      gh <- thisObj[[1]]
    }else if(class(thisObj) == "GatingHierarchy")
    {
      gh <- thisObj
    }else{
      stop("invalid x!")
    }
    
    message("Searching for uncommon nodes ..")
    nodes.uncommon <- setdiff(thisNodeSet,commonNodes)
    
    g <- .getGraph(gh)
    
    nodeDataDefaults(g, "common") <- FALSE
   
    message("hide/tag the common nodes ...")
    node.path2 <- gs_get_pop_paths(gh, showHidden = TRUE, path = 3)
    for(node in commonNodes)
    {
      nodeID <- .getNodeInd(gh, node) - 1
      
      node.label <- paste0("N_", nodeID)
      nodeData(g, node.label, attr = "common") <- TRUE
      children <- gs_pop_get_children(gh, node, path = path)
      #keep the direct parent of uncommon node
      if(!any(children%in%nodes.uncommon)||node=="root")
        nodeData(g, node.label, attr = "hidden") <- "1"
      else
      {
        nodeData(g, node.label, attr = "label") <- node.path2[nodeID+1]
      }
    }
    
    
    #filter out hidden node
    nodes.hidden <- nodeData(g,attr="hidden")
    for(i in 1:length(nodes.hidden))
    {
      if(as.logical(as.integer(nodes.hidden[[i]]))){
        
        nodeID <- names(nodes.hidden[i])
        
        g <- removeNode(nodeID, g)
      }
      
    }
    message("Rendering the substree ...")
    graphAttr <- list(rankdir="LR",page=c(8.5,11))
    nAttrs <- list()
    nodes <- nodes(g)
    if(length(nodes) == 0)
    {
      g <- addNode("Root", g)
      g <- addNode("Common nodes", g)
      g <- addEdge("Root", "Common nodes", g)
      nodes <- nodes(g)
      nLabel <- nodes
      names(nLabel) <- nodes
      plot(g
           , nodeAttrs = list(label = nLabel)
           ,attrs = list(graph = graphAttr
                         ,node = list(fixedsize = FALSE
                                      , fillcolor = "gray"
                                      , shape = "rectangle"
                                      )
                         # ,edge = list(col = "transparent")
                         )
           , main = paste0("Group ", grpid))
      # plot.new()
      next
    }
    
    common.flags <- nodeData(g,attr="common")
    nAttrs$label <- unlist(nodeData(g,attr="label"))
    nAttrs$fillcolor <- sapply(common.flags
                             ,function(iscommon)
                             {
                               ifelse(iscommon,"gray","red")
                             })
    nAttrs$lty <- sapply(common.flags
                         ,function(iscommon)
                         {
                           ifelse(!iscommon,"dotted","solid")
                         })
    
    #pass plot parameters to node attributes (some of parameters won't work via passing to layoutGraph directly)
    nAttrs[["fixedsize"]] <- sapply(common.flags, function(i)FALSE)
    nAttrs[["shape"]] <- sapply(common.flags
                                ,function(iscommon)
                                {
                                  ifelse(iscommon,"rectangle","ellipse")
                                })
    # params <- list(...)
    # for(pname in names(params))
    #   nAttrs[[pname]] <- sapply(nodes, function(i)params[[pname]])
    nodeRenderInfo(g) <- nAttrs
   
    plot(g,nodeAttrs = nAttrs,attrs=list(graph=graphAttr)
         , main = paste0("Group ", grpid)
    )
    
  }

}

#' @templateVar old checkRedundantNodes
#' @templateVar new gs_check_redundant_nodes
#' @template template-depr_pkg
NULL

#' @export
checkRedundantNodes <- function(...){
  .Deprecated("gs_check_redundant_nodes")
  gs_check_redundant_nodes(...)
}

#' try to determine the redundant terminal(or leaf) nodes that can be removed
#' 
#' These leaf nodes make the gating trees to be different from one another and can be removed by the subsequent convevient call 
#' \link{gs_remove_redundant_nodes}.
#' 
#' @name gs_check_redundant_nodes
#' @aliases checkRedundantNodes
#' @param x \code{GatingSet} or \code{list} of groups(each group is a list of 'GatingSet`). When it is a list, it is usually the outcome from \link{gs_split_by_tree}.
#' @param path argumented passed to \link{gs_get_pop_paths}. The default value is "auto".
#' @param ... other arguments passed to \link{gs_get_pop_paths}.
#' @return a list of the character vectors inicating the nodes that are considered to be redundant for each group of GatingSets.
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- gs_split_by_tree(gslist)
#' toRm <- gs_check_redundant_nodes(gs_groups)
#' }
#' @export
gs_check_redundant_nodes <- function(x, path = "auto", ...){
  
  nodeSet <- lapply(x,function(thisObj){
        
              if(class(thisObj) == "list")
              {
                gh <- thisObj[[1]][[1]]
              }else if(class(thisObj) == "GatingSet")
              {
                gh <- thisObj[[1]]
              }else if(class(thisObj) == "GatingHierarchy")
              {
                gh <- thisObj
              }else{
                stop("invalid x!")
              }
              
              gs_get_pop_paths(gh, path = path, ...)
            })
  commonNodes <- Reduce(intersect, nodeSet)
  toRemove <- mapply(nodeSet,x,FUN=function(thisNodeSet,thisObj){
                  if(class(thisObj) == "list")
                  {
                    gh <- thisObj[[1]][[1]]
                  }else if(class(thisObj) == "GatingSet")
                  {
                    gh <- thisObj[[1]]
                  }else if(class(thisObj) == "GatingHierarchy")
                  {
                    gh <- thisObj
                  }else{
                    stop("invalid x!")
                  }
                  nodesToRm <- setdiff(thisNodeSet,commonNodes)
                  #check if those nodes are terminal
                  isTerminal <- sapply(nodesToRm,function(thisNode){
                            length(gs_pop_get_children(gh,thisNode))==0
                          })
#                  browser()
                  if(!all(isTerminal)){
                    stop("Can't drop the non-terminal nodes: ",paste(nodesToRm[!isTerminal], collapse = " "))
                  }    
                  nodesToRm
                })
    toRemove       
}

#' @templateVar old dropRedundantNodes
#' @templateVar new gs_remove_redundant_nodes
#' @template template-depr_pkg
NULL

#' @export 
dropRedundantNodes <- function(x,toRemove){
  .Deprecated("gs_remove_redundant_nodes")
  gs_remove_redundant_nodes(x,toRemove)
}

#' Remove the terminal leaf nodes that make the gating trees to be different from one another.
#' 
#' It is usually called after \link{gs_split_by_tree} and \link{gs_check_redundant_nodes}. The operation is done in place through external pointers which means
#' all the orginal GatingSets are modified.
#' @name gs_remove_redundant_nodes
#' @aliases dropRedundantNodes
#' @param x \code{GatingSet} or \code{list} of groups(each group is a list of 'GatingSet`). When it is a list, it is usually the outcome from \link{gs_split_by_tree}.
#' @param toRemove \code{list} of the node sets to be removed. its length must equals to the length of 'x'. When \code{x} is a list, \code{toRemove} is usually the outcome from \link{gs_check_redundant_nodes}.  
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- gs_split_by_tree(gslist)
#' toRm <- gs_check_redundant_nodes(gs_groups)
#' gs_remove_redundant_nodes(gs_groups, toRm)
#' 
#' #Now they can be merged into a single GatingSetList.
#' #Note that the original gs objects are all modified in place.
#' GatingSetList(gslist)
#' }
#' @export 
gs_remove_redundant_nodes <- function(x,toRemove){
  mapply(toRemove,x,FUN=function(thisNodeSet,thisObj){
        
        if(length(thisNodeSet)>0){
          for(thisNode in thisNodeSet){
#                browser()
                message("Removing ", thisNode)
                if(is(thisObj, "GatingSet"))
                    gs_pop_remove(thisNode,gs = thisObj)
                else if(class(thisObj) == "list")
                  for(this_gs in thisObj)
                  {
                        gs_pop_remove(thisNode,gs = this_gs)
                  }
          }
        }
      })
  
}
#' @templateVar old dropRedundantChannels
#' @templateVar new gs_drop_redundant_channels
#' @template template-depr_pkg
NULL

#' @export 
dropRedundantChannels <- function( ...){
  .Deprecated("gs_remove_redundant_channels")
  gs_remove_redundant_channels(...)
}

#' Remove the channels from flow data that are not used by gates
#' 
#' Removing these redundant channels can help standardize the channels across different GatingSet objects and make them mergable.
#' @name gs_remove_redundant_channels
#' @aliases dropRedundantChannels
#' @param gs a \code{GatingSet} 
#' @param ... other arugments passed to gs_get_pop_paths method
#' @return a new \code{GatingSet} object that has redundant channels removed. Please note that this new object shares the same reference (or external pointers) with the original GatingSets.
#' @examples 
#' \dontrun{
#' gs_new <- gs_remove_redundant_channels(gs)
#' }
#' @export 
gs_remove_redundant_channels <- function(gs, ...){
  nodes <- gs_get_pop_paths(gs, ...)[-1]
  gh <- gs[[1]]
  params <- unlist(lapply(nodes, function(node){
        g <- gh_pop_get_gate(gh, node)
        if(class(g) != "booleanFilter"){
         as.vector(parameters(g))  
        }
        
      }))
  params <- unique(params)
  fs <- gs_cyto_data(gs)
  cols <- flowCore::colnames(fs)
  toDrop <- setdiff(cols, params)
  if(length(toDrop) >0){
    message("drop ", paste0(toDrop, collapse = ", "))
    ind <- match(toDrop, cols)
    cols <- cols[-ind]
    gs_cyto_data(gs) <- fs[, cols]
  }
  gs  
  
}
#' @templateVar old updateChannels
#' @templateVar new gs_update_channels
#' @template template-depr_pkg
NULL

#' @export 
updateChannels <- function(gs, map, all = TRUE){
  .Deprecated("gs_update_channels")
  gs_update_channels(gs, map, all)
}

#' Update the channel information of a GatingSet (c++ part)
#' 
#' It updates the channels stored in gates,compensations and transformations
#' based on given mapping between the old and new channel names.
#' 
#' @name gs_update_channels
#' @aliases updateChannels
#' @param gs a GatingSet object
#' @param map \code{data.frame} contains the mapping from old (case insensitive) to new channel names
#'                             Note: Make sure to remove the '<' or '>' characters from 'old` name because the API tries 
#'                                   to only look at the raw channel name so that the gates with both prefixed and non-prefixed names could be updated. 
#' @param all \code{logical} whether to update the flow data as well
#'                                    
#' @return when 'all' is set to TRUE, it returns a new GatingSet but it still shares the same underling c++ tree structure with the original GatingSet                                  
#'  otherwise it returns nothing (less overhead.)
#' @examples 
#' \dontrun{
#'   ##this will update both "Qdot 655-A" and "<Qdot 655-A>"
#'  gs <- gs_update_channels(gs, map = data.frame(old = c("Qdot 655-A")
#'                                          , new = c("QDot 655-A")
#'                                          )
#'                      )  
#'}
#' @export 
gs_update_channels <- function(gs, map, all = TRUE){
	stopifnot(all)
	
  map <- .preprocessMap(gs, map)
  
  #update gates and comps ,trans(c++ part)
  updateChannels_cpp(gs@pointer, map)
  
  
  if(!is.null(gs@transformation)){
    gs@transformation <- sapply(gs@transformation, function(trans){
      cols <- names(trans)
      new <- .matchCols(cols, map)
      names(trans) <- new 
      trans
    }, simplify = FALSE)
    
  }
  gs

}

.matchCols <- function(cols, map){
  sapply(cols, function(col){
        colInd <- match(col, map[["old"]])
        ifelse(is.na(colInd), col, map[["new"]][colInd])
      }, USE.NAMES = F)
}


#' validity check and add prefixed entries when applicable
#' @importFrom dplyr %>% group_by do
#' @noRd 
.preprocessMap <- function(gs, map){
  if(!is.data.frame(map))
    stop("'map' must be a data.frame!")
  if(!setequal(c("old", "new"), colnames(map)))
    stop("'map' must contain (only) 'old' and 'new' columns!")
  
  map[["old"]] <- as.character(map[["old"]]) 
  map[["new"]] <- as.character(map[["new"]]) 
  
  
  #add prefixed entries
  # we do it unconditionally to all entries
  #since user may update C++ part and flow data separately
  #and we have to make sure updated comp.chnls
  #doesn't interfere the second run of gs_update_channels
  
  sn <- cpp_getSamples( gs@pointer)[1] #can't use sampleNames(gs) since flow data may not be bound to gs yet when it is used within flowjo_to_gatingset
  comp <- cpp_getCompensation( gs@pointer, sn)
#   comp.chnl <- comp$parameters
  prefix <- comp$prefix
  suffix <- comp$suffix
  map %>% 
      group_by(old) %>%
      do({
            
#       if(.[["old"]]%in%comp.chnl){
            
            orig <- .
            paste0(prefix, unlist(orig), suffix) %>% rbind(orig)
#       }else
#         .
          })
  
}

