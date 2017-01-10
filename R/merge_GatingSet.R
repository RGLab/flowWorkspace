#' @name standardize-GatingSet
#' 
#' @title 
#' The tools to standardize the tree structures and channel names.
#'
#' @description 
#' groupByTree(x)
#' 
#' groupByChannels(x)
#' 
#' checkRedundantNodes(x)
#' 
#' dropRedundantNodes(x, toRemove)
#' 
#' dropRedundantChannels(gs)
#' 
#' updateChannels(gs, map, all = TRUE)
#' 
#' insertGate(gs, gate, parent, children)
#' 
#' setNode(x, y, FALSE)
#' 
#' @details 
#' In order to merge multiple GatingSets into single \link{GatingSetList}, the gating trees and channel names must be
#' consistent. These functions help removing the discrepancies and standardize the GatingSets so that they are mergable.
#' 
#' \link{groupByTree} splits the GatingSets into groups based on the gating tree structures.
#' 
#' \link{groupByChannels} split GatingSets into groups based on their flow channels.
#' 
#' \link{checkRedundantNodes} returns the terminal(or leaf) nodes that makes the gating trees to be different among GatingSets and thus can be considered to remove as redundant nodes.
#' 
#' \link{dropRedundantNodes} removes the terminal(or leaf) nodes that are detected as redundant by \code{checkRedundantNodes}.
#' 
#' \link{dropRedundantChannels} remove the redundant channels that are not used by any gate defined in the GatingSet.
#' 
#' \link{updateChannels} modifies the channel names in place. (Usually used to standardize the channels among GatingSets due to the letter case discrepancies or typo).
#' 
#' \link{insertGate} inserts a dummy gate to the GatingSet. Is is useful trick to deal with the extra non-leaf node in some GatingSets that can not be simply removed by \code{dropRedundantNodes}
#' 
#' \link{setNode} hide a node/gate in a GatingSet. It is useful to deal with the non-leaf node that causes the tree structure discrepancy.
#' 
#' @rdname standardize-GatingSet
#' @aliases merge-GatingSet
#' 
NULL

#' split GatingSets into groups based on their gating schemes
#' Be careful that the splitted resluts still points to the original data set!!
#' 
#' It allows isomorphism in Gating tree and ignore difference in hidden nodes
#' i.e. tree is considered to be the same
#' as long as getNodes(gh, path = "auto", showHidden = F) returns the same set
#' 
#' @param x a list of GatingSets or one GatingSet
#' @return 
#' when x is a GatingSet, this function returns a list of sub-GatingSets
#' When x is a list of GatingSets, it returns a list of list, each list itself is a list of GatingSets, which share the same gating tree. 
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- groupByTree(gslist)
#' }
#' @export 
groupByTree <- function(x){
  message("Grouping by Gating tree...")
  node_seq <-unlist(lapply(x,function(this_gs){
            this_gh <- this_gs[[1]]
            this_nodes <- getNodes(this_gh, path = "auto")
            #sort it alphabetically
            this_nodes <- sort(this_nodes)
            paste0(this_nodes, collapse = "|")
          }))
  unname(split(x,node_seq))#I am glad that split.default also works for GatingSet object out-of-box
}
.groupByTree <- groupByTree

#' split GatingSets into groups based on their flow channels
#' 
#' Sometime it is gates are defined on the different dimensions
#' across different GatingSets, (e.g. `FSC-W` or `SSC-H` may be used for Y axis for cytokines)
#' These difference in dimensions may not be critical since they are usually just used for visualization(istead of thresholding events)
#' But this prevents the gs from merging because they may not be collected across batces
#' Thus we have to separate them if we want to visualize the gates.
#' 
#' @param x a list of GatingSets
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- groupByChannels(gslist)
#' }
#' @export 
groupByChannels <- function(x){
  message("Grouping by channels...")
  key <- unlist(lapply(x,function(this_gs){
            this_gh <- this_gs[[1]]
            cols <- flowCore::colnames(getData(this_gs))
            #reorder it alphabetically
            cols <- sort(cols)
            paste0(cols, collapse = "|")
          }))
  split(x,key)
}
.groupByChannels <- groupByChannels

#' try to determine the redundant terminal(or leaf) nodes that can be removed
#' 
#' THese leaf nodes make the gating trees to be different from one another and can be removed by the subsequent convevient call 
#' \link{dropRedundantNodes}.
#' 
#' @param x \code{GatingSet} or \code{list} of groups(each group is a list of 'GatingSet`). When it is a list, it is usually the outcome from \link{groupByTree}.
#' @param path argumented passed to \link{getNodes}. The default value is "auto".
#' @param ... other arguments passed to \link{getNodes}.
#' @return a list of the character vectors inicating the nodes that are considered to be redundant for each group of GatingSets.
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- groupByTree(gslist)
#' toRm <- checkRedundantNodes(gs_groups)
#' }
#' @export 
checkRedundantNodes <- function(x, path = "auto", ...){
  
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
              
              getNodes(gh, path = path, ...)
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
                            length(getChildren(gh,thisNode))==0
                          })
#                  browser()
                  if(!all(isTerminal)){
                    stop("Can't drop the non-terminal nodes: ",paste(nodesToRm[!isTerminal], collapse = " "))
                  }    
                  nodesToRm
                })
    toRemove       
}
.checkRedundantNodes <- checkRedundantNodes
#' Remove the terminal leaf nodes that make the gating trees to be different from one another.
#' 
#' It is usually called after \link{groupByTree} and \link{checkRedundantNodes}. The operation is done in place through external pointers which means
#' all the orginal GatingSets are modified.
#' 
#' @param x \code{GatingSet} or \code{list} of groups(each group is a list of 'GatingSet`). When it is a list, it is usually the outcome from \link{groupByTree}.
#' @param toRemove \code{list} of the node sets to be removed. its length must equals to the length of 'x'. When \code{x} is a list, \code{toRemove} is usually the outcome from \link{checkRedundantNodes}.  
#' @examples 
#' \dontrun{
#' gslist <- list(gs1, gs2, gs3, gs4, gs5)
#' gs_groups <- groupByTree(gslist)
#' toRm <- checkRedundantNodes(gs_groups)
#' dropRedundantNodes(gs_groups, toRm)
#' 
#' #Now they can be merged into a single GatingSetList.
#' #Note that the original gs objects are all modified in place.
#' GatingSetList(gslist)
#' }
#' @export 
dropRedundantNodes <- function(x,toRemove){
  mapply(toRemove,x,FUN=function(thisNodeSet,thisObj){
        
        if(length(thisNodeSet)>0){
          for(thisNode in thisNodeSet){
#                browser()
                message("Removing ", thisNode)
                if(is(thisObj, "GatingSet"))
                    Rm(thisNode,thisObj)
                else if(class(thisObj) == "list")
                  for(this_gs in thisObj)
                  {
                        Rm(thisNode,this_gs)
                  }
          }
        }
      })
  
}
.dropRedundantNodes <- dropRedundantNodes
#' Remove the channels from flow data that are not used by gates
#' 
#' Removing these redundant channels can help standardize the channels across different GatingSet objects and make them mergable.
#'
#' @param gs a \code{GatingSet} 
#' @param ... other arugments passed to getNodes method
#' @return a new \code{GatingSet} object that has redundant channels removed. Please note that this new object shares the same reference (or external pointers) with the original GatingSets.
#' @examples 
#' \dontrun{
#' gs_new <- dropRedundantChannels(gs)
#' }
#' @export 
dropRedundantChannels <- function(gs, ...){
  nodes <- getNodes(gs, ...)[-1]
  gh <- gs[[1]]
  params <- unlist(lapply(nodes, function(node){
        g <- getGate(gh, node)
        if(class(g) != "booleanFilter"){
         as.vector(parameters(g))  
        }
        
      }))
  params <- unique(params)
  fs <- flowData(gs)
  cols <- flowCore::colnames(fs)
  toDrop <- setdiff(cols, params)
  if(length(toDrop) >0){
    message("drop ", paste0(toDrop, collapse = ", "))
    ind <- match(toDrop, cols)
    cols <- cols[-ind]
    flowData(gs) <- fs[, cols]
  }
  gs  
  
}
.dropRedundantChannels <- dropRedundantChannels
#' Update the channel information of a GatingSet (c++ part)
#' 
#' It updates the channels stored in gates,compensations and transformations
#' based on given mapping between the old and new channel names.
#' 
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
#'  gs <- updateChannels(gs, map = data.frame(old = c("Qdot 655-A")
#'                                          , new = c("QDot 655-A")
#'                                          )
#'                      )  
#'}
#' @export 
#' @importFrom flowCore colnames<-
#' @importFrom ncdfFlow colnames<-
updateChannels <- function(gs, map, all = TRUE){
  
  map <- flowWorkspace:::.preprocessMap(gs, map)
  
  #update gates and comps ,trans(c++ part)
  .updateChannels(gs, map)
  
  #update the externally stored comps,trans (R part)
  if(!is.null(gs@compensation)){
    gs@compensation <- lapply(gs@compensation, function(comp){
          mat <- comp@spillover
          cols <- colnames(mat)
          new <- .matchCols(cols, map)
          
          colnames(mat) <- new
          compensation(mat)
        })
  }
  
  if(!is.null(gs@transformation)){
    cols <- names(gs@transformation)
    new <- .matchCols(cols, map)
    names(gs@transformation) <- new
  }
  
  #update flow data
  if(all){
    fs <- flowData(gs)
    cols <- colnames(fs)
    newCols <- .matchCols(cols, map)
    colnames(fs) <- newCols
    flowData(gs) <- fs
    gs
  }
  
}

.matchCols <- function(cols, map){
  sapply(cols, function(col){
        colInd <- match(col, map[["old"]])
        ifelse(is.na(colInd), col, map[["new"]][colInd])
      }, USE.NAMES = F)
}


#' validity check and add prefixed entries when applicable
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
  #doesn't interfere the second run of updateChannels
  
  sn <- .cpp_getSamples( gs@pointer)[1] #can't use sampleNames(gs) since flow data may not be bound to gs yet when it is used within parseWorkspace
  comp <- .cpp_getCompensation( gs@pointer, sn)
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

#' insert a dummy gate to the GatingSet
#' 
#' Is is useful trick to make the tree structure of GatingSet same with other so that
#' they can be combined into a 'GatingSetList' object.
#' 
#' @param gs \code{GatingSet} to work with
#' @param gate \code{filter} a dummy gate to be inserted, its 'filterId' will be used as the population name
#' @param parent \code{character} full path of parent node where the new dummy gate to be added to
#' @param children \code{character} full path of chidlren nodes that the new dummy gate to be parent of
#' @return  a new \code{GatingSet} object with the new gate added but share the same flow data with the input 'GatingSet'
#' @examples 
#' \dontrun{
#' #construct a dummy singlet gate 
#'  dummyGate <- rectangleGate("FSC-A" = c(-Inf, Inf), "FSC-H" = c(-Inf, Inf), filterId = "singlets")
#' #insert it between the 'not debris" node and "lymph" node
#'  gs_clone <- insertGate(gs, dummyGate, "not debris", "lymph") 
#' }
#' @export 
insertGate <- function(gs, gate, parent, children){
  dummyNode <- gate@filterId
  nodes <- getNodes(gs)
  dummyPath <- file.path(parent, dummyNode)
  if(any(grepl(dummyPath, nodes)))
    stop(dummyPath, " already exists!")
  
  #copy the entire tree structure
  message("cloning tree structure...")
  clone <- gs
  clone@pointer <- .cpp_CloneGatingSet(gs@pointer,sampleNames(gs))
  #remove the old children
  lapply(children, function(child)Rm(child, clone))
  
  # add the new node
  add(clone, gate, parent = parent)
  #copy children
  ancester <- getNodes(clone)
  nodesToadd <- nodes[!nodes%in%ancester]
  
  lapply(nodesToadd, function(node){
        
        if(node%in%children)
          thisParent <- dummyNode #add the old direct children to the new dummy node
        else{
          #copy the other nodes to its parent
          oldParent <- getParent(gs, node)
          #match to the new parent
          thisNodes <- getNodes(clone)
          thisParent <- thisNodes[match(oldParent, gsub(paste0("/", dummyNode), "", thisNodes))] 
        }
        popName <- basename(node)
        lapply(sampleNames(gs),function(sn){
              gh <- gs[[sn]]
              gate <- getGate(gh, node)
              negated <- flowWorkspace:::isNegated(gh, node)
              add(clone[[sn]], gate, name = popName, parent = thisParent, negated = negated)      
            })  
        
      })
  recompute(clone)
  clone
}