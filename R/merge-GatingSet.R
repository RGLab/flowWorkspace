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
.groupByTree <- function(x){
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

#' split GatingSets into groups based on their flow channels
#' 
#' Sometime it is gates are defined on the different dimensions
#' across different GatingSets, (e.g. `FSC-W` or `SSC-H` may be used for Y axis for cytokines)
#' These difference in dimensions may not be critical since they are usually just used for visualization(istead of thresholding events)
#' But this prevents the gs from merging because they may not be collected across batces
#' Thus we have to separate them if we want to visualize the gates.
#' 
#' @param x a list of GatingSets
.groupByChannels <- function(x){
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
#' try to determine the redundant terminal nodes that can be removed
#' in order to make trees mergable
#' @param x \code{GatingSet} or \code{list} of groups(each group is a list of 'GatingSet`) 
.checkRedundantNodes <- function(x, path = "auto", ...){
  
  nodeSet <- lapply(x,function(thisObj){
        
              if(class(thisObj) == "list")
              {
                gh <- thisObj[[1]][[1]]
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

#' drop the terminal nodes
#' @param x \code{GatingSet} or \code{list} of groups(each group is a list of 'GatingSet`)
#' @param toRemove \code{list} of the node sets to be removed. its length must equals to the length of 'x' 
.dropRedundantNodes <- function(x,toRemove){
  mapply(toRemove,x,FUN=function(thisNodeSet,thisObj){
        
        if(length(thisNodeSet)>0){
          for(thisNode in thisNodeSet){
#                browser()
                message("Removing ", thisNode)
                if(class(thisObj) == "GatingHierarchy")
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
#' remove the channels from flow data that are not used by gates
#' 
#' @param ... other arugments passed to getNodes method
.dropRedundantChannels <- function(gs, ...){
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
