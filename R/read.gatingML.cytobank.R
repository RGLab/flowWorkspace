#' A graph object returned by 'read.gatingML.cytobank' function.
#' 
#' Each node corresponds to a population(or GateSet) defined in gatingML file. The actual gate object (both global and tailored gates) is 
#' associated with each node as nodeData. Compensation and transformations are stored in graphData slot.
#'
#' The class simply extends the graphNEL class and exists for the purpose of method dispatching.
#' 
#' @importClassesFrom graph graphNEL graphBase graph
#' @importClassesFrom Biobase AssayData
#' @export
setClass("graphGML", contains = "graphNEL")

#' Parser for gatingML exported by Cytobank
#' 
#' The Default parser (flowUtils::read.gatingML) does not  parse the population tree as well as 
#' the custom information from cytobank. (e.g. gate name, fcs filename).
#' 
#' @param file Gating-ML XML file
#' @param ... additional arguments passed to the handlers of 'xmlTreeParse'
#' @export
#' @importFrom flowUtils read.gatingML
#' @return a graphGML that represents the population tree. 
#' The gate and population name are stored in nodeData of each node. 
#' Compensation and transformations are stored in graphData.
#' @examples 
#' \dontrun{
#' xml <- system.file("extdata/cytotrol_tcell_cytobank.xml", package = "flowWorkspace")
#' g <- read.gatingML.cytobank(xml) #parse the population tree
#' plot(g) #visualize it
#' }
read.gatingML.cytobank <- function(file, ...){
  
  #parse all the elements:gate, GateSets, comp, trans
  flowEnv <- new.env()
  read.gatingML(file, flowEnv) 
  
  #parse gate info (id vs fcs and pop name)
  gateInfo <- parse.gateInfo(file)
  
  #restore the original gate parameter names 
  #because flowUtils add comp and tran names as prefix which is really not neccessary (even problematic) here.
  for(objID in ls(flowEnv)){
    obj <- flowEnv[[objID]]
    if(is(obj, "parameterFilter")){
      
      # message(class(obj))
      
      sb <- gateInfo[id == objID, ]
      if(nrow(sb)>0){
        orig_params <- sb[, params]
        orig_params <- strsplit(split = ":", orig_params)[[1]]
        params <- parameters(obj)
        ind <- sapply(orig_params, function(orig_param)grep(paste0(orig_param, "$"), params))
        parameters(obj) <- orig_params[ind]   
        flowEnv[[objID]] <-  obj  
      }
      
    }
    
  }
  
  
  #construct tree from GateSets
  g <- constructTree(flowEnv, gateInfo)
  
  #determine comps and trans to be used
  
  comp_refs <- gateInfo[, comp_ref]
  comp_refs <- unique(comp_refs[comp_refs!=""])
  if(length(comp_refs) > 1)
    stop("More than one compensation referred in gates!")
  else{
    if(comp_refs == "FCS")
      comps <- "FCS"
    else
      comps <- flowEnv[[comp_refs]]
  }
  g@graphData[["compensation"]] <- comps

  
  trans <-  sapply(ls(flowEnv), function(i){
                        
                                obj <- flowEnv[[i]]
                                if(is(obj, "transformation"))
                                  obj
                              }, USE.NAMES = FALSE)
  trans <- compact(trans)
  chnl <- sapply(trans, function(tran)unname(parameters(tran@parameters)), USE.NAMES = F)
  #convert from transform object to function since transform has empty function in .Data slot
  #which is not suitable for transformList constructor
  trans <- sapply(trans, eval, USE.NAMES = F)
  ind <- chnl != "any"
  
  g@graphData[["transformations"]] <- transformList(chnl[ind], trans[ind]) 
  
  as(g, "graphGML")
  
}

#' Parse the cytobank custom_info for each gate
#' 
#' Fcs filename and gate name stored in 'custom_info' element are beyong the scope of
#' the gatingML standard and thus not covered by the default 'read.gatingML'. 
#' 
#' @param file xml file path
#' @param ... additional arguments passed to the handlers of 'xmlTreeParse'
#' @return a data.frame that contains three columns: id (gateId), name (gate name), fcs (fcs_file_filename).
#' @importFrom XML xmlRoot xmlName xmlGetAttr xmlValue xmlElementsByTagName xmlChildren
parse.gateInfo <- function(file, ...)
{       
  
  root <- xmlRoot(flowUtils:::smartTreeParse(file,...))
  
  rbindlist(
    lapply(xmlChildren(root), function(node){
            nodeName <- xmlName(node)
            
            if(grepl("*Gate", nodeName)){
              
              
                id <- xmlGetAttr(node, "id")
                
                name <- getCustomNodeInfo(node, "name")
                fcs_file_filename <- getCustomNodeInfo(node, "fcs_file_filename")
                gate_id <- getCustomNodeInfo(node, "gate_id")#used to find tailored gate
                if(nodeName %in% c("BooleanGate"))
                {          
                  comp_ref <- trans_ref <- params <- ""
                }else
                {
                  if(nodeName == "QuadrantGate"){
                    dimTag <- "divider"
                    #update id with quardant definition
                    quadrantList <- xmlElementsByTagName(node, "Quadrant")
                    id <- unname(sapply(quadrantList, function(i)xmlGetAttr(i, "id")))
                    
                  }else
                    dimTag <- "dimension"
                
                  dimNode <- xmlElementsByTagName(node, dimTag)
                  comp_ref <- unique(sapply(dimNode, function(i)xmlGetAttr(i, "compensation-ref")))
                  trans_ref <- unique(unname(unlist(compact(sapply(dimNode, function(i)xmlGetAttr(i, "transformation-ref"))))))
                  trans_ref <- ifelse(is.null(trans_ref), "", trans_ref)
                  params <- paste(sapply(dimNode, function(i)xmlGetAttr(i[["fcs-dimension"]], "name")), collapse = ":")
                }
                # message(name)
                # browser()
                data.table(id = id, name = name, gate_id = gate_id, fcs = fcs_file_filename
                  , comp_ref = comp_ref, trans_ref = trans_ref, params = params
                  )
                
              
            }
    
  })
  )
}

getCustomNodeInfo <- function(node, nodeName){
  custom_node <- xmlElementsByTagName(node, nodeName, recursive = TRUE)
  if(length(custom_node) >0 ){
    value <- xmlValue(custom_node[[1]])
    if(length(value)==0)
      value <- ""
  }
  else 
    value <- ""
  
  value
}

#' Given the leaf node, try to find out if a collection of nodes can be matched to a path in a graph(tree) by the bottom-up searching
#' @param g graphNEL
#' @param leaf the name of leaf(terminal) node 
#' @param nodeSet a set of node names
#' @return TRUE if path is found, FALSE if not path is matched.
#' @importFrom graph inEdges
matchPath <- function(g, leaf, nodeSet){
  
    if(leaf %in% nodeSet){
      nodeSet <- nodeSet[-match(leaf, nodeSet)] #pop it out 
      leaf <- inEdges(leaf, g)[[1]]#update it with parent node
      if(length(nodeSet) == 0){ #nodeSet emptied
        if(length(leaf) == 0)  #and path reach to the top
          return (TRUE)
        else
          return(FALSE) #path has not reached to the top
      }else
      {
        if(length(leaf) == 0) #reach the top before nodeSet is emptied
          return(FALSE)
        else
          matchPath(g, leaf, nodeSet)  #continue match the path against nodeSet  
      }
    }else
      return(FALSE)

}

#' Reconstruct the population tree from the GateSets
#' @param flowEnv the enivornment contains the elements parsed by read.gatingML function
#' @param gateInfo the data.frame contains the gate name, fcs filename parsed by parse.gateInfo function
#' @return a graphNEL represent the population tree. The gate and population name are stored as nodeData in each node.
#' @importFrom graph graphNEL nodeDataDefaults<- nodeData<- addEdge edges removeNode
constructTree <- function(flowEnv, gateInfo){
  
  #parse the references from boolean gates
  #currently we use refs (which point to the gate id defined by the standard)
  #we don't want to use gate_id from custom_info used by cytobank internally
  #because quadrantGate seems do not have this id for each of its quadrant elements
  gateSets <- sapply(ls(flowEnv), function(i){
    obj <- flowEnv[[i]]
    if(class(obj) == "intersectFilter"){
      refs <- obj@filters
      refs <- sapply(refs, slot, "name")
      unique(refs) #make root node depth of 1
    }
  })
  
  gateSets <- compact(gateSets)
  popIds <- names(gateSets)
  #sort by the depths of path
  counts <- sapply(gateSets, function(i)length(i))
  counts <- sort(counts)
  
  #init the graph with all gates that have been referred
  # gateIds <- unique(unlist(gateSets))
  g <- graphNEL(nodes = popIds, edgemode = "directed")
  
  nodeDataDefaults(g, "popName") <- ""
  nodeDataDefaults(g, "gateInfo") <- list()
  
  # add edges (from nDepths 2 to n)
  for(popId in names(counts)){
    thisGateSet <- gateSets[[popId]]
    nDepth <- length(thisGateSet)
    
    popName <- gateInfo[id == popId, name]
    #add pop name
    nodeData(g, popId, "popName") <- popName
    
    if(nDepth == 1){#root node
      gateID <- thisGateSet[1]

    }else{
      #traverse all the potential parent GateSets(i.e. subset)
      parents_popIds <- names(counts[counts == nDepth - 1])
      for(parentID in parents_popIds)
      {
        pGateSet <- gateSets[[parentID]]
        #check the overlaps between two sets
        if(setequal(intersect(pGateSet, thisGateSet), pGateSet))
        { #if it is indeed a subset of the current gateSet
          gateID <- setdiff(thisGateSet, pGateSet) #parse out the gateID
          #add edge
          g <- addEdge(parentID, popId, g)
          # There might be multiple parents due to the duplicated GateSets allowed in gatingML
          # it is not clear which one should be picked and
          # we pick the first one for now
          break 
        }
      }
    }
    
    #add gate
    sb <- gateInfo[id == gateID, ]
    #try to find the tailored gate
    tg_sb <- gateInfo[gate_id == sb[, gate_id] & fcs != "", ]
    
    
    tg <- sapply(tg_sb[, id], function(gateID){
                flowEnv[[gateID]]
              }, simplify = FALSE)
    names(tg) <- tg_sb[, fcs]
#     message(popName)
    gate <- flowEnv[[gateID]]
    
    nodeData(g, popId, "gateInfo") <- list(list(gate = gate
                                                , gateName = sb[, name]
                                                , fcs = sb[, fcs]
                                                , tailored_gate = tg
                                              )
                                          )
    
  }
  
  #prune the tree by removing the orphan nodes
#   egs <- edges(g)
#   leaves <- names(egs[sapply(egs, length) == 0])
#   for(node in leaves){
#     if(length(inEdges(node, g)[[1]]) == 0)
#       g <- removeNode(node, g)
#   }
  g
}

