#' @include AllClasses.R
NULL

#' modify graph::fromGXL by using customized handler 
#' (deprecated by using Rgraphviz::agread)
.fromGXL <- function (con) 
{
  contents <- paste(readLines(con), collapse = "")
  xmlEventParse <- getExportedValue("XML", "xmlEventParse")
  xmlEventParse(contents, .graph_handler(), asText = TRUE, saxVersion = 2)$asGraphNEL()
}

#' modify graph:::graph_handler by concatenate the multiple attr string into one
#' to avoid partial node name display. Because XML::xmlEventParse somehow split
#' the node name into arrays when there is numeric character reference (&#nnnn;)
#' 
#' (deprecated by using Rgraphviz::agread)
#'  
#' @importFrom graph graphNEL addEdge
.graph_handler <- function()
{
  
  
  all_nodes_e <- new.env(parent = emptyenv(), hash = TRUE)
  node_data_e <- new.env(parent = emptyenv(), hash = TRUE)
  node_defaults_e <- new.env(parent = emptyenv(), hash = TRUE)
  edge_data_e <- new.env(parent = emptyenv(), hash = TRUE)
  edge_defaults_e <- new.env(parent = emptyenv(), hash = TRUE)
  from_e <- new.env(parent = emptyenv(), hash = TRUE)
  to_e <- new.env(parent = emptyenv(), hash = TRUE)
  nodeCount <- 0L
  edgeCount <- 0L
  graphID <- NULL
  curNode <- NULL
  curAttr <- NULL
  inNode <- FALSE
  inEdge <- FALSE
  inAttr <- FALSE
  inInt <- FALSE
  inFloat <- FALSE
  inBool <- FALSE
  edgemode <- NULL
  add_node <- function(theNode) {
    if (!exists(theNode, all_nodes_e)) {
      nodeCount <<- nodeCount + 1L
      all_nodes_e[[theNode]] <- nodeCount
    }
  }
  add_edge <- function(from, to) {
    edgeCount <<- edgeCount + 1L
    k <- as.character(edgeCount)
    from_e[[k]] <- from
    to_e[[k]] <- to
  }
  startElement <- function(x, atts, ...) {
    if (x == "graph") {
      if (!is.null(graphID)) 
        stop("multiple graphs not supported")
      graphID <<- atts["id"]
      eMode <- atts["edgemode"]
      if (!is.na(eMode)) {
        if (eMode %in% c("undirected", "defaultundirected")) 
          edgemode <<- "undirected"
        else edgemode <<- "directed"
      }
      else {
        edgemode <<- "directed"
      }
    }
    else if (x == "node") {
      inNode <<- TRUE
      theNode <- as.character(atts["id"])
      add_node(theNode)
      curNode <<- theNode
    }
    else if (x == "attr") {
      inAttr <<- TRUE
      curAttr <<- atts["name"]
    }
    else if (x == "edge") {
      inNode <<- FALSE
      inEdge <<- TRUE
      from <- as.character(atts["from"])
      to <- as.character(atts["to"])
      add_node(from)
      add_node(to)
      add_edge(from, to)
    }
    else if (x == "int") {
      inInt <<- TRUE
    }
    else if (x == "float") {
      inFloat <<- TRUE
    }
    else if (x == "bool") {
      inBool <<- TRUE
    }
  }
  text <- function(x, atts, ...) {
    if (inAttr && nchar(x) > 0) {
      if (inInt) 
        x <- as.integer(x)
      if (inFloat) 
        x <- as.double(x)
      if (inBool) {
        if (identical(x, "true")) 
          x <- TRUE
        else if (identical(x, "false")) 
          x <- FALSE
        else stop("bad bool value: ", x)
      }
      if (inNode) {
        node_defaults_e[[curAttr]] <- as.character(NA)
        nattrs <- node_data_e[[curNode]]
        if (!length(nattrs)) 
          nattrs <- list()
        #concatenate with old value before update it
        nattrs[[curAttr]] <- paste(nattrs[[curAttr]], x, sep = "")
        node_data_e[[curNode]] <- nattrs
      }
      else if (inEdge) {
        edge_defaults_e[[curAttr]] <- as.character(NA)
        k <- as.character(edgeCount)
        eattrs <- edge_data_e[[k]]
        if (!length(eattrs)) 
          eattrs <- list()
        eattrs[[curAttr]] <- x
        edge_data_e[[k]] <- eattrs
      }
    }
  }
  endElement <- function(x, ...) {
    if (x == "attr") 
      inAttr <<- FALSE
    else if (x == "node") 
      inNode <<- FALSE
    else if (x == "edge") 
      inEdge <<- FALSE
    else if (x == "int") 
      inInt <<- FALSE
    else if (x == "float") 
      inFloat <<- FALSE
    else if (x == "bool") 
      inBool <<- FALSE
  }
  asGraphNEL <- function() {
    ftmat <- cbind(from = unlist(as.list(from_e)), to = unlist(as.list(to_e)))
    nn <- unlist(as.list(all_nodes_e))
    nn <- names(nn)[order(nn)]
    g <- graphNEL(nodes = nn, edgemode = edgemode)
    if (length(node_defaults_e)) {
      nd <- new("attrData", as.list(node_defaults_e))
      nd@data <- as.list(node_data_e)
      g@nodeData <- nd
    }
    if (length(edge_data_e)) {
      ed <- new("attrData", as.list(edge_defaults_e))
      edvals <- as.list(edge_data_e)
      names(edvals) <- graph:::.makeEdgeKeys(ftmat[, 1], ftmat[, 
              2])
      ed@data <- edvals
      g@edgeData <- ed
    }
    g <- addEdge(ftmat[, 1], ftmat[, 2], g)
    validObject(g)
    g
  }
  list(startElement = startElement, endElement = endElement, 
      text = text, asGraphNEL = asGraphNEL)
}
#' @importClassesFrom graph graphNEL 
#' @importMethodsFrom Rgraphviz AgNode AgEdge nodeDataDefaults name nodeData
#' @importMethodsFrom graph nodeDataDefaults<-  nodeData<- addEdge addNode
#' @importFrom graph graphNEL
#' @importFrom Rgraphviz agread
#return a graphNEL object that only contans the node Name and isBool flags    
.getGraph <- function(x){
  DotFile <- tempfile(fileext=".dot")
  .Call("R_plotGh",x@pointer,getSample(x),DotFile,FALSE)
#  browser()
  #read dot from into Ragraph
  g <- agread(DotFile)
  #read all nodes and edges
  nodes <- AgNode(g)
  edges <- AgEdge(g)
  #read default attrs
  myNodeDataDefault <- nodeDataDefaults(g)
  
  #construct graph object based on these
  myGraph <- graphNEL(edgemode = "directed")
  
  #add node attr default
  for(i in 1:nrow(myNodeDataDefault))
    nodeDataDefaults(myGraph, myNodeDataDefault[i,"attr name"]) <- myNodeDataDefault[i,"attr value"]  
  nodeDataDefaults(myGraph, "name") <- ""

  attrNames <- as.vector(myNodeDataDefault[, "attr name"])
  #add nodes and its attr
  for(node in nodes){
    nodeID <- name(node)
    newNodeID <- paste("N", nodeID, sep = "_")
    myGraph <- addNode(newNodeID, myGraph)
    for(attrName in attrNames)
    nodeData(myGraph, newNodeID, attrName) <- as.vector(nodeData(g, nodeID,attrName))
    
    nodeData(myGraph, newNodeID, "name") <- nodeID
  }
  #add edges
  for(edge in edges){
#    browser()
    to <- paste("N", head(edge), sep = "_")
    from <- paste("N", tail(edge), sep = "_")
    myGraph <- addEdge(from, to, myGraph)
  }
  
  myGraph
}

#' @importMethodsFrom graph nodeData removeNode
.layoutGraph <- function(g,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,showHidden = FALSE){

  ##remove bool gates if necessary
  if(!boolean)
  {
    nodes<-nodeData(g,attr="isBool")
    for(i in 1:length(nodes))
    {
      if(as.logical(as.integer(nodes[[i]])))
        g <- removeNode(names(nodes[i]), g)
    }
  }
    #filter out hidden node
    if(!showHidden)
    {
        nodes<-nodeData(g,attr="hidden")
        for(i in 1:length(nodes))
          {
              if(as.logical(as.integer(nodes[[i]])))
                  g <- removeNode(names(nodes[i]), g)
            }
      }
  
  nAttrs <- list()
  
  nAttrs$label <- unlist(nodeData(g,attr="label"))
  nAttrs$fillcolor <- sapply(nodeData(g,attr="hidden")
                                ,function(thisHidden)
                              {
                                  ifelse(as.logical(as.integer(thisHidden)),"white","gray")
                                })
    #somehow lty doesn't work in nodeAttrs
      #  nAttrs$lty <- sapply(nodeData(g,attr="hidden")
      #      ,function(thisHidden)
      #      {
      #        ifelse(as.logical(as.integer(thisHidden)),"dotted","solid")
      #      })    
#           browser()

    Rgraphviz::layoutGraph(g,layoutType=layout,nodeAttrs=nAttrs
                              ,attrs=list(graph=list(rankdir="LR",page=c(8.5,11))
                                  ,node=list(fixedsize=FALSE
                        #              ,fillcolor="gray"
                                      ,fontsize=fontsize
                                      ,shape="ellipse"
                                  )
                              )
                          )
  }
  
#' @importMethodsFrom Rgraphviz renderGraph
.plotGatingTree<-function(g, ...){
  
  options("warn"=-1)
  
  lay <- .layoutGraph(g,...)
  
  renderGraph(lay)
  
  options("warn"=0)
}

#' plot a GatingHierarchy
#' 
#' Plot a tree/graph representing the GatingHierarchy
#' 
#' @param x \code{GatingHierarchy} to be plotted
#' @param y \code{missing}.
#' @param ... other arguments:
#' \itemize{
#' \item{layout}:
#' 		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
#'	\item{width}:
#' 		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
#'	\item{height}:		
#' 		See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
#'	\item{fontsize}:
#'      See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
#'	\item{labelfontsize}:		
#'  	See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
#'	\item{fixedsize}:		
#'  	See \code{\link[Rgraphviz]{layoutGraph}} in package Rgraphviz
#' }
#' 
#' @param boolean \code{TRUE|FALSE} logical specifying whether to plot boolean gate nodes. Defaults to FALSE.
#' 	
#' @examples 
#' \dontrun{
#' 	#G is a GatingHierarchy
#' 	plot(G);
#' }
#' 
#' @rdname plot-methods
#' @aliases
#' plot,GatingHierarchy,missing-method
#' plot,GatingHierarchy,numeric-method
#' plot,GatingHierarchy,character-method
#' @export 
#' @importFrom stats4 plot
setMethod("plot",c("GatingHierarchy","missing"),function(x,y,...){
      
#           browser()
      g <- .getGraph(x)
      .plotGatingTree(g,...)
      
    })

.getAllDescendants <- function(gh,startNode,nodelist){
  
  children_nodes <- getChildren(gh,startNode)
  if(length(children_nodes)>0){
    for(this_parent in children_nodes){
      nodelist$v <- c(nodelist$v, this_parent)
      .getAllDescendants (gh,this_parent,nodelist)
    }  
  }
  
}
#' plot a subgraph
#' @rdname plot-methods
#' @importMethodsFrom graph subGraph
setMethod("plot",c("GatingHierarchy","numeric"),function(x,y,...){
      
      
      # get graphNEL object
      g <- .getGraph(x)
      
      
      if(length(y)==1){#use it as the root
        nodelist <- new.env(parent=emptyenv())
        nodelist$v <-integer()
        .getAllDescendants (x,y,nodelist)  
        
        
        
        nodelist$v <- c(nodelist$v,y)
        #assume the number y is consistent with  R graph node name: N_x
        subNode_Ind <- nodelist$v
        
        
      }else{
        #when y is a vector, use it to subset the graph
        subNode_Ind <- y
      }
      
      subNodes <- paste("N",subNode_Ind-1,sep="_")
      if(length(subNodes)<=1){
        stop("Rgraphviz doesn't know how to plot leaf node!")
      }
      g <- subGraph(subNodes, g)
      .plotGatingTree(g,...)
      
    })

setMethod("plot",c("GatingHierarchy","character"),function(x,y,...){
      
      plot(x,.getNodeInd(x,y))
      
    })

setMethod("show","GatingHierarchy",function(object){
			cat("Sample: ",getSample(object),"\n");
			cat("GatingHierarchy with ",length(getNodes(object))," gates\n");
			cat("\n")
		})

    
#' Retrieve a specific keyword for a specific sample in a \code{GatingHierarchy} or or set of samples in a \code{GatingSet} or \code{GatingSetList}
#' 
#' Retrieve a specific keyword for a specific sample in a \code{GatingHierarchy} or or set of samples in a \code{GatingSet} or \code{GatingSetList}
#' 
#' @details See \code{keyword} in Package `flowCore' 
#' 
#' @param object \code{GatingHierarchy} or \code{GatingSet} or \code{GatingSetList}
#' @param keyword \code{character} specifying keyword name. When \code{missing}, extract all keywords.
#' 
#' @seealso \code{\link[flowCore]{keyword-methods}}
#' 
#' @aliases 
#' keyword 
#' keyword,GatingHierarchy,character-method 
#' keyword,GatingHierarchy,missing-method
#' keyword,GatingSet,character-method
#' keyword,GatingSet,missing-method
#' keyword,GatingSetList,character-method
#' keyword,GatingSetList,missing-method
#' @examples 
#'     \dontrun{
#'       #get all the keywords from all samples
#'       keyword(G)
#'       #get all the keywords from one sample
#'       keyword(G[[1]])
#'       #get single keyword from all samples   
#'       keyword(G, "FILENAME")
#'       #get single keyword from one sample   
#'       keyword(G[[1, "FILENAME") 
#'     }

#' @importFrom flowCore keyword
#' @export 
setMethod("keyword",c("GatingHierarchy","character"),function(object,keyword){
          
			keyword(object)[[keyword]]
		})
setMethod("getKeywords",c("GatingHierarchy","missing"),function(obj,y){
            stop("'getKeywords' is defunct. use 'keyword' instead! ")
      
			keyword(getData(obj))
		})
setMethod("keyword",c("GatingHierarchy","missing"),function(object,keyword = "missing"){
      
      flowCore::keyword(getData(object, use.exprs = FALSE))
    })    

#'  Get the names of all nodes in a gating hierarchy.
#' 
#'   \code{getNodes} returns a character vector of names of the nodes (populations) in the \code{GatingHierarchy}.
#' @param x A \code{GatingHierarchy}
#' @param y A \code{character} the name or full(/partial) gating path of the population node of interest.  Or, a \code{numeric} index into the node list of nodes in the \code{GatingHierarchy}.
#' @param order \code{order=c("regular","tsort","bfs")} returns the nodes in regular, topological or breadth-first sort order.
#'     "regular" is default.
#' @param isPath (Deprecated by 'path') A \code{logical} scalar to tell the method whether to return the full gating path or just terminal node name
#' @param path A \code{character} or \code{numeric} scalar. when \code{numeric}, it specifies the fixed length of gating path (length 1 displays terminal name).
#'              When \code{character}, it can be either 'full' (full path, which is default) or 'auto' (display the shortest unique gating path from the bottom of gating tree).
#' @param prefix A \code{character} scalar to tell the method whether to add internal node index as the prefix to the node name (only valid when 'path' is set to 1).
#'                                      the valid values are "auto", "none", "all".  
#' @param ... Additional arguments.
#' 
#' @details 
#' integer indices of nodes are based on regular order,so whenver need to map from character node name to integer node ID,make sure
#' to use default order which is regular.
#' @return 
#' getNodes returns a \code{character} vector of node/population names, ordered appropriately.
#' 
#' @examples
#'   \dontrun{
#'     #G is a gating hierarchy
#'     getNodes(G[[1], path = 1])#return node names (without prefix)
#'     getNodes(G[[1], path = 1, prefix = "all"])#return node names with unqiue ID
#'     getNodes(G[[1], path = 1, prefix = "auto"])#prepend unqiue ID as needed 
#'     getNodes(G[[1]],path = "full")#return the full path
#'     getNodes(G[[1]],path = 2)#return the path as length of two 
#'     getNodes(G[[1]],path = "auto)#automatically determine the length of path 
#'     setNode(G,"L","lymph")
#'   }
#' @aliases
#' getNodes
#' getNodes-methods
#' getNodes,GatingHierarchy-method
#' @importFrom BiocGenerics duplicated
#' @importFrom plyr ddply .
setMethod("getNodes","GatingHierarchy",function(x,y=NULL,order="regular", path = "full", prefix = c("none", "all", "auto"), showHidden = FALSE, ...){
      
            prefix <- match.arg(prefix)
            thisCall <- match.call()
            args <- names(thisCall)
            #take care the legacy argument 
            if("isPath"%in%args){
              warning("'isPath' is deprecated by 'path'.")
              isPath <- list(...)$isPath
              
              if(isPath){
                path <- "full"
                prefix <- "none"
              }else{
                path <- 1
                prefix <- "auto"
              }
              
              if("path"%in%args)
                warning("'path' argument is set to '", path, "' automatically!")               
              if("prefix"%in%args)
                warning("'prefix' is set to '", prefix, "' automatically!")
              
            }else{
              if(path == 1)
                isPath <- FALSE #passed to c API
              else
                isPath <- TRUE #passed to c API
               
              
            }
              
            order <- match.arg(order,c("regular","tsort","bfs"))
			
            orderInd <- match(order,c("regular","tsort","bfs"))
			
            orderInd <- orderInd-1
			
			nodeNames <- .Call("R_getNodes",x@pointer,getSample(x),as.integer(orderInd),isPath,showHidden)

			
            if(is.numeric(path)){
              if(path == 1){
                if(prefix != "all"){
                  dotPos <- regexpr("\\.",nodeNames)
                  #get unique IDs for each node
                  NodeIDs <- as.integer(substr(nodeNames,0,dotPos-1))
                  #strip IDs from nodeNames 
                  nodeNames <- substr(nodeNames,dotPos+1,nchar(nodeNames))
                   
                  #add ID back when prefix == auto for those ambiguous nodeNames
                  if(prefix == "auto"){
                    toAppendIDs<-duplicated(nodeNames)
                    nodeNames[toAppendIDs]<-paste(NodeIDs[toAppendIDs],nodeNames[toAppendIDs],sep=".")  
                  }
                }
                    
              }else{
                
                #truncate the path 
                nodeNames <- sapply(nodeNames, function(nodeName){
                                      
                                      if(nodeName == "root")
                                        nodeName
                                      else{
                                        nodes <- strsplit(nodeName, split = "/")[[1]][-1]
                                        nNodes <- min(length(nodes), path)
                                        nodes <- rev(rev(nodes)[1:nNodes])
 
                                        paste(nodes, collapse = "/")  
                                      }
                                            
                                    }, USE.NAMES = FALSE)
              }              
                
            }else if(path == "auto"){
              
              nodePath <- nodeNames
              nodeNames <- basename(nodePath)
              names(nodeNames) <- nodePath #init the mapping (nodePath vs final output)
              
#              browser()
              #pick duplicated node names to prepend their ancesters until their paths are unambiguous 
              dup <- unique(nodeNames[duplicated(nodeNames)])
              toModify <- nodeNames%in%dup
              toModifyNode <- nodeNames[toModify]
              toModifyID <- nodePath[toModify]
                            
              toModifyRes <- ddply(data.frame(toModifyID, toModifyNode), .(toModifyNode), function(thisDF){
#                    browser()
                    thisRes <- thisDF
                    thisNames <- as.vector(thisRes[,"toModifyNode"])
                    while(anyDuplicated(thisNames) > 0){
                      #try to paste parent node
                      thisRes <- ddply(thisRes, .(toModifyID), function(thisRow){
#                                        browser()
                                        thisID <- as.vector(thisRow[,"toModifyID"])
                                        ppath <- getParent(x, thisID)
                                        pn <- basename(ppath)
                                        #paste parent
                                        thisRow[,"toModifyNode"] <- paste(pn, thisRow[, "toModifyNode"], sep = "/")
                                        #update the current id to parent ID 
                                        thisRow[, "toModifyID"] <- ppath
                                        thisRow
                                      })
                      thisNames <- as.vector(thisRes[,"toModifyNode"])                                                        
                    }
                    thisRes[, "toModifyID"] <- thisDF[, "toModifyID"] 
                    thisRes                      
                  })
              #update the nodeNames with modified names
              nodeNames[as.vector(toModifyRes[,"toModifyID"])] <- as.vector(toModifyRes[,"toModifyNode"])
              nodeNames <- as.vector(nodeNames)
                    
            }else if(path != "full" )
			  stop("Invalid 'path' argument. The valid input is 'full' or 'auto' or a numeric value.")
			
			
			if(!is.null(y))
			  stop("'y' argument is not supported!")

			
			nodeNames
		})


#' Return the name of the parent population or a list of child populations of the current population in the GatingHierarchy
#' 
#' Returns the name of the parent population or a character/numeric vector of all the children of the current population in the given \code{GatingHierarchy}
#' @param obj A \code{GatingHierarchy}
#' @param y a \code{character/numeric} the name or full(/partial) gating path  or node indices of the node / population.
#' @param ... other arguments passed to \link{getNodes} methods
#' @return 
#'   \code{getParent} returns a \code{character} vector, the name of the parent population.
#'   \code{getChildren} returns a \code{character} or \code{numeric} vector of the node names or node indices of the child nodes of the current node. An empty vector if the node has no children.
#' @seealso \code{\link{getNodes}}
#' 
#' @examples
#'   \dontrun{
#'     #G is a gatinghierarchy
#'     #return the name of the parent of the fifth node in the hierarchy.
#'     getParent(G,getNodes(G[[1)[5])
#'     n<-getNodes(G,tsort=T)[4];
#'     getChildren(G,n);#Get the names of the child nodes of the 4th node in this gating hierarchy.
#'     getChildren(G,4);#Get the ids of the child nodes 
#'   }
#' @aliases
#' getParent
#' getParent-methods
#' getParent,GatingHierarchy,character-method
#' getParent,GatingHierarchy,numeric-method
#' getChildren
#' getChildren-methods
#' getChildren,GatingHierarchy,character-method
#' getChildren,GatingHierarchy,numeric-method
setMethod("getParent",signature(obj="GatingHierarchy",y="numeric"),function(obj,y){
			#make sure the index conversion is done properly between C and R convention
#			browser()
			.Call("R_getParent",obj@pointer,getSample(obj),as.integer(y)-1)+1
		})
setMethod("getParent",signature(obj="GatingHierarchy",y="character"),function(obj,y, ...){
#			browser()
			ind<-.getNodeInd(obj,y)
			pind<-getParent(obj,ind)
			getNodes(obj, showHidden = TRUE, ...)[pind]
		})
setMethod("getChildren",signature(obj="GatingHierarchy",y="character"),function(obj,y,tsort=FALSE, ...){
			ind<-.getNodeInd(obj,y)
			cind<-getChildren(obj,ind)
			getNodes(obj, showHidden = TRUE, ...)[cind]
})
setMethod("getChildren",signature(obj="GatingHierarchy",y="numeric"),function(obj,y){
#			browser()
			.Call("R_getChildren",obj@pointer,getSample(obj),as.integer(y)-1)+1
			
		})
#
setMethod("getProp",signature(x="GatingHierarchy",y="character"),function(x,y,flowJo = FALSE){
			#Return the proportion of the population relative to the parent and relative to the total.
			#y is nodename
			
            ind<-.getNodeInd(x,y)
			stats<-.getPopStat(x,ind)
			if(flowJo)
				unname(stats$flowJo["proportion"])
			else
				unname(stats$flowCore["proportion"])
			
		})
setMethod("getTotal",signature(x="GatingHierarchy",y="character"),function(x,y,flowJo = FALSE){
            ind<-.getNodeInd(x,y)
      
			stats<-.getPopStat(x,ind)
			if(flowJo)
				unname(stats$flowJo["count"])	
			else
				unname(stats$flowCore["count"])
#			browser()
			
		})


.getPopStat<-function(x,y){
	stopifnot(!missing(y))
	
	if(is.character(y))
      stop("y has to be numeric!")
    
	stats<-.Call("R_getPopStats",x@pointer,getSample(x),as.integer(y)-1)
	
	pInd<-try(.Call("R_getParent", x@pointer, getSample(x), as.integer(y) -1),silent=T)
	
	
	if(class(pInd)=="try-error")#if parent exist
		pstats<-stats
	else
	{
		pInd<-pInd+1 #convert c convention to R
		pstats<-.Call("R_getPopStats",x@pointer,getSample(x),as.integer(pInd)-1)
	}
		
	
#	browser()	
	list(flowCore=c(proportion=as.numeric(ifelse(pstats$FlowCore["count"]==0
										,1
										,stats$FlowCore["count"]/pstats$FlowCore["count"]
										))
					,count=as.numeric(stats$FlowCore["count"]))
		,flowJo=c(proportion=as.numeric(ifelse(pstats$FlowJo["count"]==0
										,1
										,stats$FlowJo["count"]/pstats$FlowJo["count"]
										))
					,count=as.numeric(stats$FlowJo["count"]))	
		)
}
setMethod("getPopStats","GatingHierarchy",function(x,...){

			
        nodeNamesPath<-getNodes(x,...)
       nodeNames<-getNodes(x, path = 1, prefix = "auto", ...)
                           
          
       stats<-mapply(nodeNames,nodeNamesPath,FUN = function(thisName,thisPath){
                       ind <- .getNodeInd(x,thisName)
                
						curStats<-.getPopStat(x,ind)
                        curRes<-c(thisPath
									,curStats$flowCore["proportion"]
									,curStats$flowJo["count"]
									,curStats$flowCore["count"]
									,curStats$flowJo["proportion"]
									,nodeNames[ind]
									)
						
						curRes			
                      },SIMPLIFY = FALSE)
#			browser()
			m<-do.call("rbind",stats)

			rownames(m)<-NULL;
			
			m<-data.frame(m);
			m[,2]<-as.numeric(as.character(m[,2]));
			m[,3]<-as.numeric(as.character(m[,3]));
			m[,4]<-as.numeric(as.character(m[,4]));
			m[,5]<-as.numeric(as.character(m[,5]))
			m[,6]<-as.character(m[,6])


			m[1,c(2)]<-1;
			colnames(m)<-c("pop.name","flowCore.freq","flowJo.count","flowCore.count","flowJo.freq","node")
			rn<-m[,1]
			m<-data.table(m[,2:6])
      rownames(m)<-rn
			m
		})
    
.computeCV_gh <- function(gh){
  
    x<-getPopStats(gh)
    rn<-rownames(x)
    x<-as.data.frame(x)
    rownames(x)<-rn	
    cv<-apply(as.matrix(x[,c("flowJo.count","flowCore.count")]),1,function(y)IQR(y)/median(y));
    cv<-as.matrix(cv,nrow=length(cv))
    cv[is.nan(cv)]<-0
    rownames(cv)<-basename(as.character(rownames(x)))
    cv
}        
#' @importFrom lattice barchart
setMethod("plotPopCV","GatingHierarchy",function(x,m=2,n=2,...){
      cv <- .computeCV_gh(x)
      return(barchart(cv,xlab="Coefficient of Variation",..., par.settings=ggplot2like));
    })


#'  Return the flowCore gate definition associated with a node in a GatingHierarchy/GatingSet.
#' 
#'  Return the flowCore gate definition object associated with a node in a \code{GatingHierarchy} or \code{GatingSet} object.
#' @param obj A \code{GatingHierrarchy} or \code{GatingSet}
#' @param y A \code{character} the name or full(/partial) gating path of the node of interest.  Or, a \code{numeric} index into the node list of nodes in the \code{GatingHierarchy} or \code{GatingSet}.
#' @return  A gate object from \code{flowCore}. Usually a \code{polygonGate}, but may be a \code{rectangleGate}. Boolean gates are represented by a \code{"BooleanGate"} S3 class. This is a list boolean gate definition that references populations in the GatingHierarchy and how they are to be combined logically. If \code{obj} is a \code{GatingSet}, assuming the trees associated with each \code{GatingHierarchy} are identical, then this method will return a list of gates, one for each sample in the \code{GatingSet} corresponding to the same population indexed by \code{y}.
#' @note You should not have to deal with boolean gates. It is sufficient to retrieve the contents of a boolean gate node with \code{getData}.
#' @seealso \code{\link{getData}} \code{\link{getNodes}}
#' @examples
#'   \dontrun{	#gh is a GatingHierarchy
#'     getGate(gh,5) #return the gate for the fifth node in the tree.
#'     getGate(gh,getNodes(gh)[5]) #return the gate for the fifth node in the tree, but fetch it by name.
#'     #G is a GatingSet
#'     getGate(G,5) #return a list of gates for the fifth node in each tree
#'   }
#' @aliases 
#' getGate
#' getGate,GatingHierarchy,character-method
#' getGate,GatingHierarchy,numeric-method
#' getGate,GatingSet,numeric-method
#' getGate,GatingSet,character-method
setMethod("getGate",signature(obj="GatingHierarchy",y="character"),function(obj,y){
			
#			browser()
            ind<-.getNodeInd(obj,y)
			g<-getGate(obj,ind)
			g
			
		})
    
#' return gate y for a given hierarchy (by index)
#' Note that this index is ordered by regular sorting method
#' @importFrom flowCore polygonGate rectangleGate
setMethod("getGate",signature(obj="GatingHierarchy",y="numeric"),function(obj,y,tsort=FALSE){
			vertexID=y-1
			if(vertexID<=0)
				return (NA)
			else
			{

				g<-.Call("R_getGate",obj@pointer,getSample(obj),vertexID)
				filterId <- getNodes(obj, showHidden = TRUE, path = 1, prefix = "auto")[y]
				if(g$type==1)
				{
					
					
					mat<-matrix(c(g$x,g$y),ncol=2,dimnames=list(NULL,g$parameters))
					if(nrow(mat)==2)#convert to rectangleGate
						rectangleGate(.gate=mat,filterId=filterId)
					else
						polygonGate(.gate=mat,filterId=filterId)
				}else if(g$type==2)
					rectangleGate(.gate=matrix(g$range,dimnames=list(NULL,g$parameters)),filterId=filterId)
				else if(g$type==3)
				{
					
					refPaths<-unlist(lapply(g$ref,function(curPath){
										paste(curPath,collapse="/")
										
									})
								)
					
                    #get rid of the first op
                    g$v2[1] <- "" 
                    boolExpr <- paste(g$v2, g$v,refPaths,sep="")
                    boolExpr <- paste(boolExpr,collapse="")
#                   browser()
                    boolExpr <- as.symbol(boolExpr)

                    g <- eval(substitute(booleanFilter(xx),list(xx=boolExpr)))
					g
				}else
					stop("not supported gate type",g$type)
				
			}
		})
        
.getNodeInd<-function(obj,y, ...){
#  browser()
  if(grepl("/",y)){
    
    this_path <- strsplit(split="/",y)[[1]]
    #remove empty string
    isEmpty <- sapply(this_path,function(this_char)nchar(this_char)==0)
    this_path <- this_path[!isEmpty]
    
    ind <- .Call("R_getNodeID",obj@pointer,getSample(obj),this_path)
    ind + 1 # convert to R index
  }else{
    allNodes <- getNodes(obj, path = 1, prefix = "auto", showHidden = TRUE,...)
    ind<-match(y,allNodes)#strict string match  
    if(is.na(ind)||length(ind)==0){
        stop("Node:", y," not found!")
      }else if(length(ind)>1){
        stop(y," is ambiguous!")
    }else{
      ind  
    }
    
  }
  
  
}


#'  Get the membership indices for each event with respect to a particular gate in a GatingHierarchy
#' 
#'   Returns a logical vector that describes whether each event in a sample is included or excluded by this gate. 
#' @param obj A \code{GatingHierarchy} representing a sample.
#' @param y A \code{character} or \code{numeric} giving the name or full(/partial) gating path or index of the population / node of interest.
#' @details  Returns a logical vector that describes whether each event in the data file is included in the given gate of this \code{GatingHierarchy}. The indices are for all events in the file, and do not reflect the population counts relative to the parent but relative to the root. To get population frequencies relative to the parent one cross-tabulate the  indices of \code{y} with the indices of its parent.
#' @return  A logical vector of length equal to the number of events in the FCS file that determines whether each event is or is not included in the current gate.
#' @note Generally you should not need to use \code{getIndices} but the more convenient methods \code{getProp} and \code{getPopStats} which return population frequencies relative to the parent node.
#' The indices returned reference all events in the file and are not directly suitable for computing population statistics, unless subsets are taken with respect to the parent populations.
#' @seealso \code{\link{getProp}}, \code{\link{getPopStats}}
#' @examples
#'   \dontrun{
#'     #G is a gating hierarchy
#'     #Return the indices for population 5 (topological sort)
#'     getIndices(G,getNodes(G,tsort=TRUE)[5]);
#' }
#' 
#' @aliases 
#' getIndices
#' getIndices-methods
#' getIndices,GatingHierarchy,character-method
#' getIndices,GatingHierarchy,numeric-method
#' getIndices,GatingSet,name-method
#' @importFrom ncdfFlow getIndices
#' @export 
setMethod("getIndices",signature(obj="GatingHierarchy",y="character"),function(obj,y){
			
#			browser()
			getIndices(obj,.getNodeInd(obj,y))
			
		})
    
setMethod("getIndices",signature(obj="GatingHierarchy",y="numeric"),function(obj,y){
			

			.Call("R_getIndices",obj@pointer,getSample(obj),as.integer(y-1))
			
		})
    
setGeneric("isGated",function(obj, y, ...)standardGeneric("isGated"))    
setMethod("isGated",signature(obj="GatingHierarchy",y="character"),function(obj,y){
      
#			browser()
      isGated(obj,.getNodeInd(obj,y))
      
    })

setMethod("isGated",signature(obj="GatingHierarchy",y="numeric"),function(obj,y){
      
      
      .Call("R_getGateFlag",obj@pointer,getSample(obj),as.integer(y-1))
      
    })    
        
#' get gated flow data from a GatingHierarchy/GatingSet/GatingSetList
#' 
#' get gated flow data from a GatingHierarchy/GatingSet/GatingSetList 
#'
#' @details 
#' Returns a flowFrame/flowSet containing the events in the gate defined at node \code{y}. 
#' Subset membership can be obtained using \code{getIndices}. 
#' Population statistics can be obtained using \code{getPop} and \code{getPopStats}. 
#' When calling \code{getData} on a GatingSet,the trees representing the GatingHierarchy for each sample in the GaingSet are presumed to have the same structure.
#' To update the data, use \code{flowData} method.

#' @param obj A \code{GatingHierarchy}, \code{GatingSet} or \code{GatingSetList} object.
#' @param  y \code{character}  the node name or full(/partial) gating path or \code{numeric} node index. 
#'                             	If not specified, will return the complete flowFrame/flowSet at the root node.
#' @param ... arguments passed to ncdfFlow::[[  
#' 
#' @return  
#' A \code{flowFrame} object if \code{obj} is a GatingHierarchy. 
#' A \code{flowSet} or \code{ncdfFlowSet} if a \code{GatingSet}.
#' A \code{ncdfFlowList} if a \code{GatingSetList}. 
#' @seealso
#'   \code{\link{flowData}} \code{\link{getIndices}} \code{\link{getProp}} \code{\link{getPopStats}}
#' 
#' @examples
#'   \dontrun{
#'     #G is a GatingSet
#'     geData(G,3) #get a flowSet constructed from the third node / population in the tree.
#'     geData(G,"cd4")
#'     
#'     #gh is a GatingHierarchy
#'     getData(gh)
#' }
#' @aliases 
#' getData,GatingHierarchy,missing-method
#' getData,GatingHierarchy,numeric-method
#' getData,GatingHierarchy,character-method
#' getData,GatingSet,ANY-method
#' getData,GatingSet,missing-method
#' getData,GatingSet,numeric-method
#' getData,GatingSet,character-method
#' getData,GatingSet,name-method
#' getData,GatingSetList,name-method
#' getData,GatingSetList,ANY-method
#' @rdname getData-methods
#' @export 
setMethod("getData",signature(obj="GatingHierarchy",y="missing"),function(obj,y, ...){
      if(!obj@flag){
        stop("Must gate the data before fetching data");
      }
      
      fs <- flowData(obj)

      fs[getSample(obj)][[1,...]]
      
    })

setMethod("getData",signature(obj="GatingHierarchy",y="numeric"),function(obj,y, ...){
        this_data <- getData(obj, ...)                        
        if(y == 0){
          return (this_data)  
        }else{
          this_indice <- getIndices(obj,y)
          return (this_data[this_indice,])
        }
})
    
setMethod("getData",signature(obj="GatingHierarchy",y="character"),function(obj,y, ...){
      
      getData(obj,.getNodeInd(obj,y), ...)
      
    })

.isBoolGate<-function(x,y){
	return (class(getGate(x,y))=="booleanFilter")
}


getAxisLabels <- function(obj,...){
              obj@axis[[1]]
		}

#' Return a list of transformations or a transformation in a flowJo workspace/GatingHierarchy
#'
#' Return a list of all the transformations or a transformation in a flowJo workspace/GatingHierarchy
#' 
#' @param x A \code{flowJoWorkspace} or \code{GatingHierarchy} object
#' 
#' @details
#' Returns a list of the transformations or a transformation in the flowJo workspace. The list is of length \code{L}, where \code{L} is the number of distinct transformations applied to samples in the \code{flowJoWorkspace}. Each element of \code{L} is itself a \code{list} of length \code{M}, where \code{M} is the number of parameters that were transformed for a sample or group of samples in a \code{flowJoWorkspace}. For example, if a sample has 10 parameters, and 5 are transformed during analysis, using two different sets of transformations, then L will be of length 2, and each element of L will be of length 5. The elements of \code{L} represent channel- or parameter-specific transformation functions that map from raw intensity values to channel-space used by flowJo.
#' this method currently is used convert transformation funtion from c++ to R
#' mainly for transforming range info
#' @return 
#' lists of functions, with each element of the list representing a transformation applied to a specific channel/parameter of a sample. 
#' @examples
#' \dontrun{
#' 	#Assume f is a flowJoWorkspace
#' 	getTransformations(f);
#' }
#' @aliases
#' getTransformations
#' getTransformations-methods
#' getTransformations,flowJoWorkspace-method
#' getTransformations,GatingHierarchy-method
setMethod("getTransformations","GatingHierarchy",function(x){
#			browser()
      .getTransformations(x@pointer,getSample(x))
		})
.getTransformations <- function(pointer,sampleName){        
    trans<-.Call("R_getTransformations",pointer,sampleName)
    lapply(trans,function(curTrans){
#						browser()
          if(curTrans$type=="log")
          {
            
            f <- function(x){
              ifelse(x>0,log10((x)/max_val)/decade+offset,min_val)
            }
            assign("decade", curTrans$decade, environment(f))
            assign("offset", curTrans$offset, environment(f))
            assign("min_val", 0, environment(f))
            assign("max_val", 262143, environment(f))
            
            attr(f,"type")<-"flog"
            
          }
          else if(curTrans$type=="lin")
          {
            f<-function(x){x*64}
            attr(f,"type")<-"gateOnly"
            
            
          }else 
          {
            #define the dummy spline function(simplied version of the one from stats package)
            f<-function (x, deriv = 0) 
            {
              deriv <- as.integer(deriv)
              if (deriv < 0 || deriv > 3) 
                stop("'deriv' must be between 0 and 3")
              if (deriv > 0) {
                z0 <- double(z$n)
                z[c("y", "b", "c")] <- switch(deriv, list(y = z$b, b = 2 * 
                            z$c, c = 3 * z$d), list(y = 2 * z$c, b = 6 * z$d, 
                        c = z0), list(y = 6 * z$d, b = z0, c = z0))
                z[["d"]] <- z0
              }
              
              res <- stats::: .splinefun(x,z)
              if (deriv > 0 && z$method == 2 && any(ind <- x <= z$x[1L])) 
                res[ind] <- ifelse(deriv == 1, z$y[1L], 0)
              res
            }
            #update the parameters of the function
            z<-curTrans$z
            z$n<-length(z$x)
            z$method<-curTrans$method
            assign("z",z,environment(f))
            
            attr(f,"type")<-curTrans$type	
          }
          
          return (f)
        })
  }
    

#'  Retrieve the compensation matrices from a flowJo Workspace or GatingHierarchy
#' 
#'  Retrieve the compensation matrices from a flowJo workspace or GatingHierarchy.
#' @param x A \code{flowJoWorkspace} or \code{GatingHierarchy} object.
#' @details Return all the compensation matrices in a flowJoWorkspace object or a compensation matrix in a GatingHierarchy.
#' @return 
#'   A list of \code{matrix} representing the spillover matrices in the \code{flowJoWorkspace}
#'   or a spillover matrix in \code{GatingHierarchy}
#' @seealso \code{\link{openWorkspace}}
#' @examples
#'   \dontrun{
#'     #ws is a flowJoWorkspace
#'   file<-"myworkspace.xml"
#'   ws<-openWorkspace(file)
#'   getCompensationMatrices(ws);
#' }
#' @aliases 
#' getCompensationMatrices
#' getCompensationMatrices-methods
#' getCompensationMatrices,flowJoWorkspace-method
#' getCompensationMatrices,GatingHierarchy-method
setMethod("getCompensationMatrices","GatingHierarchy",function(x){
			comp<-.Call("R_getCompensation",x@pointer,getSample(x))
			cid<-comp$cid
#			browser()
			if(cid=="")
				cid=-2
			if(cid!="-1" && cid!="-2"){
				marker<-comp$parameters
				compobj<-compensation(matrix(comp$spillOver,nrow=length(marker),ncol=length(marker),byrow=TRUE,dimnames=list(marker,marker)))
			}else if(cid=="-2"){
				#TODO the matrix may be acquisition defined.
#				message("No compensation");
				compobj<-NULL
			}
			else if(cid=="-1")
			{
				##Acquisition defined compensation.
				nm<-comp$comment
				
				if(grepl("Acquisition-defined",nm)){
					###Code to compensate the sample using the acquisition defined compensation matrices.
#					message("Compensating with Acquisition defined compensation matrix");
					#browser()
					compobj<-compensation(spillover(getData(x))$SPILL)
						
				}
				
			}
			compobj
			
})

#' fussy match of marker/channel names
#' @param pd pData of parameters of flowFrame
#' @param name \code{character} the string to match
#' @param fix whether to do regexpr match
#' @param parital whether to do the complete word match or parital match

.flowParamMatch <- function(pd, name, fix = FALSE, partial = FALSE) {
  
  # try to compelete word match by following with a space or the end of string
  if (partial) 
    pname <- name 
  else 
    pname <- paste0(name, "([ ]|$)")
  
  if (fix) {
    ind <- which(toupper(pd$name) %in% toupper(name))
  } else {
    ind <- which(grepl(pname, pd$name, ignore.case = T))
  }
  
  if (length(ind) == 0) {
    # try marker name
    ind <- which(unlist(lapply(pd$desc, function(x) {
                  # split by white space and then match each individual string
                  if (fix) {
                    any(unlist(lapply(strsplit(x, " "), function(y) toupper(y) %in% toupper(name))))
                  } else {
                    grepl(pattern = pname, x, ignore.case = T)
                  }
                })))
  }
  ind
}
#' get channel and marker information from a \code{flowFrame} that matches to the given keyword 
#' 
#' This function tries best to guess the flow parameter based on the keyword supplied by \code{name}
#' It first does a complete word match(case insensitive) between \code{name} and flow channels and markers.
#' If there are duplcated matches, throw the error. If no matches, it will try the partial match.
#'   
#' @return 
#' an one-row \code{data.frame} that contains "name"(i.e. channel) and "desc"(i.e. stained marker) columns.
#' 
#' @param frm \code{flowFrame} object
#' @param name \code{character} the keyword to match
#' @param ... other arguments: not used.
#' @export 
getChannelMarker <- function(frm, name, ...) {
  pd <- pData(parameters(frm))
  # try complete match first
  ind <- .flowParamMatch(pd, name, ...)
  
  if (length(ind) > 1) {
    stop("multiple markers matched: ", name)
  }
  
  if (length(ind) == 0) {
    # if no match then give a second try to patial match
    ind <- .flowParamMatch(pd, name, partial = TRUE, ...)
    if (length(ind) == 0) 
      stop("can't find ", name) else if (length(ind) > 1) 
      stop("multiple markers matched: ", name) else warning(name, " is partially matched with ", pd[ind, c("name", "desc")])
  }
  
  pd <- pd[ind, c("name", "desc")]
  pd[, "name"] <- as.vector(pd[, "name"])
  pd[, "desc"] <- as.vector(pd[, "desc"])
  pd
  
}


#TODO: to inverse transform the range in order to display the raw scale
setMethod("plotGate",signature(x="GatingHierarchy",y="character"),function(x,y,...){
			
      ind <- sapply(y, function(i).getNodeInd(x, i))
			plotGate(x,ind,...)
			
})
setMethod("plotGate",signature(x="GatingHierarchy",y="missing"),function(x,y,...){
	
        y <- getNodes(x)
        y <- setdiff(y,"root")
        
		plotGate(x,y,...)
		})
#' @importFrom gridExtra grid.arrange   
setMethod("plotGate", signature(x="GatingHierarchy",y="numeric")
                    , function(x, y
                                , bool=FALSE
                                , arrange.main = getSample(x),arrange=TRUE,merge=TRUE
                                , par.settings = list()
                                , gpar = NULL
                                , projections = list()
                                , ...){
			if(!x@flag){
				message("Can't plot until you gate the data \n");
				return();
			}
			
			
            theme.novpadding <- list(layout.heights = list(top.padding = 0,
                                                            main.key.padding = 0,
                                                            key.axis.padding = 0,
                                                            axis.xlab.padding = 0,
                                                            xlab.key.padding = 0,
                                                            key.sub.padding = 0,
                                                            bottom.padding = 0)
                                      , layout.widths = list(left.padding = 0,
                                                              key.ylab.padding = 0,
                                                              ylab.axis.padding = 0,
                                                              axis.key.padding = 0,
                                                              right.padding = 0)
                                      , par.xlab.text = list(cex = 0.7, col = gray(0.3))
                                      , par.ylab.text = list(cex = 0.7,  col = gray(0.3))
                                      , par.main.text = list(cex = 0.8)
                                      , axis.components = list(bottom = list(tck =0.5)
                                                              , left = list(tck =0.5))
                                      , axis.text = list(cex = 0.5)
                                  )
            
                        
            
            
            par.settings <- lattice:::updateList(theme.novpadding, par.settings)
            
            #convert popname to id
      
            prjNodeInds <- try(sapply(names(projections),.getNodeInd, obj = gh), silent = TRUE)
            if(class(prjNodeInds) == "try-error")
              stop("Invalid 'projections': ", geterrmessage())
            
            names(projections) <- prjNodeInds
            #match given axis to channel names
            fr <- getData(x, use.exprs = FALSE)
            projections <- lapply(projections, function(thisPrj){
                                  sapply(thisPrj, function(thisAxis)getChannelMarker(fr, thisAxis)[["name"]])      
                                })
            
            
			plotList<-.mergeGates(x,y,bool,merge, projections = projections)
			plotObjs<-lapply(plotList,function(y){
                        
                        if(is.list(y)){
                          myPrj <- projections[[as.character(y[["popIds"]][1])]]
                        }else{
                          myPrj <- projections[[as.character(y)]]
                        }
                        if(is.null(myPrj)){
                          formula <- NULL
                        }else{
                          formula <- mkformula(myPrj)
                        }
                        
						#defaultCond is passed to flowViz::xyplot to disable lattice strip
						return(.plotGate(x, y, par.settings = par.settings, formula = formula, ...))
					})
#			browser()
			if(arrange)			
				do.call(grid.arrange,c(plotObjs,main = arrange.main,gpar))
			else
				plotObjs
			
})
.mergeGates<-function(gh,i,bool,merge, projections = list()){
	##filter out boolean gates when bool==FALSE
#	browser()
	if(!bool)
	{
		
		boolInd<-unlist(lapply(i,.isBoolGate,x=gh))
#		browser()
		if(length(which(boolInd))>0)
			message("skipping boolean gates!")
		i<-i[!boolInd]
	}
	
	plotList<-poplist<-as.list(i)
	names(plotList)<-plotList
	if(length(plotList)==0)
		stop("not gates to plot")
      
    
	if(merge)
	{
		#check if they have same parents and parameters
		keylist <- sapply(plotList,function(y){
					
					if(!.isBoolGate(gh,y))
					{
						curGate<-getGate(gh,y)
                        thisParam <- parameters(curGate) 													
						if(extends(class(curGate),"filter"))
						{
							pid<-getParent(gh,y)
                            myPrj <- projections[[as.character(y)]]
                            if(is.null(myPrj)){
                              myPrj <- thisParam
                            }else{
                              if(!all(is.element(thisParam, myPrj)))
                                stop("Given projection ("
                                        , paste(myPrj,collapse = ",")
                                        , ") is not consistent with the gate '"
                                        , y 
                                        ,"' dimension ("
                                        , paste(thisParam,collapse = ",")
                                    , ")")
                            }
							params<-paste(sort(unname(myPrj)),collapse="")
							
							paste(pid,params,sep="|")
						}else
							return(-1)
						
					}else
						return(-2)
				})
		
		invalidNodes<-sapply(keylist,function(key)key==-1)
		poplist<-poplist[!invalidNodes]
		plotList<-plotList[!invalidNodes]
		keylist<-keylist[!invalidNodes]
		
		boolNodes<-sapply(keylist,function(key)key==-2)
		keylist<-keylist[!boolNodes]
		
		#			browser()
		keylistFeq<-table(keylist)
		toMergeKeyList<-names(keylistFeq[keylistFeq>=2])
		# construct the a special list object to replace/del the one that needs to be merged
		for(curKey in toMergeKeyList)
		{
			#				browser()
			toMerge<-as.numeric(names(keylist[keylist==curKey]))
			toReplace<-sort(toMerge)[1]#replace the first merged child node with the merge list
			toRemove<-toMerge[!(toMerge==toReplace)]#remove other children
			
			toReplaceInd<-match(toReplace,poplist)
			toRemoveInd<-match(toRemove,poplist)
			#								browser()
			
			curPid<-as.numeric(strsplit(curKey,split="\\|")[[1]][1])#extract pid
			plotList[[toReplaceInd]]<-list(popIds=toMerge,parentId=curPid)
			plotList[toRemoveInd]<-NULL
			poplist[toRemoveInd]<-NULL#make sure syn y as well vector since it is used to index plotList 
		}
		
	}
	plotList
}
#copy from sfsmisc package
pretty10exp<-function (x, drop.1 = FALSE, digits.fuzz = 7) 
{
	eT <- floor(log10(abs(x)) + 10^-digits.fuzz)
	mT <- signif(x/10^eT, digits.fuzz)
	ss <- vector("list", length(x))
	for (i in seq(along = x)) ss[[i]] <- if (x[i] == 0) 
					quote(0)
				else if (drop.1 && mT[i] == 1) 
					substitute(10^E, list(E = eT[i]))
				else if (drop.1 && mT[i] == -1) 
					substitute(-10^E, list(E = eT[i]))
				else substitute(A %*% 10^E, list(A = mT[i], E = eT[i]))
	do.call("expression", ss)
}

#' @param x a gatingHierarchy
#' @param data a flowFrame
.formatAxis <- function(x, data, xParam, yParam
                          , scales=list()
                          , marker.only = FALSE
                          , ...){
#	pd<-pData(parameters(data))
#	browser()
	xObj <- getChannelMarker(data,xParam)
	yObj <- getChannelMarker(data,yParam)
	
    if(marker.only){
      xlab <- as.character(ifelse(is.na(xObj[,"desc"]), xObj[,"name"], xObj[,"desc"]))
      ylab <- as.character(ifelse(is.na(yObj[,"desc"]), yObj[,"name"], yObj[,"desc"]))         
      
    }else
    {
      xlab <- sub("NA","",paste(unlist(xObj),collapse=" "))
      ylab <- sub("NA","",paste(unlist(yObj),collapse=" "))  
    }
	
#			browser()
	
#		xParam.ind<-match(xParam,pd$name)
#		yParam.ind<-match(yParam,pd$name)
        if(is.null(xParam)){
          x.labels <- NULL
        }else{
          x.labels<-getAxisLabels(x)[[xParam]]  
        }
          
        if(is.null(yParam)){
          y.labels <- NULL
        }else{
		  y.labels<-getAxisLabels(x)[[yParam]]
        }
		
		#init the scales and x,y lim
		
		xlim=range(data)[,xParam]
		ylim=range(data)[,yParam]
		
		#update axis when applicable
		if(!is.null(x.labels))
		{
			x.labels$label<-pretty10exp(as.numeric(x.labels$label),drop.1=TRUE)
			xscales<-list(x=list(at=x.labels$at,labels=x.labels$label))
			scales<-lattice:::updateList(xscales,scales)
			xlim=range(x.labels$at)
		}
		if(!is.null(y.labels))
		{	
			y.labels$label<-pretty10exp(as.numeric(y.labels$label),drop.1=TRUE)
			yscales<-list(y=list(at=y.labels$at,labels=y.labels$label))
			scales<-lattice:::updateList(scales,yscales)
			ylim=range(y.labels$at)
		}
		
	
	
	list(scales=scales,xlab=xlab,ylab=ylab)
}

#'  Update the name of one node in a gating hierarchy/GatingSet.
#' 
#'  \code{setNode} update the name of one node in a gating hierarchy/GatingSet.
#' @param value A \code{character} the name of the node.
#' @examples
#'   \dontrun{
#'     #G is a gating hierarchy
#'     getNodes(G[[1]])#return node names
#'     setNode(G,"L","lymph")
#'   }
#' @aliases 
#' setNode,GatingHierarchy,numeric,character-method
#' setNode,GatingHierarchy,character,character-method
#' @rdname setNode-methods
setMethod("setNode"
        ,signature(x="GatingHierarchy",y="numeric",value="character")
        ,function(x,y,value,...){

        .Call("R_setNodeName",x@pointer,getSample(x),as.integer(y-1),value)
    })

setMethod("setNode"
    ,signature(x="GatingHierarchy",y="character",value="character")
    ,function(x,y,value,...){
      setNode(x,.getNodeInd(x,y),value)
    })

#' hide/unhide a node
#' 
#' @param x \code{GatingHierarchy} object
#' @param y \code{numeric} node index
#' @param value \code{logical} whether to hide a node
#' @examples
#' \dontrun{ 
#'      setNode(gh, 4, FALSE) # hide a node
#'      setNode(gh, 4, TRUE) # unhide a node
#' }
#' @export 
#' @aliases 
#' setNode,GatingHierarchy,numeric,logical-method
#' setNode,GatingHierarchy,character,logical-method
#' @rdname setNode-methods
setMethod("setNode"
    ,signature(x="GatingHierarchy",y="numeric",value="logical")
    ,function(x,y,value,...){
      hidden = !value
      .Call("R_setNodeFlag",x@pointer,getSample(x),as.integer(y-1),hidden)
    })
setMethod("setNode"
    ,signature(x="GatingHierarchy",y="character",value="logical")
    ,function(x,y,value,...){
      setNode(x,.getNodeInd(x,y),value)
    })

#' Get the sample name associated with a GatingHierarchy
#' 
#'   Return  the sample name
#' @param x A \code{GatingHierarchy}  
#' @param isFullPath \code{isFullPath} is a logical value indicating whether the full path of the sample FCS file is returned.Default is FALSE.
#' 
#' @details Returns the name of the sample, or the path to the FCS file.
#' @return  A "character" vector of length 1. Either the sample name or the path to the FCS file.
#' 
#' @examples
#'   \dontrun{
#'     #G is  a GatingHierarhcy
#'     getSample(G)
#'   }
#' @aliases
#' getSample
#' getSample-method
#' getSample,GatingHierarchy-method
setMethod("getSample","GatingHierarchy",function(x,isFullPath=FALSE){
      thisSample <- sampleNames(x)
      
      if(isFullPath)
        thisSample <- file.path(x@FCSPath,thisSample)
      thisSample
      
    })
