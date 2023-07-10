#' @include AllClasses.R
#' @include getStats.R
NULL

#' modify graph::fromGXL by using customized handler
#' (deprecated by using Rgraphviz::agread)
#' @noRd 
.fromGXL <- function (con)
{
  contents <- paste(readLines(con), collapse = "")
  xmlEventParse <- getExportedValue("XML", "xmlEventParse")
  xmlEventParse(contents, .graph_handler(), asText = TRUE, saxVersion = 2)$asGraphNEL()
}

#' extract compensation object from GatingSet
#' @param gs GatingSet
#' @param sampleName sample name
#' @export 
gs_get_compensation_internal <- function(gs, sampleName) {
  cpp_getCompensation(gs, sampleName)
}

#This legacy routine is currently not used
#Bug here when the GatingSet has a mix of compensated and uncompensated data.. maybe need a isCompensated method..
.isCompensated<-function(x){
  flowCore:::checkClass(x,"GatingHierarchy")
  comp<-gh_get_compensations(x)@spillover

  !(is.null(rownames(comp))&identical(comp%*%comp,comp))
}

#' modify graph:::graph_handler by concatenate the multiple attr string into one
#' to avoid partial node name display. Because XML::xmlEventParse somehow split
#' the node name into arrays when there is numeric character reference (&#nnnn;)
#'
#' (deprecated by using Rgraphviz::agread)
#'
#' @importFrom graph graphNEL addEdge
#' @noRd 
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
#' @importMethodsFrom graph nodeDataDefaults<-  nodeData<- addEdge addNode nodes
#' @importFrom graph graphNEL
#' @importFrom Rgraphviz agread
#' @param x a \code{GatingHierarchy}
#' @noRd 
#return a graphNEL object that only contans the node Name and isBool flags
.getGraph <- function(x){
  DotFile <- tempfile(fileext=".dot")
  cpp_plotGh(x@pointer,sampleNames(x),DotFile)
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

#' @importMethodsFrom graph nodeData removeNode edges inEdges edgeData edgeData<- edgeDataDefaults<-
#' @importFrom graph edgeRenderInfo<- nodeRenderInfo<-
#' @noRd 
.layoutGraph <- function(g,layout="dot"
                        ,fixedsize=FALSE, shape = "ellipse"
                         ,boolean=FALSE,showHidden = FALSE
                         , ...){

  edgeDataDefaults(g, "virtual") <- FALSE
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
              if(as.logical(as.integer(nodes[[i]]))){

                nodeID <- names(nodes[i])
                parentID <- inEdges(nodeID, g)[[1]]
                childrenIDs <- edges(g)[[nodeID]]
                #add edges between its parent and children
                #if it is non-leaf node
                if(length(childrenIDs) > 0){
                  for(childID in childrenIDs)
                      g <- addEdge(parentID, childID, g)
                      edgeData(g, parentID, childID, "virtual") <- TRUE
                }

                g <- removeNode(nodeID, g)
              }

            }
      }


  nAttrs <- list()

  nAttrs$label <- unlist(nodeData(g,attr="label"))
  nAttrs$fillcolor <- sapply(nodeData(g,attr="hidden")
                                ,function(thisHidden)
                              {
                                  ifelse(as.logical(as.integer(thisHidden)),"white","gray")
                                })
  nAttrs$lty <- sapply(nodeData(g,attr="hidden")
                          ,function(thisHidden)
                          {
                            ifelse(as.logical(as.integer(thisHidden)),"dotted","solid")
                          })
  nAttrs$col <- sapply(nodeData(g,attr="isBool")
      ,function(thisBool)
      {
        ifelse(as.logical(as.integer(thisBool)),"blue","black")
      })
  nAttrs$textCol <- sapply(nodeData(g,attr="isBool")
      ,function(thisBool)
      {
        ifelse(as.logical(as.integer(thisBool)),"blue","black")
      })
  
  #pass plot parameters to node attributes (some of parameters won't work via passing to layoutGraph directly)
  nAttrs[["fixedsize"]] <- sapply(nodes, function(i)fixedsize)
  nAttrs[["shape"]] <- sapply(nodes, function(i)shape)
  params <- list(...)
  for(pname in names(params))
    nAttrs[[pname]] <- sapply(nodes, function(i)params[[pname]])
  nodeRenderInfo(g) <- nAttrs

  eData <- edgeData(g, attr = "virtual")
  e.colnames <- names(eData)
  e.colnames <- gsub("\\|", "~", e.colnames)
  names(eData) <- e.colnames
  eStyles <- sapply(eData,function(i)ifelse(i,"dotted","solid"))
#  browser()
#  eColors <- sapply(eData,function(i)ifelse(i,"red","blue"))

  eAttrs <- list(lty = eStyles
#                , color = eColors
                )
#  browser()
  edgeRenderInfo(g) <- eAttrs
  #nodeAttrs and edgeAttrs arguments don't fully work as expected
  #(e.g. lty won't get passed into render info)
  #so we have to also use the renderInfo slot directly for set some parameters
  Rgraphviz::layoutGraph(g,layoutType=layout
                              ,nodeAttrs = nAttrs
                              , edgeAttrs = eAttrs
                            ,attrs=list(graph=list(rankdir="LR",page=c(8.5,11))
                              #           ,node=list(fixedsize=fixedsize
                              # #              ,fillcolor="gray"
                              #               # ,fontsize = fontsize
                              #               ,shape = shape
                              #           )
                            )
                        )
}

#' plot a gating tree
#'
#' Plot a tree/graph representing the GatingHierarchy
#'
#'
#' @param x \code{GatingHierarchy} or \code{GatingSet}. If \code{GatingSet}, the first sample will be used to extract gating tree.
#' @param y \code{missing} or \code{character} specifies.
#' @param ... other arguments:
#' \itemize{
#' \item{boolean}: \code{TRUE|FALSE} logical specifying whether to plot boolean gate nodes. Defaults to FALSE.
#' \item{showHidden}: \code{TRUE|FALSE} logical whether to show hidden nodes
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
#'
#'
#' @examples
#' \dontrun{
#'  #gs is a GatingSet
#'  plot(gs) # the same as plot(gs[[1]])
#'  #plot a substree rooted from 'CD4'
#'  plot(gs, "CD4")
#'
#' }
#'
#' @name plot-methods
#' @aliases plot plot,GatingSet,missing-method
#' plot,GatingSet,character-method
#' @usage plot(x,y, ...)
#' @export
#' @importFrom stats4 plot
#' @importMethodsFrom Rgraphviz renderGraph
setMethod("plot",c("GatingSet","missing"),function(x,y,...){

#           browser()
      g <- .getGraph(x[[1]]) #get tree from the first sample
      lay <- .layoutGraph(g,...)
      renderGraph(lay)
      

    })

.getAllDescendants <- function(gh,startNode,nodelist){

  children_nodes <- gs_pop_get_children(gh,startNode)
  if(length(children_nodes)>0){
    for(this_parent in children_nodes){
      nodelist$v <- c(nodelist$v, .getNodeInd(gh, this_parent))
      .getAllDescendants (gh,this_parent,nodelist)
    }
  }

}

#' @export
#' @importMethodsFrom graph subGraph
setMethod("plot",c("GatingSet","character"),function(x,y,...){
      node <- y
      y <- .getNodeInd(x[[1]],y)

      # get graphNEL object
      gh <- x[[1]]
      g <- .getGraph(gh)


      if(length(y)==1){#use it as the root
        nodelist <- new.env(parent=emptyenv())
        nodelist$v <-integer()
        .getAllDescendants (gh,node,nodelist)



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
      lay <- .layoutGraph(g,...)
      renderGraph(lay)

    })

setMethod("show","GatingHierarchy",function(object){
			cat("Sample: ",sampleNames(object),"\n");
			cat("GatingHierarchy with ",length(gs_get_pop_paths(object))," gates\n");
			cat("\n")
		})


#' Retrieve a specific keyword for a specific sample in a \code{GatingHierarchy} or or set of samples in a \code{GatingSet} or \code{GatingSetList}
#'
#' Retrieve a specific keyword for a specific sample in a \code{GatingHierarchy} or or set of samples in a \code{GatingSet} or \code{GatingSetList}
#' 
#' @name keyword
#' @aliases keyword,GatingHierarchy,character-method keyword,GatingHierarchy,missing-method
#' keyword,GatingSet,character-method keyword,GatingSet,missing-method keyword,GatingSetList,character-method
#' keyword,GatingSetList,missing-method
#' 
#' @details See \code{keyword} in Package `flowCore'
#' 
#' @param object \code{GatingHierarchy} or \code{GatingSet} or \code{GatingSetList}
#' @param keyword \code{character} specifying keyword name. When \code{missing}, extract all keywords.
#' @param ... other arguments passed to \code{\link[flowCore]{keyword-methods}}
#' @seealso \code{\link[flowCore]{keyword-methods}}
#'
#' @examples
#' \dontrun{
#'     # get all the keywords from all samples
#'     keyword(G)
#'     # get all the keywords from one sample
#'     keyword(G[[1]])
#'     # filter the instrument setting
#'     keyword(G[[1]], compact = TRUE)
#'     # get single keyword from all samples
#'     keyword(G, "FILENAME")
#'     # get single keyword from one sample
#'     keyword(G[[1]], "FILENAME")
#' }
#' @export
setMethod("keyword",c("GatingHierarchy","character"),function(object,keyword){

			keyword(object)[[keyword]]
		})
#' @name keyword
#' @export
setMethod("keyword",c("GatingHierarchy","missing"),function(object,keyword = "missing", ...){
      fr <- gh_pop_get_data(object, use.exprs = FALSE)
      flowCore::keyword(fr, ...)
    })

#' @rdname keyword-mutators
#' @export
gh_keyword_insert <- function(gh, keys, values){
  cf <- gh_pop_get_data(gh)
  if(missing(values))
    cf_keyword_insert(cf, keys)
  else
    cf_keyword_insert(cf, keys, values)
}

#' @rdname keyword-mutators
#' @export
gh_keyword_delete <- function(gh, keys){
  cf <- gh_pop_get_data(gh)
  cf_keyword_delete(cf, keys)
}

#' @rdname keyword-mutators
#' @export
gh_keyword_rename <- function(gh, old_keys, new_keys){
  cf <- gh_pop_get_data(gh)
  if(missing(new_keys))
    cf_keyword_rename(cf, old_keys)
  else
    cf_keyword_rename(cf, old_keys, new_keys)
}

#' @rdname keyword-mutators
#' @export
gh_keyword_set <- function(gh, keys, values){
  cf <- gh_pop_get_data(gh)
  if(missing(values))
    cf_keyword_set(cf, keys)
  else
    cf_keyword_set(cf, keys, values)
}

#' @title Deprecated functions in package \pkg{flowWorkspace}.
#' @templateVar old getNodes
#' @templateVar new gs_get_pop_paths
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getNodes",function(x,...)standardGeneric("getNodes"))

#' @export
setMethod("getNodes","GatingSet",function(x,y=NULL,order="regular", path = "full", showHidden = FALSE, ...){
  .Deprecated("gs_get_pop_paths")
  gs_get_pop_paths(x, y, order, path, showHidden, ...)
})

#'  Get the names of all nodes from a gating hierarchy.
#'
#'  \code{gs_get_pop_paths} returns a character vector of names of the nodes (populations) in the \code{GatingSet}.
#'  
#' @name gs_get_pop_paths
#' @aliases gh_get_pop_paths getNodes getNodes,GatingSet-method
#' @param x A \code{GatingSet} Assuming the gating hierarchy are identical within the \code{GatingSet}, the Gating tree of the first sample is used to query the node information.
#' @param y A \code{character} not used.
#' @param order \code{order=c("regular","tsort","bfs")} returns the nodes in regular, topological or breadth-first sort order.
#'     "regular" is default.
#' @param path A \code{character} or \code{numeric} scalar. when \code{numeric}, it specifies the fixed length of gating path (length 1 displays terminal name).
#'              When \code{character}, it can be either 'full' (full path, which is default) or 'auto' (display the shortest unique gating path from the bottom of gating tree).
#' @param showHidden \code{logical} whether to include the hidden nodes
#' @param ... Additional arguments.
#'
#' @details
#' integer indices of nodes are based on regular order,so whenver need to map from character node name to integer node ID,make sure
#' to use default order which is regular.
#' @return
#' gs_get_pop_paths returns a \code{character} vector of node/population names, ordered appropriately.
#'
#' @examples
#'   \dontrun{
#'     # G is a gating hierarchy
#'     gs_get_pop_paths(G, path = 1)#return node names (without prefix)
#'     gs_get_pop_paths(G, path = "full")#return the full path
#'     gs_get_pop_paths(G, path = 2)#return the path as length of two
#'     gs_get_pop_paths(G, path = "auto")#automatically determine the length of path
#'     gs_pop_set_name(G, "L", "lymph")
#'   }
#' @importFrom BiocGenerics duplicated
#' @export
gs_get_pop_paths <- function(x,y=NULL,order="regular", path = "full", showHidden = FALSE, ...){

            order <- match.arg(order,c("regular","tsort","bfs"))
            orderInd <- match(order,c("regular","tsort","bfs"))
            orderInd <- orderInd - 1

            if(is.numeric(path)){
              nodeNames <- cpp_getNodes(x@pointer,sampleNames(x)[1],as.integer(orderInd),TRUE,showHidden)

              if(path == 1){
                nodeNames <- basename(nodeNames)
              }else{

                #truncate the path
                nodeNames <- sapply(nodeNames, function(nodeName){

                                      if(nodeName == "root")
                                        nodeName
                                      else{
                                        nodes <- strsplit(nodeName, split = "/", fixed=TRUE)[[1]][-1]
                                        nNodes <- min(length(nodes), path)
                                        nodes <- rev(rev(nodes)[1:nNodes])

                                        paste(nodes, collapse = "/")
                                      }

                                    }, USE.NAMES = FALSE)
              }

            }else if(path == "auto"){
                nodeNames <- cpp_getNodes(x@pointer,sampleNames(x)[1],as.integer(orderInd),FALSE,showHidden)

            }else if(path == "full"){
              nodeNames <- cpp_getNodes(x@pointer,sampleNames(x)[1],as.integer(orderInd),TRUE,showHidden)
            }else
			  stop("Invalid 'path' argument. The valid input is 'full' or 'auto' or a numeric value.")


			if(!is.null(y))
			  stop("'y' argument is not supported!")


			nodeNames
		}

#' @rdname gs_get_pop_paths
#' @export
gh_get_pop_paths <- gs_get_pop_paths
			
#' convert the partial gating path to the full path
#' @param gh GatingHierarchy object
#' @param path the partial gating path
#' @return the full gating path
#' @export 
gh_pop_get_full_path <- function(gh, path){
	getNodePath(gh@pointer, sampleNames(gh)[1], .getNodeInd(gh, path) - 1)
}
#' @templateVar old getParent
#' @templateVar new gs_pop_get_parent
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getParent",function(obj,y,...)standardGeneric("getParent"))

#' @export
setMethod("getParent",signature(obj="GatingSet",y="character"),function(obj,y, ...){
  .Deprecated("gs_pop_get_parent")
  gs_pop_get_parent(obj, y, ...)
})

#' @rdname gs_pop_get_children
#' @export
gs_pop_get_parent <- function(obj,y, ...){
            pind <- cpp_getParent(obj@pointer,sampleNames(obj)[1], y)
            pind <- pind +1
			gs_get_pop_paths(obj, showHidden = TRUE, ...)[pind]
		}
#' @rdname gs_pop_get_children
#' @export
gh_pop_get_parent <- gs_pop_get_parent
			
#' @templateVar old getChildren
#' @templateVar new gs_pop_get_children
#' @template template-depr_pkg
		NULL
#' @export
setGeneric("getChildren",function(obj,y,...)standardGeneric("getChildren"))

#' @export
setMethod("getChildren",signature(obj="GatingSet",y="character"),function(obj,y, showHidden = TRUE, ...){
  .Deprecated("gs_pop_get_children")
  gs_pop_get_children(obj, y, showHidden, ...)
})
		
#' Return the name of the parent population or a list of child populations of the current population in the GatingHierarchy
#'
#' Returns the name of the parent population or a character/numeric vector of all the children of the current population in the given \code{GatingHierarchy}
#' 
#' @name gs_pop_get_children
#' @aliases gs_pop_get_children gh_pop_get_children gs_pop_get_parent gh_pop_get_parent 
#' getChildren getChildren,GatingSet,character-method
#' getParent getParent,GatingSet,character-method
#' @param obj A \code{GatingHierarchy}
#' @param y a \code{character/numeric} the name or full(/partial) gating path  or node indices of the node / population.
#' @param showHidden \code{logical} whether to include the hidden children nodes.
#' @param ... other arguments passed to \link{gs_get_pop_paths} methods
#' @return
#'   \code{gs_pop_get_parent} returns a \code{character} vector, the name of the parent population.
#'   \code{gs_pop_get_children} returns a \code{character} or \code{numeric} vector of the node names or node indices of the child nodes of the current node. An empty vector if the node has no children.
#' @seealso \code{\link{gs_get_pop_paths}}
#'
#' @examples
#' \dontrun{
#'     # G is a GatingHierarchy
#'     # return the name of the parent of the fifth node in the hierarchy.
#'     gs_pop_get_parent(G,gs_get_pop_paths(G[[1]])[5])
#'     n<-gs_get_pop_paths(G,tsort=T)[4]
#'     #Get the names of the child nodes of the 4th node in this gating hierarchy.
#'     gs_pop_get_children(G,n)
#'     #Get the ids of the child nodes
#'     gs_pop_get_children(G,4)
#' }
#' @export
gs_pop_get_children <- function(obj,y, showHidden = TRUE, ...){
      cind <- cpp_getChildren(obj@pointer,sampleNames(obj)[1], y, showHidden)
      cind <- cind + 1
			gs_get_pop_paths(obj, showHidden = TRUE, ...)[cind]
}

#' @rdname gs_pop_get_children
#' @export
gh_pop_get_children <- gs_pop_get_children
	
#' @templateVar old getGate
#' @templateVar new gs(/gh)_get_gate
#' @template template-depr_pkg
NULL

#' @export
setGeneric("getGate",function(obj,y,...)standardGeneric("getGate"))

#' @export
setMethod("getGate",signature(obj="GatingHierarchy",y="character"),function(obj,y){
      .Deprecated("gh_pop_get_gate")
			gh_pop_get_gate(obj, y)
		})
#'  Return the flowCore gate definition associated with a node in a GatingHierarchy/GatingSet.
#'
#'  Return the flowCore gate definition object associated with a node in a \code{GatingHierarchy} or \code{GatingSet} object.
#' @name gs_pop_get_gate
#' @param obj A \code{GatingHierrarchy} or \code{GatingSet}
#' @param y A \code{character} the name or full(/partial) gating path of the node of interest.
#'
#' @return  A gate object from \code{flowCore}. Usually a \code{polygonGate}, but may be a \code{rectangleGate}. Boolean gates are represented by a \code{"BooleanGate"} S3 class. This is a list boolean gate definition that references populations in the GatingHierarchy and how they are to be combined logically. If \code{obj} is a \code{GatingSet}, assuming the trees associated with each \code{GatingHierarchy} are identical, then this method will return a list of gates, one for each sample in the \code{GatingSet} corresponding to the same population indexed by \code{y}.
#'
#' @seealso \code{\link{gh_pop_get_data}} \code{\link{gs_get_pop_paths}}
#' @examples
#'   \dontrun{	#gh is a GatingHierarchy
#'     gh_pop_get_gate(gh, "CD3") #return the gate for the fifth node in the tree, but fetch it by name.
#'     #G is a GatingSet
#'     gs_pop_get_gate(G, "CD3") #return a list of gates for the fifth node in each tree
#'   }
#' @aliases gh_pop_get_gate getGate getGate,GatingHierarchy,character-method
#' getGate,GatingSet,character-method getGate,GatingSetList,character-method
#' @export
gh_pop_get_gate <- function(obj,y){

				g<-cpp_getGate(obj@pointer,sampleNames(obj), y)
				filterId <- g$filterId
				if(g$type==1)
				{


					mat<-matrix(c(g$x,g$y),ncol=2,dimnames=list(NULL,g$parameters))
					if(nrow(mat)==2)#convert to rectangleGate
					{
						rg <- rectangleGate(.gate=mat,filterId=filterId)
						if(!is.null(g[["quadintersection"]]))#attach quadgate
						{
							quadrants = g[["quadrants"]];
							names(quadrants) <- g[["quadpops"]];
							attr(rg, "quadrants") <- quadrants
							attr(rg, "quadintersection") <- g[["quadintersection"]]
							
						}
						rg
					}else{
                      #restore gate coordinates due to the double overflow during pb archiving
                      mat[mat == Inf] <- .Machine$double.xmax
                      mat[mat == -Inf] <- -.Machine$double.xmax
                      polygonGate(.gate=mat,filterId=filterId)
                    }

				}else if(g$type==2)
					rectangleGate(.gate=matrix(g$range,dimnames=list(NULL,g$parameters)),filterId=filterId)
				else if(g$type ==3)
				{

					refPaths<-unlist(lapply(g$ref,function(curPath){
										paste(curPath,collapse="/")

									})
								)

          #get rid of the first op
          g$v2[1] <- ""
          boolExpr <- paste(g$v2, g$v,refPaths,sep="")
          boolExpr <- paste(boolExpr,collapse="")
          if(nchar(boolExpr) == 0)
            stop("Empty boolean expression from :", filterId)
          boolExpr <- as.symbol(boolExpr)

          g <- eval(substitute(booleanFilter(xx, filterId=filterId),list(xx=boolExpr)))
					g
				}else if (g$type == 4)
			  {
              cov.mat <- g$cov
              dimnames(cov.mat) <- list(g$parameters, g$parameters)
              ellipsoidGate(.gate = cov.mat, mean = g$mu, distance = g$dist, filterId = filterId)
        }else if (g$type %in% c(6,8)){#logicalGate
          booleanFilter(filterId=filterId)#return dummy boolean filter
        }else if (g$type %in% c(11)){ #multi range gate
          multiRangeGate(filterId=filterId,ranges = g$ranges)
        }else
					stop("not supported gate type",g$type)


		}
        
#' Retrieve the cluster labels from the cluster nodes
#' 
#' Clustering results are stored as individual gated nodes. This helper function
#' collect all the gating indices from the same clustering run (identified by 'parent'
#' node and 'cluster_method_name" and merge them as a single factor.
#' 
#' @param gh GatingHierarchy
#' @param parent the parent population/node name or path
#' @param cluster_method_name the name of the clustering method
#' @export
gh_get_cluster_labels <- function(gh, parent, cluster_method_name){
  nodes <- gs_pop_get_children(gh, parent)
  res <- rep(NA, gh_pop_get_stats(gh, "root")[[2]])
  empty_pops <- NULL
  isFound <- FALSE
  for(node in nodes)
  {
      g <-cpp_getGate(gh@pointer,sampleNames(gh), node)
      if(g[["type"]] == 8)
      {
        if(g[["cluster_method_name"]] == cluster_method_name)
        {
          isFound <- TRUE
          ind <- which(gh_pop_get_indices(gh, node))
          if(all(is.na(res[ind])))
          {
            pop <- extract_cluster_pop_name_from_node(node, cluster_method_name)
            if(length(ind) == 0)
              empty_pops <- c(empty_pops, pop)
            else
              res[ind] <- pop
          }
          else
            stop(node, " has overlapped memberships with other cluster nodes!")
          
        }
          
      }
    }
  if(!isFound)
     stop("No clustering results for ", cluster_method_name)
  else
  {
    res <- as.factor(res)
    
    #add empty pop by adding levels
    levels(res) <- c(levels(res) , empty_pops)
    res
  }
    
  
}

#' Compute logicle transformation from the flowData associated with a GatingHierarchy
#' 
#' See details in \link[flowCore]{estimateLogicle}
#' 
#' @name estimateLogicle
#' @aliases estimateLogicle,GatingHierarchy-method estimateLogicle,GatingSet-method
#' @param x a GatingHierarchy
#' @param channels channels or markers for which the logicle transformation is to be estimated.
#' @param ... other arguments
#' @return transformerList object
#'  
#' @examples
#' \dontrun{
#'  # gs is a GatingSet
#'  trans.list <- estimateLogicle(gs[[1]], c("CD3", "CD4", "CD8")) 
#'  # trans.list is a transformerList that can be directly applied to GatinigSet
#'  gs <- transform(gs, trans.list)
#' }
#' @export 
estimateLogicle.GatingHierarchy <- function(x, channels, ...){
  fr <- gh_pop_get_data(x)
  trans <- flowCore:::.estimateLogicle(fr, channels, ...)
  
  trans <- lapply(trans, function(t){
    inv <- inverseLogicleTransform(trans = t)
    flow_trans("logicle", t@.Data, inv@.Data)
  })
  channels <- names(trans)
  transformerList(channels, trans)
}

#' Extract the population name from the node path
#' It strips the parent path and cluster method name.
#' @param node population node path
#' @param cluster_method_name the name of the clustering method
#' @export 
#' @examples 
#' extract_cluster_pop_name_from_node("cd3/flowClust_pop1", "flowClust")
#' #returns "pop1"
extract_cluster_pop_name_from_node <- function(node, cluster_method_name)
{
  pop <- basename(node)
  sub(paste0(cluster_method_name, "_"), "", pop) #strip cluster name prefix
  
}

#' check if a node is clustering node
#' @param gh GatingHierarchy
#' @param node the population/node name or path
#' @return the name of the clustering method. If it is not cluster node, returns NULL
#' @export
gh_pop_get_cluster_name <- function(gh, node){
  g <-cpp_getGate(gh@pointer,sampleNames(gh), node)
  if(g[["type"]] == 8)
    g[["cluster_method_name"]]
  else
    NULL
}

#' @export
#' @noRd 
.getNodeInd <- function(obj,y, ...){

    ind <- cpp_getNodeID(obj@pointer,sampleNames(obj)[1], y)

    ind + 1 # convert to R index

}


#' @templateVar old getIndices
#' @templateVar new gh_pop_get_indices
#' @template template-depr_pkg
NULL

#' @export
setMethod("getIndices",signature(obj="GatingHierarchy",y="character"),function(obj,y){
			.Deprecated("gh_pop_get_indices")
			gh_pop_get_indices(obj, y)
 
		})

#' Get the membership indices for each event with respect to a particular gate in a GatingHierarchy
#'
#' Returns a logical vector that describes whether each event in a sample is included or excluded by this gate.
#'  
#' @name gh_pop_get_indices
#' @aliases getIndices getIndices,GatingHierarchy,character-method
#' @param obj A \code{GatingHierarchy} representing a sample.
#' @param y A \code{character} giving the name or full(/partial) gating path of the population / node of interest.
#'
#' @details  Returns a logical vector that describes whether each event in the data file is included in the given gate of this \code{GatingHierarchy}. The indices are for all events in the file, and do not reflect the population counts relative to the parent but relative to the root. To get population frequencies relative to the parent one cross-tabulate the  indices of \code{y} with the indices of its parent.
#'
#' @return  A logical vector of length equal to the number of events in the FCS file that determines whether each event is or is not included in the current gate.
#'
#' @note Generally you should not need to use \code{gh_pop_get_indices} but the more convenient methods \code{gh_pop_get_proportion} and \code{gh_pop_compare_stats} which return population frequencies relative to the parent node.
#' The indices returned reference all events in the file and are not directly suitable for computing population statistics, unless subsets are taken with respect to the parent populations.
#'
#' @seealso \code{\link{gh_pop_compare_stats}}
#'
#' @examples
#'   \dontrun{
#'     #G is a gating hierarchy
#'     #Return the indices for population 5 (topological sort)
#'     gh_pop_get_indices(G,gs_get_pop_paths(G,tsort=TRUE)[5]);
#' }
#'
#' @aliases gh_pop_get_indices
#' @export
gh_pop_get_indices <- function(obj,y){
	cpp_getIndices(obj@pointer,sampleNames(obj), y)
}
#' @templateVar old isGated
#' @templateVar new gh_pop_is_gated
#' @template template-depr_pkg
NULL

#' @export 
isGated <- function(obj,y){
			.Deprecated("gh_pop_is_gated")
			gh_pop_is_gated(obj, y)
			
}
#' The flags of gate nodes
#' 
#' gh_pop_is_gated checks if a node is already gated.
#' gh_pop_is_negated checks if a node is negated.
#' gh_pop_is_hidden checks if a node is hidden.
#' 
#' @name nodeflags
#' @aliases isGated isNegated isHidden
#' 
#' @param obj GatingHierarchy
#' @param y node/gating path
#' @export 
gh_pop_is_gated <- function(obj,y){
      cpp_getGateFlag(obj@pointer,sampleNames(obj), y)

    }

#' @templateVar old isNegated
#' @templateVar new gh_pop_is_negated
#' @template template-depr_pkg
NULL

#' @export 
isNegated <- function(obj,y){
			.Deprecated("gh_pop_is_negated")
			gh_pop_is_negated(obj, y)
			
		}
#' @rdname nodeflags
#' @export 
gh_pop_is_negated <- function(obj,y){
      cpp_getNegateFlag(obj@pointer,sampleNames(obj), y)

    }

#' @templateVar old isHidden
#' @templateVar new gh_pop_is_hidden
#' @template template-depr_pkg
NULL

#' @export 
isHidden <- function(obj,y){
			.Deprecated("gh_pop_is_hidden")
			gh_pop_is_hidden(obj, y)
			
		}
#' @rdname nodeflags
#' @export 
gh_pop_is_hidden  <- function(obj,y){		
      cpp_getHiddenFlag(obj@pointer,sampleNames(obj), y)
    }


#' @templateVar old getData
#' @templateVar new gs(/gh)_get_data
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getData",function(obj,y,...)standardGeneric("getData"))

#' @export
setMethod("getData",signature(obj="GatingHierarchy",y="ANY"),function(obj,y, ...){
  .Deprecated("gh_pop_get_data")
  if(missing(y)){
    gh_pop_get_data(obj, ...)
  }else{
    gh_pop_get_data(obj, y, ...)
  }
})


#' get gated flow data from a GatingHierarchy/GatingSet/GatingSetList
#'
#' get gated flow data from a GatingHierarchy/GatingSet/GatingSetList
#' 
#' @name gh_pop_get_data
#' @aliases gs_pop_get_data getData getData,GatingHierarchy-method
#' getData,GatingSet-method getData,GatingSetList-method
#' @details
#' Returns a flowFrame/flowSet containing the events in the gate defined at node \code{y}.
#' Subset membership can be obtained using \code{gh_pop_get_indices}.
#' Population statistics can be obtained using \code{getPop} and \code{gh_pop_compare_stats}.
#' When calling \code{gh_pop_get_data} on a GatingSet,the trees representing the GatingHierarchy for each sample in the GaingSet are presumed to have the same structure.
#' To update the data, use \code{gs_cyto_data} method.
#' @param obj A \code{GatingHierarchy}, \code{GatingSet} or \code{GatingSetList} object.
#' @param  y \code{character}  the node name or full(/partial) gating path.
#'                             	If not specified, will return the complete flowFrame/flowSet at the root node.
#' @param inverse.transform logical flag indicating whether to inverse transform the data
#' @param ... arguments passed to ncdfFlow::[[
#'
#' @return
#' A \code{flowFrame} object if \code{obj} is a GatingHierarchy.
#' A \code{flowSet} or \code{ncdfFlowSet} if a \code{GatingSet}.
#' A \code{ncdfFlowList} if a \code{GatingSetList}.
#' @seealso
#'   \code{\link{gs_cyto_data}} \code{\link{gh_pop_get_indices}} \code{\link{gh_pop_compare_stats}}
#'
#' @examples
#'   \dontrun{
#'     #G is a GatingSet
#'     geData(G,3) #get a flowSet constructed from the third node / population in the tree.
#'     geData(G,"cd4")
#'
#'     #gh is a GatingHierarchy
#'     gh_pop_get_data(gh)
#' }
#' @export
gh_pop_get_data <- function(obj, y = "root", inverse.transform = FALSE, ...){
      
	fs <- gs_pop_get_data(obj, y, inverse.transform)
	fs[[1, ...]]
}

#' @export 
#' @rdname nodeflags
gh_pop_is_bool_gate<-function(obj,y){
	return (class(gh_pop_get_gate(obj,y))=="booleanFilter")
}

#' Determine tick mark locations and labels for a given channel axis
#'
#' @param gh \code{GatingHiearchy}
#' @param channel \code{character} channel name
#'
#' @return when there is transformation function associated with the given channel, it returns a list of that contains positions and labels to draw on the axis
#'  other wise returns NULL
#' @examples
#' \dontrun{
#'  prettyAxis(gh, "<B710-A>")
#' }
#' @export
prettyAxis <- function(gh, channel){



        # res <- getAxisLabels(gh)[[channel]] #this call is to be deprecated once we figure out how to preserve trans when cloning GatingSet
        # if(is.null(res)){
          #try to grab trans and do inverse trans for axis label on the fly
            trans <- gh_get_transformations(gh, channel, only.function = FALSE)
            if(is.null(trans)){
              res <- NULL
            }else{
              inv.func <- trans[["inverse"]]
              trans.func <- trans[["transform"]]
              brk.func <- trans[["breaks"]]

              fr <- gh_pop_get_data(gh, use.exprs = FALSE)
              r <- as.vector(range(fr)[,channel])#range
              raw <- inv.func(r)
              brks <- brk.func(raw)
              pos <- signif(trans.func(brks))
              #format it
              label <- trans[["format"]](brks)


              res <- list(label = label, at = pos)
            }
# 
#         }else{
#           #use the stored axis label if exists
#           res$label <- pretty10exp(as.numeric(res$label),drop.1=TRUE)
#         }

        res
}

##to be deprecated
#getAxisLabels <- function(obj,...){
#  obj@axis[[sampleNames(obj)]]
#}

#' @templateVar old getTransformations
#' @templateVar new gh_get_transformations
#' @template template-depr_pkg
NULL

#' @export 
getTransformations <- function(x, ...)UseMethod("getTransformations")

#' @export 
getTransformations.GatingHierarchy <- function(...){
	.Deprecated("gh_get_transformations")
  gh_get_transformations(...)
}

#' Return a list of transformations or a transformation in a GatingHierarchy
#'
#' Return a list of all the transformations or a transformation in a GatingHierarchy
#' 
#' @name gh_get_transformations
#' @aliases getTransformations getTransformations,GatingHierarchy-method
#' @param x A \code{GatingHierarchy} object
#' @param inverse \code{logical} whether to return the inverse transformation function. Valid when only.funtion is TRUE
#' @param only.function \code{logical} whether to return the function or the entire transformer object(see \code{scales} package) that contains transform and inverse and breaks function.
#' @param channel \code{character} channel name
#' @param ... other arguments
#'         equal.spaced \code{logical} passed to the breaks functio to determine whether to break at 10^n or equally spaced intervals
#' @details
#' Returns a list of the transformations or a transformation in the flowJo workspace.
#' The list is of length \code{L}, where \code{L} is the number of distinct transformations applied to samples
#' in the \code{flowjo_workspace}. Each element of \code{L} is itself a \code{list} of length \code{M},
#' where \code{M} is the number of parameters that were transformed for a sample or group of samples
#' in a \code{flowjo_workspace}. For example, if a sample has 10 parameters, and 5 are transformed during analysis,
#' using two different sets of transformations, then L will be of length 2, and each element of L will be of length 5.
#' The elements of \code{L} represent channel- or parameter-specific transformation functions that map from raw intensity values
#' to channel-space used by flowJo.
#' @return
#' lists of functions(or transform objects when only.function is FALSE), with each element of the list representing a transformation applied to a specific
#' channel/parameter of a sample.
#' @examples
#' \dontrun{
#' 	#Assume gh is a GatingHierarchy
#' 	gh_get_transformations(gh); # return a list transformation functions
#'  gh_get_transformations(gh, inverse = TRUE); # return a list inverse transformation functions
#'  gh_get_transformations(gh, channel = "FL1-H") # only return the transfromation associated with given channel
#'  gh_get_transformations(gh, channel = "FL1-H", only.function = FALSE) # return the entire transform object
#' }
#' @export 
gh_get_transformations  <- function(x, channel = NULL, inverse = FALSE, only.function = TRUE, ...){
      trans.objects <- x@transformation
      if(length(trans.objects) == 0){
        trans.objects <- .getTransformations(x@pointer,sampleNames(x), ...)
      }else
        trans.objects <- trans.objects[[1]]
      trans_names <- names(trans.objects)
      
      #this option is for backward compatibility
      if(only.function){
        trans.objects <- sapply(trans.objects, function(obj){

                    if(inverse)
                      obj[["inverse"]]
                    else
                      obj[["transform"]]
            })
      }


      #try to match to given channel
      if(!is.null(channel)){
        if(channel=="all")
        {
          # skip filtering only when specifically asked for (by setting channel  to "all", reserved for interal usage
          # when data was not available during the parsing xml stage)
          trans.objects
        }else
        {
          #do strict match first
          j <- trans_names %in% channel
          
          if(sum(j) > 1){
            stop("multiple tranformation functions matched to: ", channel)
          }else if(sum(j) == 0){
            return(NULL)
          }else{
            trans.objects[[which(j)]]
          }
        }
      }else
      {
        
         
          #filter out the non-prefixed trans objs by default
          chnls <- colnames(x)
          
         trans.objects[trans_names %in% chnls]  
       
      }
        
}


.convertTrans <- function(trans, inverse = FALSE){
  #remedy for dealing cpp1 bug see https://github.com/r-lib/cpp11/issues/206
  chnls <- names(trans)
  names(trans) <- chnls[chnls!=""]
  transList <- lapply(trans,function(curTrans){
#						browser()
        if(curTrans$type=="log")
        {
          f <- flowjo_log_trans(decade = curTrans$decade
              , offset = curTrans$offset
	  		  , scale = curTrans$scale
          )
          
          if(inverse){
            f <- f[["inverse"]]
            attr(f,"type")<-"log.inverse"
          }else{
            f <- f[["transform"]]
            attr(f,"type")<- "log"
			}


        }
        else if(curTrans$type=="logtGml2")
        {
          f <- logtGml2_trans(t = curTrans$t, m = curTrans$m)
          
          if(inverse){
            f <- f[["inverse"]]
            attr(f,"type")<-"logtGml2.inverse"
          }else{
            f <- f[["transform"]]
            attr(f,"type")<- "logtGml2"
          }
        }
        else if(curTrans$type=="lin")
        {
          f<-function(x){x*64}
          attr(f,"type")<-"gateOnly"


        }else if(curTrans$type %in% c("caltbl" , "biexp")){
          f <- .flowJoTrans(curTrans)
          # attr(f,"type")<-"biexp"
        }else if(curTrans$type=="fasinh"){
          if(inverse){
            f <- flowjo_fsinh(t = curTrans$T, m = curTrans$M, a = curTrans$A, length = curTrans$length)
            attr(f,"type")<-"fsinh"
          }else{
            f <- flowjo_fasinh(t = curTrans$T, m = curTrans$M, a = curTrans$A, length = curTrans$length)
            attr(f,"type")<-"fasinh"
          }


        }else if(curTrans$type=="logicle"){
          f <- logicleTransform(t = curTrans$T, m = curTrans$M, a = curTrans$A, w = curTrans$W)
          if(inverse)
            f <- inverseLogicleTransform(f)
          f <- f@.Data
          attr(f,"type")<-"logicle"
        }else if(curTrans$type=="scale"){
        	if(inverse){
        		f<-function(x){x/curTrans$scale_factor}
        		attr(f,"type")<-"scale.inverse"
        	}else{
        		f<-function(x){x*curTrans$scale_factor}
        		attr(f,"type")<-"scale"
        	}
        }

        return (f)
      })


}
#' extract trans from c++
#' @noRd 
.getTransformations <- function(pointer,sampleName, equal.space = FALSE, ...){
    trans.func <- cpp_getTransformations(pointer,sampleName, inverse = FALSE)
    inv.func <- cpp_getTransformations(pointer,sampleName, inverse = TRUE)
    trans.list <- .convertTrans(trans.func)
    inv.list <- .convertTrans(inv.func, inverse = TRUE)
    trans.name <- "flowJo_"
    mapply(trans.list, inv.list, FUN = function(trans, inv){
           trans.type <- attr(trans,"type")
           if(trans.type != "gateOnly"){
             trans.name <- paste0(trans.name, trans.type)
            flow_trans(name = trans.name, trans.fun = trans, inverse.fun = inv, n = 6, equal.space = equal.space)
           }
        }, SIMPLIFY = FALSE)


  }


#' @templateVar old getCompensationMatrices
#' @templateVar new gh_get_compensations
#' @template template-depr_pkg
NULL

#' @export 
 getCompensationMatrices <- function(x)UseMethod("getCompensationMatrices")

#' @export
getCompensationMatrices.GatingHierarchy <- function(x){
  .Deprecated("gh_get_compensations")
  gh_get_compensations(x)
}

#'  Retrieve the compensation matrices from a \code{GatingHierarchy} or \code{GatingSet}
#'
#'  Retrieve the compensation matrices from a \code{GatingHierarchy} or \code{GatingSet}.
#'
#' @name gh_get_compensations
#' @aliases getCompensationMatrices getCompensationMatrices,GatingHierachy-method
#' gs_get_compensations
#' @param x A \code{GatingHierarchy} or \code{GatingSet} object.
#'
#' @details Return all the compensation matrices in a \code{GatingHierarchy} or \code{GatingSet} 
#' @return
#'   A list of \code{matrix} representing the spillover matrix in \code{GatingHierarchy} or \code{GatingSet} 
#' @examples
#'   \dontrun{
#' 	 # Assume gh is a GatingHierarchy and gs is a GatingSet
#'   gh_get_compensations(gh)
#'   gs_get_compensations(gs)
#' }
#' @export
gh_get_compensations <- function(x){
  
      sn <- sampleNames(x)
#      if(is.null(compobj)){
        comp<-cpp_getCompensation(x@pointer, sn)
        cid<-comp$cid
        #			browser()
        if(cid=="")
          cid=-2
        if(cid!="-1" && cid!="-2"){
          marker<-comp$parameters
          detector<-comp$detectors
          compobj<-compensation(matrix(comp$spillOver
                                       ,ncol=length(detector)
                                       ,nrow=length(marker)
                                       ,byrow=TRUE
                                       ,dimnames=list(marker, detector)
          )
          )        
          
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
            compobj<-compensation(spillover(gh_pop_get_data(x))$SPILL)

          }

        }
#      }else{
#        compobj <- compobj[[sn]]
#      }

			compobj

}

.mergeGates<-function(gh,i,bool,merge, projections = list()){
	##filter out boolean gates when bool==FALSE
#	browser()
	if(!bool)
	{

		boolInd<-unlist(lapply(i,gh_pop_is_bool_gate,obj = gh))
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
	  allNodes <- gs_get_pop_paths(gh, showHidden = TRUE, path = "auto")
	  sn <- sampleNames(gh)
		#check if they have same parents and parameters
		keylist <- sapply(plotList,function(y){

					if(!gh_pop_is_bool_gate(gh,y))
					{
						curGate<-gh_pop_get_gate(gh,y)
                        thisParam <- parameters(curGate)
						if(extends(class(curGate),"filter"))
						{
							#use id instead of node name to avoid the special characters (e.g. '!') from interfering the 
						  #parsing of parent info from the contatenated string  later on
						  pid <- cpp_getParent(gh@pointer,sn, y)+1
							
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


		keylistFeq <- table(keylist)
		toMergeKeyList <- names(keylistFeq[keylistFeq>=2])
		# construct the a special list object to replace/del the one that needs to be merged
		for(curKey in toMergeKeyList)
		{

			toMerge <- names(keylist[keylist==curKey])
            #replace the first merged child node with the merge list
            #we don't want to sort the character here since it is locale-dependent
			toReplace <- toMerge[1]
			toRemove <- toMerge[!(toMerge==toReplace)]#remove other children

			toReplaceInd <- match(toReplace,poplist)
			toRemoveInd <- match(toRemove,poplist)
			#								browser()

			curPid <- as.integer(strsplit(curKey, split="|", fixed=TRUE)[[1]][1])#extract pid
			plotList[[toReplaceInd]] <- list(popIds=toMerge,parentId=allNodes[curPid])
			plotList[toRemoveInd] <- NULL
			poplist[toRemoveInd] <- NULL#make sure syn y as well vector since it is used to index plotList
		}

	}
	plotList
}
#copy from sfsmisc package
#modified to handle NA values
pretty10exp <-function (x, drop.1 = FALSE, digits.fuzz = 7)
{
	eT <- floor(log10(abs(x)) + 10^-digits.fuzz)
	mT <- signif(x/10^eT, digits.fuzz)
	ss <- vector("list", length(x))
	for (i in seq(along = x)) ss[[i]] <- if (is.na(x[i]))
                	                  quote(NA)
                    	  else if (x[i] == 0)
                					quote(0)
                				else if (drop.1 && mT[i] == 1)
                					substitute(10^E, list(E = eT[i]))
                				else if (drop.1 && mT[i] == -1)
                					substitute(-10^E, list(E = eT[i]))
                				else substitute(A %*% 10^E, list(A = mT[i], E = eT[i]))
	
	do.call("expression", ss)
}


#' @templateVar old setNode
#' @templateVar new gs(/gh)_set_node_name/gs(/gh)_set_node_visible
#' @template template-depr_pkg
NULL
#' @export
setGeneric("setNode",function(x,y,value,...)standardGeneric("setNode"))

#' @export
setMethod("setNode"
    ,signature(x="GatingHierarchy",y="character",value="character")
    ,function(x,y,value){
      .Deprecated("gh_pop_set_name")
	  gh_pop_set_name(x, y, value)
      })

#' Update the name of one node in a gating hierarchy/GatingSet.
#'
#' \code{gh_pop_set_name/gs_pop_set_name} update the name of one node in a gating hierarchy/GatingSet.
#'
#' @name gs_pop_set_name
#' @param x GatingHierarchy
#' @param y pop name/path
#' @param value A \code{character} the name of the node
#' @examples
#' \dontrun{
#'     # G is a GatingHierarchy
#'     gs_get_pop_paths(G[[1]])#return node names
#'     gh_pop_set_name(G,"L","lymph")
#' }
#' @aliases setNode setNode,GatingHierarchy,character,character-method
#' setNode,GatingHierarchy,character,ANY-method setNode,GatingSet,character,ANY-method
#' @export
gh_pop_set_name <- function(x,y,value){
  setNodeName(x@pointer,sampleNames(x), y,value)
}

#' @export
setMethod("setNode"
    ,signature(x="GatingHierarchy",y="character",value="logical")
    ,function(x,y,value){
      .Deprecated("gh_pop_set_visibility")
      gh_pop_set_visibility(x, y, value)
      })

#' hide/unhide a node
#' 
#' @name gs_pop_set_visibility
#' @aliases gh_pop_set_visibility setNode,GatingHierarchy,character,logical-method
#' @param x \code{GatingHierarchy} object
#' @param y \code{character} node name or path
#' @param value TRUE/FALSE to indicate whether to hide a node
#' @examples
#' \dontrun{
#'      gh_pop_set_visibility(gh, 4, FALSE) # hide a node
#'      gh_pop_set_visibility(gh, 4, TRUE) # unhide a node
#' }
#' @export
gh_pop_set_visibility <- function(x,y,value){
            
            hidden = !value
            setNodeFlag(x@pointer,sampleNames(x), y, hidden)
          }


#' @export
setMethod("pData","GatingHierarchy",function(object){
      pData(gs_cyto_data(object))[sampleNames(object), , drop = FALSE]
    })

#' Get/set the column(channel) or marker names
#'
#' It simply calls the methods for the underlying flow data (flowSet/ncdfFlowSet/ncdfFlowList).
#' 
#' @name markernames
#' @aliases markernames,GatingSet-method markernames,GatingHierarchy-method
#' markernames,cytoset-method
#' @param x,object GatingHierarchy/GatingSet/GatingSetList
#' @param value named character vector for markernames<-, regular character vector for colnames<-.
#' @examples
#' \dontrun{
#'
#' markers.new <- c("CD4", "CD8")
#' chnls <- c("<B710-A>", "<R780-A>")
#' names(markers.new) <- chnls
#' markernames(gs) <- markers.new
#'
#' chnls <- colnames(gs)
#' chnls.new <- chnls
#' chnls.new[c(1,4)] <- c("fsc", "ssc")
#' colnames(gs) <-  chnls.new
#' }
#' @export
setMethod("markernames",
          signature=signature(object="GatingHierarchy"),
          definition=function(object){

            markernames(gh_pop_get_data(object, returnType = "cytoframe", use.exprs = FALSE))

          })


#' @rdname markernames
#' @aliases markernames<-,GatingSet,ANY-method markernames<-,GatingSet-method
#' markernmaes<-,cytoframe-method markernames<-,cytoset-method
#' @export
setReplaceMethod("markernames",
                 signature=signature(object="GatingHierarchy", value="ANY"), function(object, value){

                   sn <- sampleNames(object)
                   cs <- gs_cyto_data(object)[sn]
                   markernames(cs) <- value
	              object
                 })


#' @param do.NULL,prefix not used.
#' @rdname markernames
#' @aliases colnames,GatingSet-method colnames,GatingHierarchy-method
#' colnames,cytoframe-method colnames,cytoset-method
#' @export
setMethod("colnames",
          signature=signature(x="GatingHierarchy"),
          definition=function(x, do.NULL="missing", prefix="missing"){

            colnames(gh_pop_get_data(x, returnType = "cytoframe", use.exprs = FALSE))

          })

#' @rdname markernames
#' @aliases colnames<-,GatingSet,ANY-method colnames<-,GatingSet-method
#' colnames<-,cytoframe-method colnames<-,cytoset-method
#' @export
setReplaceMethod("colnames",
                 signature=signature(x="GatingHierarchy", value="ANY"), function(x, value){
                  stop("Can't change colnames for the individual sample. Please call colnames<- on the whole GatingSet instead!")
#                    sn <- sampleNames(x)
#                    colnames(gs_cyto_data(x)@frames[[sn]]) <- value

                   # x
                 })

#' @rdname cleanup_temp
#' @export
gh_cleanup_temp <- function(x, temp_dir = NULL){
	sn <- sampleNames(x)
	cf <- gs_cyto_data(x)[[sn, returnType = "cytoframe"]]
	cf_cleanup_temp(cf, temp_dir)
}
