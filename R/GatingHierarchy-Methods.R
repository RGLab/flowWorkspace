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

#This legacy routine is currently not used
#Bug here when the GatingSet has a mix of compensated and uncompensated data.. maybe need a isCompensated method..
.isCompensated<-function(x){
  flowCore:::checkClass(x,"GatingHierarchy")
  comp<-getCompensationMatrices(x)@spillover

  !(is.null(rownames(comp))&identical(comp%*%comp,comp))
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
#' @param x a \code{GatingHierarchy}
#return a graphNEL object that only contans the node Name and isBool flags
.getGraph <- function(x){
  DotFile <- tempfile(fileext=".dot")
  .Call("R_plotGh",x@pointer,sampleNames(x),DotFile,FALSE)
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
.layoutGraph <- function(g,layout="dot",width=3,height=2,fontsize=14,labelfontsize=14,fixedsize=FALSE,boolean=FALSE,showHidden = FALSE){

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
                                        ,node=list(fixedsize=FALSE
                              #              ,fillcolor="gray"
                                            ,fontsize=fontsize
                                            ,shape="ellipse"
                                        )
                            )
                        )
}

#' based on Rgrapvhiz:::renderNodes. plot each gate as png file and add them as svg URL to each node of gating three
.renderNodes.svgAnno <- function(g)
{
  if(!require(RSVGTipsDevice))
    stop("svg output is not supported because RSVGTipsDevice is not installed!")
  ## get necessary render parameters from the graph or use defaults
  ## these are generated by the layout algorithm
  getRenderPar <- Rgraphviz:::getRenderPar
  nodeX <- getRenderPar(g, "nodeX", "nodes")
  nodeY <- getRenderPar(g, "nodeY", "nodes")
  lw <- getRenderPar(g, "lWidth", "nodes")
  rw <- getRenderPar(g, "rWidth", "nodes")
  height <- getRenderPar(g, "height", "nodes")
  labelX <- getRenderPar(g, "labelX", "nodes")
  labelY <- getRenderPar(g, "labelY", "nodes")


  ## these only live within R
  fill <- unlist(getRenderPar(g, "fill", "nodes"))
  col <- unlist(getRenderPar(g, "col", "nodes"))
  lwd <- unlist(getRenderPar(g, "lwd", "nodes"))
  lty <- unlist(getRenderPar(g, "lty", "nodes"))
  textCol <- unlist(getRenderPar(g, "textCol", "nodes"))
  style <- unlist(getRenderPar(g, "style", "nodes"))
  shape <- getRenderPar(g, "shape", "nodes")
  label <- unlist(getRenderPar(g, "label", "nodes"))
  fontsize <- unlist(getRenderPar(g, "fontsize", "nodes"))
  if (is.null(label)) label <- nodes(g)


  ## deal with different shapes
  ## first deal with user-defined functions
  funs <- sapply(shape, is.function)
  if(any(funs)){
    for(i in which(funs)){
      bbox <- matrix(c(nodeX[i]-lw[i], nodeX[i]+rw[i], nodeY[i]-height[i]/2,
              nodeY[i]+height[i]/2), ncol=2)
      try(shape[[i]](bbox, labelX=labelX[i], labelY=labelY[i], fill=fill[i],
              col=col[i], lwd=lwd[i], lty=lty[i], textCol=textCol[i],
              style=style[i], label=label[i], fontsize=fontsize[i]))
    }
  }

  ## now the default shapes
  possible.shapes <-
      c("circle", "ellipse", "box", "rectangle", "plaintext", "triangle", "diamond")
  shape <-
      possible.shapes[pmatch(shape,
              possible.shapes,
              duplicates.ok = TRUE,
              nomatch=5)]
  ## shape == circle
  i <- shape == "circle"
  if (any(i, na.rm=TRUE))
  {
    rad <- pmin(height, (lw+rw))/2
    wh <- which(i)
    sapply(wh, function(ww) {
          symbols(nodeX[ww], nodeY[ww], circles = rad[ww],
              fg = col[ww], bg = fill[ww], lwd = lwd[ww], lty = lty[ww],
              inches = FALSE, add = TRUE)
        }) ## we need to do this because symbols does not recycle lwd
  }
  ## shape == box, rect, etc
  i <- shape %in% c("box", "rectangle", "rect")
  if (any(i, na.rm=TRUE))
  {
    rect(nodeX[i] - lw[i], nodeY[i] - (height[i] / 2),
        nodeX[i] + rw[i], nodeY[i] + (height[i] / 2),
        col = fill[i], border = col[i], lty = lty[i], lwd = lwd[i])
  }
  ## shape == triangle
  ## FIXME: The edges are not computed for triangle shapes in Graphviz
  ##        allthough the correct shape is stored in the agraph object.
  ##        There must be something weird going on internally in the
  ##        C code....
  i <- shape == "triangle"
  if (any(i, na.rm=TRUE))
  {
    wh <- which(i)
    sapply(wh, function(ww) {
          polygon(x = c(nodeX[ww] - lw[ww], nodeX[ww], nodeX[ww] + lw[ww]),
              y = c(nodeY[ww] - (height[ww] / 2),
                  nodeY[ww] + (height[ww] / 2),
                  nodeY[ww] - (height[ww] / 2)),
              col = fill[ww], border = col[ww], lty = lty[ww],
              lwd = lwd[ww])
        })
  }
  ## shape == ellipse
  i <- shape == "ellipse"
  if (any(i, na.rm=TRUE))
  {
    rad <- (lw+rw)/2
    npoints <- 101
    tt <- c(seq(-pi, pi, length = npoints), NA)
    wh <- which(i)
    sapply(wh, function(ww) {
          polygon(nodeX[ww] + sin(tt) * rad[ww],
              nodeY[ww] + cos(tt) * height[ww]/2,
              border = col[ww], col = fill[ww], lwd = lwd[ww],
              lty = lty[ww])
        }) ## we need to do this because polygon does not recycle lwd
  }

  ## shape == diamond
  i <- shape == "diamond"
  if (any(i, na.rm=TRUE))
  {
    for(j in which(i)) polygon(x=c(nodeX[j] - lw[j], nodeX[j], nodeX[j] + rw[j], nodeX[j]),
          y=c(nodeY[j], nodeY[j] + (height[j] / 2), nodeY[j], nodeY[j] - (height[j] / 2)),
          col = fill[j], border = col[j], lty = lty[j], lwd = lwd[j])
  }

  ## shape == plaintext
  ## nothing to do (for style = "filled", use fill = "grey")

  ## compute label cex from node dimensions if not set
  cex <- getRenderPar(g, "cex", "nodes")
  if(is.null(cex)){
    nodeDims <- cbind(lw+rw, height)
    stw <- strwidth(label)
    sth <- strheight(label)
    strDims  <- cbind(stw*1.1, sth*1.4)
    strDims[!nzchar(label),] <- c(strwidth(" "), strheight(" "))
    cex <- min(nodeDims / strDims)
  }

  ## draw labels

  dir <- parent.frame(3)$dir
  gs <- parent.frame(4)$x
  allNodes <- getNodes(gs, showHidden = TRUE)
  png.par <- parent.frame(3)$png.par
  for(i in 1:length(label)){
    #get pop info
    thislabel <- label[i]
    thisPopId <- as.numeric(gsub("N_", "", names(thislabel))) + 1

    if(thisPopId > 1){
      #plot gate
      paths <- tempfile(pattern = "gate", tmpdir = dir ,fileext=".png")

      #open png device
      thisCall <- quote(png(paths))
      thisCall <- as.call(c(as.list(thisCall),png.par))
      eval(thisCall)

      #have to maintain the device stack manually
      #since the device list is circular and dev.cur() will point to dev.next()
      #which really should be dev.prev()
      preDev <- dev.prev()
  
      curPlotObj <- try(plotGate(gs, allNodes[thisPopId],  arrange = FALSE), silent = TRUE)
      # catch error to ensure the device is closed before error is thrown
      if(class(curPlotObj) == "try-error"){

        invisible(dev.off())
        dev.set(preDev) #reset to previous dev instead of next dev
        stop(curPlotObj)

      }else{
        ##
        if(class(gs) == "GatingHierarchy")
          print(curPlotObj[[1]])
        else
          print(curPlotObj)

        invisible(dev.off())
        dev.set(preDev) #reset to previous dev instead of next dev

        #add svg anno
        RSVGTipsDevice::setSVGShapeURL(paths)
        message("gate '", thislabel, "' plotted.")
      }
    }


    #draw label
    text(labelX[i], labelY[i], label[i], col = textCol[i], cex=cex*as.numeric(fontsize[i])/14)
  }

}
#' @param dir \code{character} Default is NULL, which render the gating tree in regular R plot device.
#'                              Otherwise it specifies a folder where the gating tree is output to a svg image
#'                              with some interactivity (e.g. when click on each node, the actual gates will be displayed)
#'                              This interactivity currently only works when the svg is rendered within a HTML webpage (e.g. generated as knitr report)
#' @param svg.par a \code{list} of parameters passed to \code{devSVGTips}
#' @param png.par a \code{list} of parameters passed to \code{png}
#' @importMethodsFrom Rgraphviz renderGraph
.plotGatingTree <- function(g, dir = NULL, svg.par = list(), png.par = list(),...){

  options("warn"=-1)

  lay <- .layoutGraph(g,...)
#  browser()
  if(!is.null(dir))
  {
    if(!require(RSVGTipsDevice))
      stop("svg output is not supported because RSVGTipsDevice is not installed!")
    if(!file.exists(dir))
      dir.create(dir)

    sfile <- tempfile(pattern = "gatingTree", tmpdir = dir, fileext = ".svg")

    thisCall <- quote(devSVGTips(sfile))
    thisCall <- as.call(c(as.list(thisCall),svg.par))
    eval(thisCall)
    preDev <- dev.prev()

    drawNodesFunc <- .renderNodes.svgAnno
  }else
    drawNodesFunc <- "renderNodes"
#  browser()
  res <- try(renderGraph(lay, drawNodes = drawNodesFunc), silent = TRUE)
  if(class(res) == "try-error"){
    invisible(dev.off())
    dev.set(preDev) #reset to previous dev instead of next dev
    stop(res)
  }
  options("warn"=0)
  if(!is.null(dir)){
    invisible(dev.off())
    dev.set(preDev) #reset to previous dev instead of next dev
    .postProcessSVG(sfile)
    sfile
  }


}
#' insert javascript into svg to enable interactity (e.g.links)
#' @importFrom XML xmlTreeParse xmlRoot xmlNode xmlCDataNode addChildren saveXML xmlGetAttr xmlAttrs<-
.postProcessSVG <- function(sfile)
{



  srcFile <- list.files(system.file("javascript",package="flowWorkspace")
                        , pattern="showPlot.js"
                        , full.names=TRUE )
  srcFile <- file(srcFile, "r")
  srcCode <- readLines(srcFile)

  close(srcFile)

  doc <- xmlTreeParse(sfile, useInternalNodes = FALSE, addAttributeNamespaces = TRUE)

  top <- xmlRoot(doc)

  newNode <- xmlNode("script",attrs=c(type="text/ecmascript"))
  newNode <- addChildren(newNode, xmlCDataNode(paste(srcCode,collapse="\n")))
  top[["script"]] <- newNode

  #update xlink with onclick event

  anchor_ind <- grep("a", names(top))
  for(thisInd in anchor_ind)
  {
    thisNode <- top[[thisInd]]
    class(thisNode)
    imgSrc <- xmlGetAttr(thisNode, "xlink:href")
    newNode <- thisNode[["text"]]
    xmlAttrs(newNode) <- c(onclick = paste("showPlot(evt, ", imgSrc, ")", sep = "'"))
    top[[thisInd]] <- newNode
  }
  saveXML(top, sfile)


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
#' \item{dir}:
#'       \code{character} Default is NULL, which render the gating tree in regular R plot device.
#'                              Otherwise it specifies a folder where the gating tree is output to a svg image
#'                              with some interactivity (e.g. when click on each node, the actual gates will be displayed)
#'                              This interactivity currently only works when the svg is rendered within a HTML webpage (e.g. generated as knitr report)
#' \item{svg.par}:
#'         a \code{list} of parameters passed to \code{devSVGTips}
#' \item{png.par}:
#'           a \code{list} of parameters passed to \code{png}
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
#'  # output as svg format with onclick event of each node displaying the gate(saved as png files) within pop-up window
#'  plot(gs[[1]], dir = "myFolder")
#'
#'  # customize the size of svg and png files
#'  plot(gh, dir = "myFolder", svg.par =list(width = 7, height = 7), png.par = list(width = 200, height = 200)
#'
#'  #each node link to multiple gates across samples
#'  #here is the example code showing how to embed
#'  #the svg output within knitr quick report
#'
#'  #+ eval=T, results = "asis", message = F
#'  svgFile <- plot(gs, dir = './svg', svg.par =list(width = 7, height = 7), png.par = list(width = 400, height = 400 ))
#'  cat("<embed src=", svgFile, " type='image/svg+xml' />", sep = "")
#
#' }
#'
#' @rdname plot-methods
#' @aliases plot
#' @export
#' @importFrom stats4 plot
setMethod("plot",c("GatingSet","missing"),function(x,y,...){

#           browser()
      g <- .getGraph(x[[1]]) #get tree from the first sample
      .plotGatingTree(g, ...)

    })

.getAllDescendants <- function(gh,startNode,nodelist){

  children_nodes <- getChildren(gh,startNode)
  if(length(children_nodes)>0){
    for(this_parent in children_nodes){
      nodelist$v <- c(nodelist$v, .getNodeInd(gh, this_parent))
      .getAllDescendants (gh,this_parent,nodelist)
    }
  }

}

#' @rdname plot-methods
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
      .plotGatingTree(g, ...)

    })

setMethod("show","GatingHierarchy",function(object){
			cat("Sample: ",sampleNames(object),"\n");
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
#' @aliases keyword
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
#' @rdname keyword
#' @export
setMethod("keyword",c("GatingHierarchy","character"),function(object,keyword){

			keyword(object)[[keyword]]
		})
#' @rdname keyword
#' @export    
setMethod("keyword",c("GatingHierarchy","missing"),function(object,keyword = "missing"){

      flowCore::keyword(getData(object, use.exprs = FALSE))
    })

#'  Get the names of all nodes from a gating hierarchy.
#' 
#'  \code{getNodes} returns a character vector of names of the nodes (populations) in the \code{GatingSet}.
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
#' getNodes returns a \code{character} vector of node/population names, ordered appropriately.
#'
#' @examples
#'   \dontrun{
#'     #G is a gating hierarchy
#'     getNodes(G, path = 1])#return node names (without prefix)
#'     getNodes(G,path = "full")#return the full path
#'     getNodes(G,path = 2)#return the path as length of two 
#'     getNodes(G,path = "auto)#automatically determine the length of path 
#'     setNode(G,"L","lymph")
#'   }
#' @aliases getNodes
#' @rdname getNodes
#' @export 
#' @importFrom BiocGenerics duplicated
#' @importFrom plyr ddply . ldply
setMethod("getNodes","GatingSet",function(x,y=NULL,order="regular", path = "full", showHidden = FALSE, ...){

            order <- match.arg(order,c("regular","tsort","bfs"))
            orderInd <- match(order,c("regular","tsort","bfs"))
            orderInd <- orderInd - 1

            if(is.numeric(path)){
              nodeNames <- .Call("R_getNodes",x@pointer,sampleNames(x)[1],as.integer(orderInd),TRUE,showHidden)
              
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
                nodeNames <- .Call("R_getNodes",x@pointer,sampleNames(x)[1],as.integer(orderInd),FALSE,showHidden)
              
            }else if(path == "full"){
              nodeNames <- .Call("R_getNodes",x@pointer,sampleNames(x)[1],as.integer(orderInd),TRUE,showHidden)
            }else
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
#' @param showHidden \code{logical} whether to include the hidden children nodes.
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
#' @aliases getParent
#' @rdname getParent
#' @export 
setMethod("getParent",signature(obj="GatingSet",y="character"),function(obj,y, ...){
            pind <- .Call("R_getParent",obj@pointer,sampleNames(obj)[1], y)
            pind <- pind +1
			getNodes(obj, showHidden = TRUE, ...)[pind]
		})
#' @rdname getParent
#' @export
#' @aliases getChildren   
setMethod("getChildren",signature(obj="GatingSet",y="character"),function(obj,y, showHidden = TRUE, ...){
			
            cind <- .Call("R_getChildren",obj@pointer,sampleNames(obj), y, showHidden)
            cind <- cind + 1
			getNodes(obj, showHidden = TRUE, ...)[cind]
})
#' @param y \code{character} node name or path
#' @rdname getPopStats
#' @export
#' @aliases getProp
setMethod("getProp",signature(x="GatingHierarchy",y="character"),function(x,y,flowJo = FALSE){
			#Return the proportion of the population relative to the parent and relative to the total.
			#y is nodename
            
			stats<-.getPopStat(x,y)
			if(flowJo)
				unname(stats$flowJo["proportion"])
			else
				unname(stats$flowCore["proportion"])

		})
#' @rdname getPopStats
#' @export   
#' @aliases getTotal 
setMethod("getTotal",signature(x="GatingHierarchy",y="character"),function(x,y,flowJo = FALSE){
            stats<-.getPopStat(x,y)
			if(flowJo)
				unname(stats$flowJo["count"])
			else
				unname(stats$flowCore["count"])
#			browser()

		})


.getPopStat<-function(x,y){
	stopifnot(!missing(y))

    
	stats<-.Call("R_getPopStats",x@pointer,sampleNames(x), y)

    
	parent<-try(getParent(x, y),silent=T)


	if(class(parent)=="try-error")#if parent exist
		pstats<-stats
	else
	{
		
		pstats<-.Call("R_getPopStats",x@pointer,sampleNames(x), parent)
	}


#	browser()
	list(flowCore=c(proportion=as.numeric(ifelse(pstats$FlowCore["count"]==0
										,0
										,stats$FlowCore["count"]/pstats$FlowCore["count"]
										))
					,count=as.numeric(stats$FlowCore["count"]))
		,flowJo=c(proportion=as.numeric(ifelse(pstats$FlowJo["count"]==0
										,0
										,stats$FlowJo["count"]/pstats$FlowJo["count"]
										))
					,count=as.numeric(stats$FlowJo["count"]))
		)
}
#' @rdname getPopStats
#' @export
setMethod("getPopStats","GatingHierarchy",function(x, path = "auto", ...){


        nodePath <- getNodes(x, path = path, ...)
        
        stats <- ldply(nodePath, function(thisPath){
    					curStats <- .getPopStat(x,thisPath)
                        data.frame(flowCore.freq = curStats$flowCore["proportion"]
                                    ,flowJo.freq = curStats$flowJo["proportion"]
            						,flowJo.count = curStats$flowJo["count"]
            						,flowCore.count = curStats$flowCore["count"]
                                    , node = thisPath
                                    , stringsAsFactors = FALSE
            						)
            		
                      })

      stats <- data.table(stats)
      rownames(stats) <- stats[, node]
      stats
		})

.computeCV_gh <- function(gh, ...){

    x<-getPopStats(gh, ...)
    rn<-rownames(x)
    x<-as.data.frame(x)
    rownames(x)<-rn
    cv<-apply(as.matrix(x[,c("flowJo.count","flowCore.count")]),1,function(y)IQR(y)/median(y));
    cv<-as.matrix(cv,nrow=length(cv))
    cv[is.nan(cv)]<-0
    rownames(cv) <- as.character(rownames(x))
    cv
}
#' @importFrom lattice barchart
#' @export
#' @rdname plotPopCV
setMethod("plotPopCV","GatingHierarchy",function(x,m=2,n=2, path = "auto", ...){
      cv <- .computeCV_gh(x, path = path)
      return(barchart(cv,xlab="Coefficient of Variation",..., par.settings=ggplot2like));
    })


#'  Return the flowCore gate definition associated with a node in a GatingHierarchy/GatingSet.
#'
#'  Return the flowCore gate definition object associated with a node in a \code{GatingHierarchy} or \code{GatingSet} object.
#' 
#' @param obj A \code{GatingHierrarchy} or \code{GatingSet}
#' @param y A \code{character} the name or full(/partial) gating path of the node of interest.  Or, a \code{numeric} index into the node list of nodes in the \code{GatingHierarchy} or \code{GatingSet}.
#' 
#' @return  A gate object from \code{flowCore}. Usually a \code{polygonGate}, but may be a \code{rectangleGate}. Boolean gates are represented by a \code{"BooleanGate"} S3 class. This is a list boolean gate definition that references populations in the GatingHierarchy and how they are to be combined logically. If \code{obj} is a \code{GatingSet}, assuming the trees associated with each \code{GatingHierarchy} are identical, then this method will return a list of gates, one for each sample in the \code{GatingSet} corresponding to the same population indexed by \code{y}.
#' 
#' @seealso \code{\link{getData}} \code{\link{getNodes}}
#' @examples
#'   \dontrun{	#gh is a GatingHierarchy
#'     getGate(gh, "CD3") #return the gate for the fifth node in the tree, but fetch it by name.
#'     #G is a GatingSet
#'     getGate(G, "CD3") #return a list of gates for the fifth node in each tree
#'   }
#' @aliases
#' getGate
#' @importFrom flowCore polygonGate rectangleGate
#' @rdname getGate
#' @export
setMethod("getGate",signature(obj="GatingHierarchy",y="character"),function(obj,y){
			

				g<-.Call("R_getGate",obj@pointer,sampleNames(obj), y)
				filterId <- g$filterId
				if(g$type==1)
				{


					mat<-matrix(c(g$x,g$y),ncol=2,dimnames=list(NULL,g$parameters))
					if(nrow(mat)==2)#convert to rectangleGate
						rectangleGate(.gate=mat,filterId=filterId)
					else
						polygonGate(.gate=mat,filterId=filterId)
				}else if(g$type==2)
					rectangleGate(.gate=matrix(g$range,dimnames=list(NULL,g$parameters)),filterId=filterId)
				else if(g$type %in% c(3,6))
				{

					refPaths<-unlist(lapply(g$ref,function(curPath){
										paste(curPath,collapse="/")

									})
								)

                    #get rid of the first op
                    g$v2[1] <- ""
                    boolExpr <- paste(g$v2, g$v,refPaths,sep="")
                    boolExpr <- paste(boolExpr,collapse="")
                    if(nchar(boolExpr) > 0)
                    boolExpr <- as.symbol(boolExpr)

                    g <- eval(substitute(booleanFilter(xx, filterId=filterId),list(xx=boolExpr)))
					g
				}else if (g$type == 4){
                    cov.mat <- g$cov
                    dimnames(cov.mat) <- list(g$parameters, g$parameters)
                    ellipsoidGate(.gate = cov.mat, mean = g$mu, distance = g$dist, filterId = filterId) 
                }else
					stop("not supported gate type",g$type)

			
		})
        
.getNodeInd <- function(obj,y, ...){
    
    ind <- .Call("R_getNodeID",obj@pointer,sampleNames(obj)[1], y)

    ind + 1 # convert to R index

}


#'  Get the membership indices for each event with respect to a particular gate in a GatingHierarchy
#'
#'  Returns a logical vector that describes whether each event in a sample is included or excluded by this gate.
#' 
#' @param obj A \code{GatingHierarchy} representing a sample.
#' @param y A \code{character} giving the name or full(/partial) gating path or index of the population / node of interest.
#' 
#' @details  Returns a logical vector that describes whether each event in the data file is included in the given gate of this \code{GatingHierarchy}. The indices are for all events in the file, and do not reflect the population counts relative to the parent but relative to the root. To get population frequencies relative to the parent one cross-tabulate the  indices of \code{y} with the indices of its parent.
#' 
#' @return  A logical vector of length equal to the number of events in the FCS file that determines whether each event is or is not included in the current gate.
#' 
#' @note Generally you should not need to use \code{getIndices} but the more convenient methods \code{getProp} and \code{getPopStats} which return population frequencies relative to the parent node.
#' The indices returned reference all events in the file and are not directly suitable for computing population statistics, unless subsets are taken with respect to the parent populations.
#' 
#' @seealso \code{\link{getPopStats}}
#' 
#' @examples
#'   \dontrun{
#'     #G is a gating hierarchy
#'     #Return the indices for population 5 (topological sort)
#'     getIndices(G,getNodes(G,tsort=TRUE)[5]);
#' }
#'
#' @aliases getIndices
#' @importFrom ncdfFlow getIndices
#' @rdname getIndices
#' @export
setMethod("getIndices",signature(obj="GatingHierarchy",y="character"),function(obj,y){
            
            .Call("R_getIndices",obj@pointer,sampleNames(obj), y)

		})


setGeneric("isGated",function(obj, y, ...)standardGeneric("isGated"))
setMethod("isGated",signature(obj="GatingHierarchy",y="character"),function(obj,y){

#			browser()
      
      .Call("R_getGateFlag",obj@pointer,sampleNames(obj), y)

    })


setGeneric("isNegated",function(obj, y, ...)standardGeneric("isNegated"))
setMethod("isNegated",signature(obj="GatingHierarchy",y="character"),function(obj,y){
      
      .Call("R_getNegateFlag",obj@pointer,sampleNames(obj), y)
      
      
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
#'   \code{\link{flowData}} \code{\link{getIndices}} \code{\link{getPopStats}}
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
#' @aliases getData
#' @rdname getData-methods
#' @export
setMethod("getData",signature(obj="GatingHierarchy",y="missing"),function(obj,y, ...){
      if(!obj@flag){
        stop("Must gate the data before fetching data");
      }

      fs <- flowData(obj)
      fs[[sampleNames(obj),...]]
      
})

#' @rdname getData-methods
#' @export
setMethod("getData",signature(obj="GatingHierarchy",y="character"),function(obj,y, ...){
        
        this_data <- getData(obj, ...)
        if(y == "root"){
          return (this_data)
        }else{
          
          this_indice <- getIndices(obj,y)
          return (this_data[this_indice,])
        }
})


.isBoolGate<-function(x,y){
	return (class(getGate(x,y))=="booleanFilter")
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
      
        
        
        res <- getAxisLabels(gh)[[channel]] #this call is to be deprecated once we figure out how to preserve trans when cloning GatingSet
        if(is.null(res)){
          #try to grab trans and do inverse trans for axis label on the fly
            trans <- getTransformations(gh, channel)
            if(is.null(trans)){
              res <- NULL
            }else{
              inverseTrans <- getTransformations(gh, channel, inverse = TRUE)
              
              fr <- getData(gh, use.exprs = FALSE)
              r <- as.vector(range(fr)[,channel])#range
              rw <- inverseTrans(r)
              ######################################
              #create equal spaced locations at raw scale
              ######################################
              base10raw <- unlist(lapply(2:6,function(e)10^e))
              base10raw <- c(0,base10raw)
              raw <- base10raw[base10raw>min(rw)&base10raw<max(rw)]
              pos <- signif(trans(raw))
              #format it
              label <- pretty10exp(as.numeric(raw),drop.1=TRUE)
              
              res <- list(label = label, at = pos)
            }
          
        }else{
          #use the stored axis label if exists
          res$label <- pretty10exp(as.numeric(res$label),drop.1=TRUE)
        }
        
        res
}
        
#to be deprecated        
getAxisLabels <- function(obj,...){
  obj@axis[[sampleNames(obj)]]
}
        
#' Return a list of transformations or a transformation in a GatingHierarchy
#'
#' Return a list of all the transformations or a transformation in a GatingHierarchy
#'
#' @param x A \code{GatingHierarchy} object
#' @param ... other arguments
#' 
#'         inverse \code{logical} whether to return the inverse transformation function.
#'         channel \code{character} channel name
#' @details
#' Returns a list of the transformations or a transformation in the flowJo workspace. The list is of length \code{L}, where \code{L} is the number of distinct transformations applied to samples in the \code{flowJoWorkspace}. Each element of \code{L} is itself a \code{list} of length \code{M}, where \code{M} is the number of parameters that were transformed for a sample or group of samples in a \code{flowJoWorkspace}. For example, if a sample has 10 parameters, and 5 are transformed during analysis, using two different sets of transformations, then L will be of length 2, and each element of L will be of length 5. The elements of \code{L} represent channel- or parameter-specific transformation functions that map from raw intensity values to channel-space used by flowJo.
#' this method currently is used convert transformation funtion from c++ to R
#' mainly for transforming range info
#' @return
#' lists of functions, with each element of the list representing a transformation applied to a specific channel/parameter of a sample.
#' @examples
#' \dontrun{
#' 	#Assume gh is a GatingHierarchy
#' 	getTransformations(gh); # return a list transformation functions
#'  getTransformations(gh, inverse = TRUE); # return a list inverse transformation functions 
#'  getTransformations(gh, channel = "<B710-A") # only return the transfromation associated with given channel
#' }
#' @aliases getTransformations
#' @rdname getTransformations
setMethod("getTransformations","GatingHierarchy",function(x, ...){
#			browser()
      .getTransformations(x@pointer,sampleNames(x), ...)
		})
    
.getTransformations <- function(pointer,sampleName, channel = NULL, ...){
    trans <- .Call("R_getTransformations",pointer,sampleName)
    transList <- lapply(trans,function(curTrans){
#						browser()
          if(curTrans$type=="log")
          {

            f <- function(x){
              sapply(x, function(i)ifelse(i>0,log10((i)/max_val)/decade+offset,min_val))
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


          }else if(curTrans$type %in% c("caltbl" , "biexp"))
             f <- .flowJoTrans(curTrans, ...)

          return (f)
        })
    
    #try to match to given channel
    if(!is.null(channel)){
      trans_names <- names(transList)
      #do strict match first
      j <- grep(paste0("^", channel, "$"), trans_names)
      if(length(j) == 0){
        #do fuzzy match if no matches
        j <- grep(channel, trans_names)
      }
      
      if(length(j) > 1){
          stop("multiple tranformation functions matched to: ", channel)
      }else if(length(j) == 0){
        return(NULL)
      }else{
        transList[[j]]
      }
    }else
      transList
    
  }
  
#' wrap the calibration table into transformation function using stats:::splinefun
#' 
#' @param coef the coefficients returned by the calibration table from flowJo
.flowJoTrans <- function(coef, inverse = FALSE){
  if(inverse){
    #swap x y vector
    x <- coef$z$x 
    coef$z$x <- coef$z$y
    coef$z$y <- x
    
  }
  #define the dummy spline function(simplied version of the one from stats package)
  f <- function (x, deriv = 0)
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
  z<-coef$z
  z$n<-length(z$x)
  z$method<-coef$method
  assign("z",z,environment(f))
  
  type <- coef$type 
  attr(f,"type") <- type 
  if(type == "biexp")
      attr(f,"parameters") <- list(channelRange = coef$channelRange
                                  , maxValue = coef$maxValue
                                  , neg = coef$neg
                                  , pos = coef$pos
                                  , widthBasis = coef$widthBasis
                                )
  
  
  return (f)
  
}  
#' construct the flowJo-type biexponentioal transformation function
#' 
#' Normally it was parsed from flowJo xml workspace. This function provides the alternate 
#' way to construct the flowJo version of logicle transformation function within R.
#' 
#' @param channelRange \code{numeric} the maximum value of transformed data
#' @param maxValue \code{numeric} the maximum value of input data
#' @param pos \code{numeric} the full width of the transformed display in asymptotic decades
#' @param neg \code{numeric} Additional negative range to be included in the display in asymptotic decades
#' @param widthBasis \code{numeric} unkown.
#' @param inverse \code{logical} whether to return the inverse transformation function. 
#' @export 
flowJoTrans <- function(channelRange=4096, maxValue=262144, pos = 4.5, neg = 0, widthBasis = -10, inverse = FALSE){
  
  coef <- .getSplineCoefs(channelRange = channelRange, maxValue = maxValue, pos = pos, neg = neg, widthBasis = widthBasis)
  .flowJoTrans(coef, inverse = inverse)
  
}

#'  Retrieve the compensation matrices from a GatingHierarchy
#'
#'  Retrieve the compensation matrices from a GatingHierarchy.
#' 
#' @param x A \code{GatingHierarchy} object.
#' 
#' @details Return all the compensation matrices in a GatingHierarchy.
#' @return
#'   A list of \code{matrix} representing the spillover matrix in \code{GatingHierarchy}
#' @examples
#'   \dontrun{
#' 	#Assume gh is a GatingHierarchy
#'   getCompensationMatrices(gh);
#' }
#' @aliases getCompensationMatrices
#' @export 
#' @rdname getCompensationMatrices
setMethod("getCompensationMatrices","GatingHierarchy",function(x){
			comp<-.Call("R_getCompensation",x@pointer,sampleNames(x))
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



#TODO: to inverse transform the range in order to display the raw scale
setMethod("plotGate",signature(x="GatingHierarchy",y="character"),function(x,y,...){
      .plotGate.gh(x,y, ...)
})
setMethod("plotGate",signature(x="GatingHierarchy",y="missing"),function(x,y,...){

        y <- getNodes(x)
        y <- setdiff(y,"root")

		plotGate(x,y,...)
		})
#' @examples \dontrun{
#' projections <- list("cd3" = c(x = "cd3", y = "AViD")
#'                     , "cd4" = c(x = "cd8", y = "cd4")
#'                     , "cd4/IL2" = c(x = "IL2", y = "IFNg")
#'                     , "cd4/IFNg" = c(x = "IL2", y = "IFNg")
#'                 )
#' plotGate(gh, c("cd3", "cd4", "cd4/IL2", "cd4/IFNg"), path = "auto", projections = projections, gpar = c(nrow = 2))
#'
#' }
#' @importFrom gridExtra grid.arrange
#' @importFrom flowCore getChannelMarker
#' @rdname plotGate-methods
setMethod("plotGate", signature(x="GatingHierarchy",y="numeric")
                    , function(x, y, ...
                                ){
      plotGate(x, getNodes(x, path = "auto")[y], ...)                                
                    })
.plotGate.gh <- function(x, y, bool=FALSE
                            , arrange.main = sampleNames(x),arrange=TRUE,merge=TRUE
                            , par.settings = list()
                            , gpar = NULL
                            , projections = list()
                            , ...){
                               
			if(!x@flag){
				message("Can't plot until you gate the data \n");
				return();
			}

			
            par.settings <- lattice:::updateList(flowWorkspace.par.get("theme.novpadding"), par.settings)

            #convert popname to id
#            prjNodeInds <- try(sapply(names(projections),.getNodeInd, obj = x), silent = TRUE)
#            if(class(prjNodeInds) == "try-error")
#              stop("Invalid 'projections': ", geterrmessage())

#            names(projections) <- prjNodeInds
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
			if(flowWorkspace.par.get("plotGate")[["arrange"]]&&arrange)
				do.call(grid.arrange,c(plotObjs,main = arrange.main,gpar))
			else
				plotObjs

}
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
							pid<-getParent(gh,y, path = "auto")
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

			curPid <- strsplit(curKey, split="|", fixed=TRUE)[[1]][1]#extract pid
			plotList[[toReplaceInd]] <- list(popIds=toMerge,parentId=curPid)
			plotList[toRemoveInd] <- NULL
			poplist[toRemoveInd] <- NULL#make sure syn y as well vector since it is used to index plotList
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
                          , raw.scale = TRUE
                          , ...){
	

        if(is.null(xParam)||!raw.scale){
          x.labels <- NULL
        }else{
          x.labels <- prettyAxis(x, xParam)
        }

        if(is.null(yParam)||!raw.scale){
          y.labels <- NULL
        }else{
		  y.labels <- prettyAxis(x, yParam)
        }

		#init the scales and x,y lim
		#update axis when applicable
		if(!is.null(x.labels))
		{
			
			xscales<-list(x=list(at=x.labels$at,labels=x.labels$label))
			scales<-lattice:::updateList(xscales,scales)

		}
		if(!is.null(y.labels))
		{
			
			yscales<-list(y=list(at=y.labels$at,labels=y.labels$label))
			scales<-lattice:::updateList(scales,yscales)

		}



	list(scales=scales)
}

#'  Update the name of one node in a gating hierarchy/GatingSet.
#'
#'  \code{setNode} update the name of one node in a gating hierarchy/GatingSet.
#' @param value A \code{character} the name of the node. or \code{logical} to indicate whether to hide a node
#' @examples
#'   \dontrun{
#'     #G is a gating hierarchy
#'     getNodes(G[[1]])#return node names
#'     setNode(G,"L","lymph")
#'   }
#' @aliases setNode
#' @rdname setNode-methods
#' @export 
setMethod("setNode"
    ,signature(x="GatingHierarchy",y="character",value="character")
    ,function(x,y,value){
      
      .Call("R_setNodeName",x@pointer,sampleNames(x), y,value)
    })

#' hide/unhide a node
#'
#' @param x \code{GatingHierarchy} object
#' @param y \code{character} node name or path
#' @examples
#' \dontrun{
#'      setNode(gh, 4, FALSE) # hide a node
#'      setNode(gh, 4, TRUE) # unhide a node
#' }
#' @export
#' @rdname setNode-methods
setMethod("setNode"
    ,signature(x="GatingHierarchy",y="character",value="logical")
    ,function(x,y,value){
      
      hidden = !value
      .Call("R_setNodeFlag",x@pointer,sampleNames(x), y, hidden)
    })

#' @rdname sampleNames
#' @export
setMethod("sampleNames","GatingHierarchy",function(object){
      object@name
    })


#' @export
#' @rdname pData-methods
setMethod("pData","GatingHierarchy",function(object){
      pData(flowData(object))[sampleNames(object),]
    })

