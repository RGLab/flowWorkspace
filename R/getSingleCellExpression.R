### Extract single-cell data by boooean expansion. For COMPASS

#' routine to return the indices by specify boolean combination of reference nodes:
#'
#' It adds the boolean gates and does the gating on the fly, and
#' return the indices associated with that bool gate, and remove the bool gate
#' the typical use case would be extracting any-cytokine-expressed cells
#'
#' @param obj \code{GatingSet}
#' @param y a quoted expression.
#' @examples
#' \dontrun{
#'
#'	getIndices(gs,quote(`4+/TNFa+|4+/IL2+`))
#'
#'}
#' @export
setMethod("getIndices",signature=c("GatingSet","name"),function(obj, y){
      
      bf <- eval(substitute(booleanFilter(v),list(v=y)))
      gh <- obj[[1]]
      
      suppressMessages({
            suppressWarnings(
                id <- add(obj,bf)
            )
            
            allNodes <- getNodes(gh, showHidden = TRUE)
            this_node <- allNodes[id]
            
            
            res <-try(recompute(obj,this_node),silent=T)
          })
      
      
      if(class(res)=="try-error"){
        Rm(this_node,obj)
        stop(res)
      }else{
        this_ind <- lapply(obj,function(this_gh)getIndices(this_gh,this_node))
        Rm(this_node,obj)
        this_ind
      }
      
    })
#' Return the single-cell matrix of 1/0 dichotomized expression
#' @param gh \code{GatingHierarchy} object
#' @param y \code{character} node name
#' @export
getIndiceMat <- function(gh,y){
  strExpr <- as.character(y)
  nodes <- strsplit(strExpr,split="\\|")[[1]]
  .getIndiceMat(gh, sampleNames(gh), nodes)
  
}

.getIndiceMat <- function(gs, thisSample, nodes){
  
  #extract logical indices for each cytokine gate
  indice_list <- sapply(nodes,function(this_node).cpp_getIndices(gs@pointer, thisSample, this_node)
      ,simplify = FALSE)
  
  #construct the indice matrix
  do.call(cbind, indice_list)
#  as.data.table(indice_list)
}
#' create mapping between pops and channels
#'
#' The goal is translate the markers provided by user map or directly parsed from popNames (when map is NULL)
#' to the accurate channel info for indexing the flow data
#' because user might give the short form of either  'name' or 'desc' of flowFrame based on swap argument.
#'
#' @param this_pd \code{data.frame} extraced from flowFrame object to provide the channel and marker info
#' @param popNames \code{character} node names in gating tree
#' @param map \code{list} contains the node-to-marker mapping explicitly specified user
#'
#' @importFrom dplyr mutate
#' @noRd 
.getPopChnlMapping <- function(this_pd, popNames, map =  NULL, swap = FALSE, ignore.case = FALSE){
  
  datSrc <- ifelse(swap, "name", "desc")
  this_pd[, datSrc] <- as.vector(this_pd[, datSrc])
  
  #match to the pdata of flow frame
  all_markers <- this_pd[,datSrc]
  all_markers[is.na(all_markers)] <- "NA"
  
  
  lapply(popNames, function(popName){
        
        
        #fetch the marker from map first
        toMatch <- map[[popName]]
        #if not supplied, parse it from popName
        if(is.null(toMatch)){
          this_pops <- strsplit(split="/", popName, fixed=TRUE)[[1]]
          #get the terminal node
          toMatch <- this_pops[length(this_pops)]
        }
        
        
        #partial match first
        if(ignore.case)
          matchedInd <- grep(tolower(toMatch), tolower(all_markers), fixed = TRUE)
        else
          matchedInd <- grep(toMatch, all_markers, fixed = TRUE)
        if(length(matchedInd) > 1)
        {
          #Switch to exact match because multiple markers matched
          if(ignore.case)
            matchedInd <- match(tolower(toMatch), tolower(all_markers))
          else
            matchedInd <- match(toMatch, all_markers)
          matchedInd <- matchedInd[!is.na(matchedInd)]
          
          if(length(matchedInd) == 0){
            #no exact match
            stop(toMatch, "  paritally matched to multiple markers but failed to exactly matched to any of them. ")
          }else if(length(matchedInd) > 1){
            #multiple exact matches
            stop(toMatch, "exactly matches to multiple markers!")
          }
        }else if(length(matchedInd) == 0){
          #no partial match
          stop("Marker not found: ", toMatch)
        }
        
        this_pd[matchedInd,c("name", "desc")]
        
      })%>% bind_rows %>% mutate(pop = popNames)
  
  
}

#' Return the cell events data that express in any of the single populations defined in \code{y}
#'
#' Returns a list of matrix containing the events that expressed in any one of the populations defined in \code{y}
#'
#' @param x A \code{GatingSet} or \code{GatingSetList} object .
#' @param nodes \code{character} vector specifying different cell populations
#' @param ... other arguments
#' 
#'        other.markers \code{character} vector specifying the extra markers/channels to be returned besides the ones derived from "nodes" and "map" argument.It is only valid when threshold is set to FALSE.
#' 
#'        swap \code{logical} indicates whether channels and markers of flow data are swapped.
#'        
#'        threshold \code{logical} indicates whether to threshold the flow data by setting intensity value to zero when it is below the gate threshold.
#'        
#'        marginal \code{logical} indicates whether to the gate is treaded as 1d marginal gate. Default is TRUE, which means markers are determined either
#'                                by node name or by 'map' argument explained below. When FALSE, the markers are determined by the gate dimensions.
#'                                and node name and 'map' argument are ignored.
#' 
#'        map a named list providing the mapping between node names (as specified in the gating hierarchy of the gating set) and channel
#'                         names (as specified in either the \code{desc} or \code{name}
#'                          columns of the parameters of the associated \code{flowFrame}s
#'                          in the \code{GatingSet}). see examples.
#'
#'        ignore.case whether to ignore case when match the marker names. Default is FALSE.
#'
#'        mc.cores passed to \code{mclapply}. Default is 1, which means the process runs in serial mode. When it is larger than 1, parallel mode is enabled.
#' 
#' @return A \code{list} of \code{numerci matrices}
#' @aliases getSingleCellExpression
#' @author Mike Jiang \email{wjiang2@@fhcrc.org}
#' @seealso \code{\link{getIndices}}  \code{\link{getPopStats}}
#' @examples \dontrun{
#'   #G is a GatingSet
#' 	nodes <- c("4+/TNFa+", "4+/IL2+")
#' 	res <- getSingleCellExpression(gs, nodes)
#' 	res[[1]]
#' 	
#'  # if it fails to match the given nodes to the markers, then try to provide the mapping between node and marker explicitly
#' 	res <- getSingleCellExpression(gs, nodes , map = list("4+/TNFa+" = "TNFa", "4+/IL2+" = "IL2"))
#' 	
#' 	# It can also operate on the 2d gates by setting marginal to FALSE
#' 	# The markers are no longer deduced from node names or supplied by map
#' 	# Instead, it retrieves the markers that are associated with the gates
#' 	nodes <- c("4+/TNFa+IFNg+", "4+/IL2+IL3+")
#' 	res <- getSingleCellExpression(gs, nodes, marginal = FALSE)
#' 	#or simply call convenient wrapper
#' 	getSingleCellExpressionByGate(gs, nodes)
#' }
#' @rdname getSingleCellExpression
#' @importFrom dplyr bind_rows
#' @export
setMethod("getSingleCellExpression",signature=c("GatingSet","character"),function(x, nodes, ...){
      .getSingleCellExpression(x, nodes, ...)
    })
.getSingleCellExpression <- function(x, nodes
    , other.markers = NULL
    , swap = FALSE
    , threshold = TRUE
    , marginal = TRUE
    , mc.cores = getOption("mc.cores", 1L)
    , ...){
  datSrc <- ifelse(swap, "name", "desc")
  fs <- getData(x)
  sn <- sampleNames(x)
  
  names(sn) <- sn
  
  thisCall <- quote(lapply(sn,function(sample){
            
            message(".", appendLF = FALSE)
            fr <- fs[[sample, use.exprs = FALSE]]
            this_pd <- pData(parameters(fr))
            
            if(marginal){#parse marker info from node name or map argument
              #get pop vs channel mapping
              pop_chnl <- .getPopChnlMapping(this_pd, nodes, swap = swap, ...)  
              chnls <- as.character(pop_chnl[["name"]])
              pops <-  as.character(pop_chnl[["pop"]])
              markers <- as.character(pop_chnl[[datSrc]])
            }else{#parse markers from gates
              chnls <- lapply(nodes, function(node){
                    gate <- getGate(x[[sample]], node)
                    if(is(gate, "booleanFilter"))
                      stop("can't operate on boolean gates: ", node)
                    dim <- parameters(gate)
                    names(dim) <- rep(node, length(dim))
                    dim
                  })
              chnls <- unlist(chnls)
              
              #pops will be repeated for 2d gate to keep its length consistent with the number of chnls
              #so that the original c++ code still works without modification
              pops <- names(chnls)
              marker_chnl <- lapply(chnls, getChannelMarker, frm = fr)%>% bind_rows
              markers <- marker_chnl[[datSrc]]
              markers.unique <- unique(markers)
              markers_pops <- sapply(markers.unique, function(marker){
                                                    pops[markers == marker]
                                            }, simplify = FALSE)
              markers <- markers.unique
              chnls <- unique(chnls)
            }
            
            
            
            #append the extra markers
            if(!is.null(other.markers)){
              
              other_marker_chnl <- lapply(other.markers, getChannelMarker, frm = fr)%>% bind_rows
              
              #remove the other_chnls that are already present in chnls
              toKeep <- ! other_marker_chnl[["name"]] %in% chnls
              other_marker_chnl <- other_marker_chnl[toKeep, ,drop = FALSE]
              chnls <- c(chnls, other_marker_chnl[["name"]])
              markers <- c(markers, other_marker_chnl[[datSrc]])
            }
            
            
            
            data <- fs[[sample, unique(chnls)]]
            data <- exprs(data)
            if(marginal)
              data <- .cpp_getSingleCellExpression(x@pointer, sample, pops, data, markers, threshold)
            else#modify data in place
              data <- .cpp_getSingleCellExpressionByGate(x@pointer, sample, markers_pops, data, markers, threshold)
            
            data
            
            
          })
  )
  
  if(mc.cores > 1){
    requireNamespace(parallel)
    thisCall[[1]] <- quote(mclapply)
    thisCall[["mc.cores"]] <- mc.cores
  }
  eval(thisCall)
}

#' @rdname getSingleCellExpression
#' @export
getSingleCellExpressionByGate <- function(...){
  getSingleCellExpression(..., marginal = FALSE)  
}

