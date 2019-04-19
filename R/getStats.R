#' @templateVar old getStats
#' @templateVar new gs(/gh)_pop_get_stats
#' @template template-depr_pkg
NULL
#' Extract stats from populations(or nodes)
#'
#'
#' @param x a GatingSet or GatingHierarchy
#' @param ... arguments passed to \link{gs_get_pop_paths} method.
#' @return a data.table that contains stats values (if MFI, for each marker per column)
#'  along with 'pop' column and 'sample' column (when used on a 'GatingSet')
#' @import flowCore
#' @import ncdfFlow
#' @import data.table
#' @export
#' @aliases gh_pop_get_stats
#' @examples
#' \dontrun{
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#'
#' # get stats all nodes
#' dt <- gs_pop_get_stats(gs) #default is "count"
#'
#' nodes <- c("CD4", "CD8")
#' gs_pop_get_stats(gs, nodes, "percent")
#'
#' # pass a build-in function
#' gs_pop_get_stats(gs, nodes, type = pop.MFI)
#'
#' # compute the stats based on the raw data scale
#' gs_pop_get_stats(gs, nodes, type = pop.MFI, inverse.transform = TRUE)
#'
#' # supply user-defined stats fun
#' pop.quantiles <- function(fr){
#'    chnls <- colnames(fr)
#'    res <- matrixStats::colQuantiles(exprs(fr), probs = 0.75)
#'    names(res) <- chnls
#'    res
#'    }
#' gs_pop_get_stats(gs, nodes, type = pop.quantiles)
#' }
#' @rdname gs_pop_get_stats
getStats <- function(x, ...)UseMethod("getStats")

#' @export
#' @rdname gs_pop_get_stats
getStats.GatingSetList <- function(x, ...){
  getStats.GatingSet(x, ...)
}

#' @export
#' @rdname gs_pop_get_stats
getStats.GatingSet <- function(...){
	.Deprecated("gs_pop_get_stats")
  gs_pop_get_stats(...)
}
#' @export
#' @rdname gs_pop_get_stats
gs_pop_get_stats <- function(x, ...){
  res <-  lapply(x, function(gh){
    gh_pop_get_stats(gh, ...)

  })
  rbindlist(res, idcol = "sample")
}

#' @export
#' @rdname gs_pop_get_stats
#' @param nodes the character vector specifies the populations of interest. default is all available nodes
#' @param type the character vector specifies the type of pop stats or
#'          a function used to compute population stats.
#'          when character, it is expected to be either "count" or "percent". Default is "count" (total number of events in the populations).
#'          when a function,  it takes a flowFrame object through 'fr' argument and return the stats as a named vector.
#' @param inverse.transform logical flag . Whether inverse transform the data before computing the stats.
#' @param stats.fun.arg a list of arguments passed to `type` when 'type' is a function.
getStats.GatingHierarchy <- function(...){
  .Deprecated("gh_pop_get_stats")
  gh_pop_get_stats(...)
}
#' @param xml whether to extract xml stats or openCyto stats
#' @export
#' @rdname gs_pop_get_stats
gh_pop_get_stats <- function(x, nodes = NULL, type = "count", xml = FALSE, inverse.transform = FALSE, stats.fun.arg = list(), ...){
  gh <- x
  if(is.null(nodes))
    nodes <- gs_get_pop_paths(gh, ...)
  res <- sapply(nodes, function(node){
    if(is.character(type))
    {
      type <- match.arg(type, c("count", "percent"))
	  stats<-.getPopStat(x,y = node)
	  source <- ifelse(xml, "xml", "openCyto")
	
	  res <- stats[[source]][type]
      
        names(res) <- type
      
    }else{
      fr <- gh_pop_get_data(gh, y = node)
      if(inverse.transform)
      {
        trans <- gh_get_transformations(gh, inverse = TRUE)
        if(length(trans)==0)
          stop("No inverse transformation is found from the GatingSet!")
        trans <- transformList(names(trans), trans)
        fr <- transform(fr, trans)
      }
      thisCall <- quote(type(fr))
      thisCall <- as.call(c(as.list(thisCall), stats.fun.arg))
      
      res <- eval(thisCall)
    }

    as.data.table(t(res))
  }, simplify = FALSE)
  rbindlist(res, idcol = "pop")

}

#' built-in stats functions.
#'
#' pop.MFI computes and returns the median fluorescence intensity for each marker.
#' They are typically used as the arguments passed to \code{gh_pop_get_stats} method to perform the sample-wise population stats calculations.
#'
#' @param fr a flowFrame represents a gated population
#' @return a named numeric vector
#'
#' @rdname stats.fun
#' @export
#' @importFrom  matrixStats colMedians
pop.MFI <- function(fr){
  pd <- pData(parameters(fr))
  pd <- data.table(pd)
  pd <- pd[!is.na(desc), ]
  chnls  <- pd[, name]
  markers <- pd[, desc]

  res <- colMedians(exprs(fr)[, chnls, drop = FALSE])
  names(res) <- markers
  res
}

#' @templateVar old getProp
#' @templateVar new gh_pop_get_proportion
#' @template template-depr_pkg
NULL
#' Get count or proportion from populations
#' @param x GatingHierarchy
#' @param y \code{character} node name or path
#' @rdname gh_pop_get_proportion
#' @aliases getProp
#' @export
getProp <- function(x,y,xml = FALSE){
	.Deprecated("gh_pop_get_proportion")
	gh_pop_get_proportion(x, y, xml)
}
#' @param xml whether to extract xml stats or openCyto stats
#' @rdname gh_pop_get_proportion
#' @export
gh_pop_get_proportion <- function(x,y,xml = FALSE){
	gh_pop_get_stats(x, y, xml = xml, type = "percent")[, percent]
}

#' @templateVar old getTotal
#' @templateVar new gh_pop_get_count
#' @template template-depr_pkg
NULL
#' @rdname gh_pop_get_proportion
#' @export
#' @aliases getProp
getTotal <- function(x,y,xml = FALSE){
	.Deprecated("gh_pop_get_count")
	gh_pop_get_count(x, y, xml)
}

#' @rdname gh_pop_get_proportion
#' @export
gh_pop_get_count <- function(x,y,xml = FALSE){
	gh_pop_get_stats(x, y, xml = xml, type = "count")[, count]
	
}


.getPopStat<-function(x,y){
	stopifnot(!missing(y))
	
	
	stats<-.cpp_getPopStats(x@pointer,sampleNames(x), y)
	
	
	parent<-try(gs_pop_get_parent(x, y),silent=T)
	
	
	if(class(parent)=="try-error")#if parent exist
		pstats<-stats
	else
	{
		
		pstats<-.cpp_getPopStats(x@pointer,sampleNames(x), parent)
	}
	
	
#	browser()
	list(openCyto=c(percent=as.numeric(ifelse(pstats$FlowCore["count"]==0
									,0
									,stats$FlowCore["count"]/pstats$FlowCore["count"]
							))
					,count=as.numeric(stats$FlowCore["count"]))
			,xml=c(percent=as.numeric(ifelse(pstats$FlowJo["count"]==0
									,0
									,stats$FlowJo["count"]/pstats$FlowJo["count"]
							))
					,count=as.numeric(stats$FlowJo["count"]))
	)
}
#' @templateVar old getPopStats
#' @templateVar new gs(/gh)_pop_get_stats
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getPopStats",function(x,...)standardGeneric("getPopStats"))
#' @rdname gh_pop_compare_stats
#' @export
setMethod("getPopStats","GatingHierarchy",function(x, path = "auto", ...){
			.Deprecated("gh_pop_compare_stats")
			gh_pop_compare_stats(x, path, ...)
		})
#' Compare the stats(count/freq) between the version parsed from xml and the one recalculated/gated from R
#'
#' @param x GatingHierarchy
#' @param path see \link{gs_get_pop_paths}
#' @param ... not used
#' @rdname gh_pop_compare_stats
#' @export
gh_pop_compare_stats <- function(x, path = "auto", ...){
	
	nodePath <- gs_get_pop_paths(x, path = path, ...)
	stats <- rbindlist(lapply(nodePath, function(thisPath){
						curStats <- .getPopStat(x,thisPath)
						data.table(openCyto.freq = curStats$openCyto["percent"]
								,xml.freq = curStats$xml["percent"]
								,openCyto.count = curStats$openCyto["count"]
								,xml.count = curStats$xml["count"]
								, node = thisPath
						
						)
					})
	)
	
	rownames(stats) <- stats[, node]
	stats
}

.computeCV_gh <- function(gh, ...){
	
	x<-gh_pop_compare_stats(gh, ...)
	rn<-rownames(x)
	x<-as.data.frame(x)
	rownames(x)<-rn
	cv<-apply(as.matrix(x[,c("xml.count","openCyto.count")]),1,function(y)IQR(y)/median(y));
	cv<-as.matrix(cv,nrow=length(cv))
	cv[is.nan(cv)]<-0
	rownames(cv) <- as.character(rownames(x))
	cv
}

#' @importFrom lattice barchart
#' @export
#' @rdname gh_pop_compare_stats
#' @aliases gh_plot_pop_count_cv
gh_plot_pop_count_cv <- function(x, path = "auto", ...){
	cv <- .computeCV_gh(x, path = path)
	return(barchart(cv,xlab="Coefficient of Variation",..., par.settings=ggplot2like));
}

