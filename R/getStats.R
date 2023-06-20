#' @templateVar old getStats
#' @templateVar new gs(/gh)_pop_get_stats
#' @template template-depr_pkg
NULL

#' @export
getStats <- function(x, ...)UseMethod("getStats")

#' @export
getStats.GatingSetList <- function(x, ...){
  getStats.GatingSet(x, ...)
}

#' @export
getStats.GatingSet <- function(...){
	.Deprecated("gs_pop_get_stats")
  gs_pop_get_stats(...)
}
#' Extract stats from populations(or nodes)
#'
#' @name gs_pop_get_stats
#' @param x a GatingSet or GatingHierarchy
#' @param ... arguments passed to \link{gs_get_pop_paths} method.
#' @return a data.table that contains stats values (if MFI, for each marker per column)
#'  along with 'pop' column and 'sample' column (when used on a 'GatingSet')
#' @import flowCore
#' @import ncdfFlow
#' @import data.table
#' @export
#' @aliases gh_pop_get_stats getStats getStats,GatingSet-method getStats,GatingHierarchy-method
#' getStats,GatingSetList-method
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
#' @export
gs_pop_get_stats <- function(x, ...){
  res <-  lapply(x, function(gh){
    gh_pop_get_stats(gh, ...)

  })
  rbindlist(res, idcol = "sample")
}

#' @export
getStats.GatingHierarchy <- function(...){
  .Deprecated("gh_pop_get_stats")
  gh_pop_get_stats(...)
}

#' @param nodes the character vector specifies the populations of interest. default is all available nodes
#' @param type the character vector specifies the type of pop stats or
#'          a function used to compute population stats.
#'          when character, it is expected to be either "count" or "percent". Default is "count" (total number of events in the populations).
#'          when a function,  it takes a flowFrame object through 'fr' argument and return the stats as a named vector.
#' @param inverse.transform logical flag . Whether inverse transform the data before computing the stats.
#' @param stats.fun.arg a list of arguments passed to `type` when 'type' is a function.
#' @param xml whether to extract xml stats or openCyto stats
#' @rdname gs_pop_get_stats
#' @export
gh_pop_get_stats <- function(x, nodes = NULL, type = "count", xml = FALSE, inverse.transform = FALSE, stats.fun.arg = list(), ...){
  gh <- x
  if(is.null(nodes))
    nodes <- gs_get_pop_paths(gh, ...)
  res <- sapply(nodes, function(node){
    if(is.character(type))
    {
      type <- match.arg(tolower(type), c("count", "percent"))
	  stats<-.getPopStat(x,y = node)
	  source <- ifelse(xml, "xml", "openCyto")
	
	  res <- stats[[source]][type]
      
        names(res) <- type
      
    }else{
      fr <- gh_pop_get_data(gh, y = node, returnType = "flowFrame")
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

#' Extract stats from populations(or nodes) within a restricted time window
#' 
#' @rdname gs_pop_get_stats_tfilter
#' @param x GatingSet or GatingHierarchy
#' @param nodes the character vector specifies the populations of interest. default is all available nodes
#' @param type the character vector specifies the type of pop stats or
#'          a function used to compute population stats.
#'          When it is a character, it is expected to be either "Count" or "Frequency". Default is "Count" (total number of events in the populations).
#'          When it is a function,  it takes a flowFrame object through the 'fr' argument and returns the stats as a named vector.
#' @param inverse.transform logical flag . Whether to inverse transform the data before computing the stats.
#' @param stats.fun.arg a list of arguments passed to `type` when 'type' is a function.
#' @param tfilter Either a list (tmin, tmax) specifying the minimum and maximum of a the time window filter 
#'                or a GatingHierarchy, whose minimum and maximum time will be used to determine the window.
#'                For both x and the reference GatingHierarchy in tfilter, the only channels
#'                that will match this filter are "Time" or "time" and the filter will be applied to each event
#'                such that only events with time value t where tmin <= t <= tmax will be evaluated. 
#' @export
gs_pop_get_stats_tfilter <- function(x, ...){
  res <-  lapply(x, function(gh){
    gh_pop_get_stats_tfilter(gh, ...)
    
  })
  rbindlist(res)
}

#' @rdname gs_pop_get_stats_tfilter
#' @param path,... arguments passed to 'gh_get_pop_paths()'
#' @export
gh_pop_get_stats_tfilter <- function(x, nodes = NULL, type = c("Count", "Frequency"), inverse.transform = FALSE, stats.fun.arg = list(), 
                                           tfilter=NULL , path = c("full","auto"), ...){
  if (is(tfilter, "GatingHierarchy")) {
    time_channel <- grep("^[Tt]ime$", colnames(tfilter), value=TRUE)
    time <- exprs(gh_pop_get_data(tfilter))[,"Time"]
    tfilter <- list(min(time), max(time))
  } else if (!(is(tfilter, "list") && length(tfilter == 2))) {
    stop("tfilter must be either a GatingHierarchy or a list with 2 time values: (tmin, tmax)!")
  }
  gh <- x
  path <- match.arg(path, c("full", "auto"))
  if (is.null(nodes))
    nodes <- gh_get_pop_paths(gh, path=path, ...)
  time_channel <- grep("^[Tt]ime$", colnames(gh), value=TRUE)
  if (is.character(type))
  {
    type <- match.arg(type, c("Count", "Frequency"))
    time <- exprs(gh_pop_get_data(gh))[,time_channel]
    tfilt <- time >= tfilter[[1]] & time <= tfilter[[2]]
    in_gates <- gh_pop_get_indices_mat(gh, nodes)
    
    res <- colSums(sweep(in_gates, 1, tfilt, '&'))
    if(type == "Frequency"){
      # nodes[[1]] should be "root", but allow flexibility
      res <- res / res[[nodes[[1]]]]
    }
    res <- as.data.table(res)
    names(res) <- type
    res <- cbind(Population=nodes, res)
    
    # Add extra columns relating to parent to match output of gs_pop_get_count_fast
    parent_cols <- lapply(nodes[-1], function(node){
                      parent = gh_pop_get_parent(x, node, path=path)
                      parent_stat = res[Population == parent][[type]]
                      data.frame(Parent=parent, ParentStat=parent_stat, stringsAsFactors = FALSE)
                    })
    
    parent_cols <- do.call(rbind, parent_cols)
    names(parent_cols)[[2]] <- paste0("Parent", type)
    # Add sampleName and reorder columns to match gs_pop_get_count_fast
    res <- cbind(name = sampleNames(x), res[-1], parent_cols)[, c(1,2,4,3,5)]
    
  } else {
    # Same as for gh_pop_get_stats, but with time filtering after transformation
    # In this case, make column names match gh_pop_get_stats
    res <- sapply(nodes, function(node){
      fr <- gh_pop_get_data(gh, y = node)
      if(inverse.transform)
      {
        trans <- gh_get_transformations(gh, inverse = TRUE)
        if(length(trans)==0)
          stop("No inverse transformation is found from the GatingSet!")
        trans <- transformList(names(trans), trans)
        fr <- transform(fr, trans)
      }
      time <- exprs(fr)[,time_channel]
      tfilt <- time >= tfilter[[1]] & time <= tfilter[[2]]
      fr <- Subset(fr, tfilt)
      
      thisCall <- quote(type(fr))
      thisCall <- as.call(c(as.list(thisCall), stats.fun.arg))
      
      res <- eval(thisCall)
      as.data.table(t(res))
    }, simplify = FALSE)
    res <- rbindlist(res, idcol = "pop")
    res <- cbind(sample = sampleNames(gh), res)
  }
  res
}


#' built-in stats functions.
#'
#' pop.MFI computes and returns the median fluorescence intensity for each marker.
#' They are typically used as the arguments passed to \code{gh_pop_get_stats} method to perform the sample-wise population stats calculations.
#' 
#' @name stats.fun
#' @aliases pop.MFI
#' @param fr a flowFrame represents a gated population
#' @return a named numeric vector
#'
#' @importFrom  matrixStats colMedians
#' @export
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

#' @export
getProp <- function(x,y,xml = FALSE){
	.Deprecated("gh_pop_get_proportion")
	gh_pop_get_proportion(x, y, xml)
}
#' Get count or proportion from populations
#' @param x GatingHierarchy
#' @param y \code{character} node name or path
#' @param xml whether to extract xml stats or openCyto stats
#' @name gh_pop_get_proportion
#' @aliases getProp getTotal
#' @export
gh_pop_get_proportion <- function(x,y,xml = FALSE){
	gh_pop_get_stats(x, y, xml = xml, type = "percent")[, percent]
}

#' @templateVar old getTotal
#' @templateVar new gh_pop_get_count
#' @template template-depr_pkg
NULL

#' @export
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
	
	
	stats<-cpp_getPopStats(x@pointer,sampleNames(x), y)
	
	
	parent<-try(gs_pop_get_parent(x, y),silent=T)
	
	
	if(class(parent)=="try-error")#if parent exist
		pstats<-stats
	else
	{
		
		pstats<-cpp_getPopStats(x@pointer,sampleNames(x), parent)
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

#' @export
setMethod("getPopStats","GatingHierarchy",function(x, path = "auto", ...){
			.Deprecated("gh_pop_compare_stats")
			gh_pop_compare_stats(x, path, ...)
		})

#' Compare the stats(count/freq) between the version parsed from xml and the one recalculated/gated from R
#' 
#' @name gh_pop_compare_stats
#' @aliases getPopStats getPopStats,GatingHierarchy-method
#' @param x GatingHierarchy
#' @param path see \link{gs_get_pop_paths}
#' @param ... not used
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

#' @rdname gs_plot_pop_count_cv
#' @aliases gh_plot_pop_count_cv
#' @export
gh_plot_pop_count_cv <- function(x, path = "auto", ...){
	cv <- .computeCV_gh(x, path = path)
	return(barchart(cv,xlab="Coefficient of Variation",..., par.settings=ggplot2like));
}

