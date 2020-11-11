#' Extract stats from populations(or nodes)
#'
#' @name gs_pop_stats_print
#' @param gs a GatingSet 
#' @param gh a GatingHierarchy
#' @param ... arguments passed to \link{gs_get_pop_paths} method.
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
#' @rdname gs_pop_stats
#' @export
gs_pop_stats_print <- function(gs, ...){
  res <-  lapply(gs, function(gh){
    gh_pop_stats_print(gh, ...)

  })
  rbindlist(res, idcol = "sample")
}


#' @param nodes the character vector specifies the populations of interest. default is all available nodes
#' @param type the character vector specifies the type of pop stats or
#'          a function used to compute population stats.
#'          when character, it is expected to be either "count" or "percent". Default is "count" (total number of events in the populations).
#'          when a function,  it takes a flowFrame object through 'fr' argument and return the stats as a named vector.
#' @param inverse.transform logical flag . Whether inverse transform the data before computing the stats.
#' @param stats.fun.arg a list of arguments passed to `type` when 'type' is a function.
#' @param xml whether to extract xml stats or openCyto stats
#' @rdname gs_pop_stats
#' @export
gh_pop_stats_print <- function(gh, nodes = NULL, type = "Count", xml = FALSE, inverse.transform = FALSE, stats.fun.arg = list(), ...){
  if(is.character(type))##buildin stats
  {
    type <- match.arg(type, cyto_stats_supported())
    
    stats<- gh_pop_stats_compare(gh, nodes = nodes, type = type, ...)
    keep.val <- ifelse(xml, "xml", "cytolib")
    stats  %>%
      rename(pop := node) %>%
      mutate(value = stats[[keep.val]])%>% 
      select(-contains(c("type", "cytolib", "xml")))
    
  }else##custom stats func
  {
    if(is.null(nodes))
      nodes <- gs_get_pop_paths(gh, ...)
    
    res <- sapply(nodes, function(node){
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
     
  
      as.data.table(t(res))
    }, simplify = FALSE)
    rbindlist(res, idcol = "pop")
    
  }

}

#' @rdname gs_pop_stats
#' @export
gh_pop_get_stats <- gh_pop_stats_print

#' @rdname gs_pop_stats
#' @export
gs_pop_get_stats <- gs_pop_stats_print

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


#' Get count or proportion from populations
#' @param x GatingHierarchy
#' @param y \code{character} node name or path
#' @param xml whether to extract xml stats or openCyto stats
#' @name gh_pop_get_proportion
#' @export
gh_pop_get_proportion <- function(x,y,xml = FALSE){
  gh_pop_get_count(x, y, xml = xml)/gh_pop_get_count(x, gh_pop_get_parent(x, y), xml)
}

#' @rdname gh_pop_get_proportion
#' @export
gh_pop_get_count <- function(x,y,xml = FALSE){
	gh_pop_get_stats(x, y, xml = xml, type = "Count")[, value]
	
}


.getPopStat<-function(x,y){
	stopifnot(!missing(y))
	
	
	stats<-.cpp_getPopStats(x@pointer,sampleNames(x), y)
	stats
}

#' Compare the stats(count/freq) between the version parsed from xml and the one recalculated/gated from R
#' 
#' @param path see \link{gs_get_pop_paths}
#' @param ... not used
#' @export
#' @rdname gs_pop_stats
#' @importFrom dplyr filter tibble rename rename_if select
#' @importFrom tidyselect contains
gh_pop_stats_compare <- function(gh, nodes = NULL, path = "auto", type = c("Count"), legacy = FALSE, ...){
	if(is.null(nodes))
	  nodes <- gs_get_pop_paths(gh, path = path, ...)
	stats <- do.call(rbind, lapply(nodes, function(thisPath){
              					 .getPopStat(gh,thisPath) %>% 
	                                            tibble() %>%
              					                      filter(type %in% !!type) %>%
	                                            mutate(node = thisPath)
              					})
	)
	
	## format it according to its type
	col.show <- c("cytolib", "xml", "node")
	
	no.attr <- all(type %in% c("Count", "Freqofparent", "Freqofgrandparent", "FreqofTotal"))
	if(!no.attr)
	  col.show <- c("attr", col.show)
	
	if(type == "Freqof")
	  col.attr <- "ancestor"
	else
	  col.attr <- "channel"
	if(type == "Percentile")
	  col.show <- c("type", "key", col.show)
	else
	  col.show <- c("type", col.show)
	stats <- select(stats, col.show)
	if(!no.attr)
	  stats <- rename(stats, !!col.attr := attr)
	
	if(type == "Percentile")#parse out percent info from key
	  stats <- mutate(stats, key = paste0(strsplit(key, " ")[[1]][3], "%")) %>% rename(percent := key)
	##convert to legacy format for backward compatibility
	if(legacy)
	{
	  stopifnot(length(type)==1)
	  
	  stats <- select(stats, -contains("type")) %>% rename(!!paste0("openCyto.", tolower(type)) := cytolib
	                  , !!paste0("xml.", tolower(type)) := xml)   
	}
  
	  
	stats%>%
	  data.table()
}
#' @rdname gs_pop_stats
#' @export
gh_pop_compare_stats <- function(...){
  .Deprecated("gh_pop_stats_compare")
  gh_pop_stats_compare(...)
}

.computeCV_gh <- function(gh, ...){
	
	x<-gh_pop_stats_compare(gh, legacy = TRUE,...)
	rn<-x[["node"]]
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

#' Compute all the stats
#' 
#' To save time, some channel-specific stats such as mean, SD are not computed automatically at the time when they are added to GatingSet.
#' This function allows users to invoke this computation manually for the given population node and all its descendants.
#' 
#' 
#' @rdname gs_pop_stats
#' @export
gh_pop_stats_compute <- function(gh, node = "root")
{
  gh_compute_stats(gh@pointer, sampleNames(gh), node)
}

#' @rdname gs_pop_stats
#' @export
gs_pop_stats_compute <- function(gs, node = "root")
{
  for(sn in sampleNames(gs))
    gh_compute_stats(gs@pointer, sn, node)
}

#' list the stats type available in the gating tree
#' 
#' @export
#' @rdname gs_pop_stats
gh_pop_stats_ls <- function(gh, node = NULL, ...){
  if(is.null(node))
    gh_ls_stats(gh@pointer, sampleNames(gh))
  else
    gh_ls_pop_stats(gh@pointer, sampleNames(gh), node)
}

#' @export
#' @rdname gs_pop_stats
gs_pop_stats_ls <- function(gs, ...){
  unique(unlist(lapply(gs, gh_pop_stats_ls, ...)))
}

is_empty_val <- function(x)
{
  if(is.character(x))
    x==""
  else
    length(x) == 0
}



#' @export
cyto_stats_supported <- function(){
  unique(unlist(options("cyto_stats_type"), use.names = FALSE))
}

#' constructor of cyto_stats
#' 
#' @param type character stats type. type 'cyto_stats_supported()' for all the supported stats types
#' @param channel character channel info 
#' @param ancestor character ancestor population . only valid for 'Freqof' stats
#' @param percent numeric only valid for 'Percentile' stats
#' @export
cyto_stats <- function(type,  channel = "", ancestor = "", percent = "")
{
  res <- lapply(type, function(tp){
    lapply(channel, function(ch){
      lapply(ancestor, function(an){
        lapply(percent, function(pc){
          .cyto_stats(tp, ch, an, pc)
        })
      })
    })
  })
  
  for(i in 1:3)
    res <- unlist(res, FALSE)
  res
}

.cyto_stats <- function(type,  channel = "", ancestor = "", percent = "")
{
  type_basic <- getOption("cyto_stats_type")[["basic"]]
  type_with_ancestor <- getOption("cyto_stats_type")[["with_ancestor"]]
  type_with_channel <- getOption("cyto_stats_type")[["with_channel"]]
  type_with_percent <- getOption("cyto_stats_type")[["with_percent"]]
  type <- match.arg(type, c(type_basic, type_with_ancestor, type_with_channel))
  ##validity checks
  if(type %in% type_basic)  
  {
    if(!is_empty_val(channel))
      stop("channel argument is not applicable for ", type)
    if(!is_empty_val(ancestor))
      stop("ancestor argument is not applicable for ", type)
    if(!is_empty_val(percent))
      stop("percent argument is not applicable for ", type)
    
    res <- list(type = type)
    class(res) <- "cyto_stats_basic"
    
  }else if(type %in% type_with_channel)  
  {
    if(is_empty_val(channel))
      stop("channels argument is required for ", type)
    if(!is_empty_val(ancestor))
      stop("ancestor argument is not applicable for ", type)
    
    res <- list(type = type, attr = channel)
    class(res) <- "cyto_stats_channel"
    if(type %in% type_with_percent)
    {
      if(is_empty_val(percent))
        stop("percent argument is required for ", type)
      stopifnot(is.numeric(percent))
      res[["percent"]] <- percent
      class(res) <- "cyto_stats_percent"
    }else
    {
      if(!is_empty_val(percent))
        stop("percent argument is not applicable for ", type)  
    }
    
  }else if(type %in% type_with_ancestor)  
  {
    if(!is_empty_val(channel))
      stop("channel argument is not applicable for ", type)
    if(is_empty_val(ancestor))
      stop("ancestor argument is required for ", type)
    if(!is_empty_val(percent))
      stop("percent argument is not applicable for ", type)
    
    res <- list(type = type, attr = ancestor)
    class(res) <- "cyto_stats_ancestor"
  }
  
  class(res) <- c("cyto_stats", class(res))
  res
}

#somehow roxygen doesn't export s3 method automatically (may be due to the print generic got changed into s4 generic at some point), current it is manually added to NAMESPACE file
#' @export print.cyto_stats
#' @export
#' @importFrom crayon bold green cyan italic
print.cyto_stats <- function(x, ...){
  # is(x, "cyto_stats")
  cat(bold(green("cyto_stats \n")))
  cat(bold(green("type: ")), italic(cyan(x[["type"]])), "\n")
  if(!is.null(x[["attr"]]))
    cat(bold(green("attr: ")), italic(cyan(x[["attr"]])), "\n")
}

#' @export
#' @rdname gs_pop_stats
gh_pop_stats_add <- function(gh, nodes = NULL, stats){
  if(is(stats, "cyto_stats"))
    stats <- list(stats)
  if(any(sapply(stats, function(stat)!is(stat, "cyto_stats"))))
    stop("stat must be a valid 'cyto_stats' object generated by 'cyto_stats()' constuctor")
  if(is.null(nodes))
    nodes <- gh_get_pop_paths(gh)
  gh_add_stats(gh@pointer, sampleNames(gh), nodes, stats)
}

#' @export
#' @rdname gs_pop_stats
gs_pop_stats_add <- function(gs, ...){
  invisible(lapply(gs, gs_pop_stats_add, ...))
}


#' @export
#' @rdname gs_pop_stats
gh_pop_stats_remove <- function(gh, nodes = NULL, stats){
  if(is(stats, "cyto_stats"))
    stats <- list(stats)
  if(any(sapply(stats, function(stat)!is(stat, "cyto_stats"))))
    stop("stat must be a valid 'cyto_stats' object generated by 'cyto_stats()' constuctor")
  if(is.null(nodes))
    nodes <- gh_get_pop_paths(gh)
  gh_remove_stats(gh@pointer, sampleNames(gh), nodes, stats)
}

#' @export
#' @rdname gs_pop_stats
gs_pop_stats_remove <- function(gs, ...){
  invisible(lapply(gs, gh_pop_stats_remove, ...))
}
