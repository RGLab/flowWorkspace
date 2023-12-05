#' @include GatingHierarchy_Methods.R
NULL
#' @importClassesFrom methods ANY character data.frame environment list logical matrix missing numeric oldClass
#' @importMethodsFrom methods coerce show
NULL

#' subset gs by population node
#' 
#' Basically it returns a new GatingSet with only the substree of the given population node
#' @param gs GatingSet
#' @param pop the population node that will become the new root node
#' @return a new GatingSet that share the underlying events data
#' @export
gs_pop_get_gs <- function(gs, pop){
  gs1 <- gs_copy_tree_only(gs)
  cs <- gs_pop_get_data(gs1, pop)
  # # check if pop is child of root
  # if(gs_pop_get_parent(gs1, pop) == "root")
  # {
  #   stop("Can't ")
  # }
  if(pop != "root")
  {
    nodes.first.level <- gs_pop_get_children(gs1, "root")
  
    nodes.tomv <- gs_pop_get_children(gs1, pop)
    for(node in nodes.tomv)
      for(i in seq_len(length(gs1)))
      {
        gh_pop_move(gs1[[i]], node, "root", recompute = FALSE)
      }
    #purge the rest ancestors and siblings
    for(node in nodes.first.level)
      gs_pop_remove(gs1, node)
    
    gs_cyto_data(gs1) <- cs
  }
  recompute(gs1)#update cell indicies even stats remain unchanged
  gs1
}
#' @templateVar old isNcdf
#' @templateVar new gs_is_h5
#' @template template-depr_pkg
NULL
#' determine whether the flow data associated with a GatingSet is persistent(on-disk) or in-memory
#'
#' @param x \code{GatingSet} object
#' @return \code{logical}
#' @export
#' @rdname gs_is_persistent
gs_is_persistent <- function(x){
  return (cs_get_uri(gs_cyto_data(x))!="")

}

#' @export
#' @rdname gs_is_persistent
gs_is_h5 <- function(x){
	.Deprecated("gs_is_persistent")
	
}

#' @export
#' @rdname gs_is_persistent
isNcdf <- function(x){
  .Deprecated("gs_is_h5")
  gs_is_h5(x)
  }

setMethod("nrow",
          signature=signature(x="GatingSet"),
          definition=function(x)
            nrow(gs_cyto_data(x))
)

setMethod("Subset",
          signature=signature(x="GatingSet",
                              subset="ANY"),
          definition=function(x, subset, ...)
          {
            cs <- gs_cyto_data(x)
            cs <- Subset(cs, subset, ...)
            gs_cyto_data(x) <- cs
            recompute(x)
            x
          })

#' @export
#' @rdname cs_get_uri  
gs_get_uri <- function(x){
  cs_get_uri(x)
}

#' @export
gs_get_cytoframe <- function(x, ...){
  cs_get_cytoframe(gs_cyto_data(x), ...)
}

#' @export
setMethod("GatingSet", c("GatingHierarchy", "character"), function(x, y, path="."
																	, ...){
            .Deprecated("gh_apply_to_new_fcs")
			samples <- y
			dataPaths <- vector("character")
			excludefiles <- vector("logical")
			for(file in samples){
#				browser()
				#########################################################
				#get full path for each fcs and store in dataPath slot
				#########################################################
				##escape "illegal" characters
				file<-gsub("\\)","\\\\)",gsub("\\(","\\\\(",file))
				absPath<-list.files(pattern=paste("^",file,"$",sep=""),path=path,recursive=TRUE,full.names=TRUE)

				if(length(absPath)==0){
					warning("Can't find ",file," in directory: ",path,"\n");
					excludefiles<-c(excludefiles,TRUE);

				}else{
					dataPaths<-c(dataPaths,dirname(absPath[1]))
					excludefiles<-c(excludefiles,FALSE);
				}
			}
			#Remove samples where files don't exist.
			if(length(which(excludefiles))>0){
				message("Removing ",length(which(excludefiles))," samples from the analysis since we can't find their FCS files.");
				samples<-samples[!excludefiles];
			}


			files<-file.path(dataPaths,samples)
			gh_apply_to_new_fcs(x, files, ...)	
		})

#' Construct a \code{GatingSet} using a template
#'
#' This uses a \code{\link{GatingHierarchy}} as a template to apply to other loaded samples in the form of a \code{\link{cytoset}},
#' resulting in a \code{\link{GatingSet}}. The transformations and gates from the template are applied to all samples. The compensation
#' applied to each of the samples can be controlled via the \code{compensation_source} argument.
#'  
#' @name gh_apply_to_cs
#' @aliases GatingSet,GatingHierarchy,character-method
#' @param x GatingHierarchy
#' @param cs a cytoset
#' @param ... not currently used
#' @param swap_cols for internal usage
#' @param compensation_source One of the following options:
#' \itemize{
#'   \item "sample" -- each cytoframe will be compensated with the spillover matrix included in its own FCS
#'   \item "template" -- all cytoframes will be compensatied with the spillover matrix of the template GatingHierarchy
#'   \item "none" -- no compensation will be applied
#' }
#' @return a \code{GatingSet} 
#' @export
gh_apply_to_cs <- function(x, cs
									              , swap_cols = FALSE #for diva parsing
									              , compensation_source = "sample"
									              , ...){	
      
			compensation_source <- match.arg(compensation_source, c("sample", "template", "none"))
			message("generating new GatingSet from the gating template...")
			
			cols.old <- colnames(cs)
			cols <- swap_data_cols(cols.old, swap_cols)#validity check
			if(!all(cols==cols.old))
			{
				#can't assign cols directly due to the backend implemented as std::unordered_map
				for(c1 in names(swap_cols))
				{
					c2 <- swap_cols[[c1]]
					cs_swap_colnames(cs, c1, c2)					
				}
				
			}
			execute_in_c <- length(x@transformation) == 0
			gs <- new("GatingSet", pointer = cpp_NewGatingSet(x@pointer,sampleNames(x), cs@pointer, execute_in_c, compensation_source))
			#deal with the trans that are not stored in c++
			if(!execute_in_c)
			{
			  if(compensation_source == "template")
			    compensate(gs, gh_get_compensations(x))#setting comp is redundant(since it is already copied) but it is lightweight so should fine
			  else if(compensation_source == "sample"){
			    # Could end up with NULLs if not using proper SPILL keyword
			    comp <- lapply(seq_along(cs), function(idx){
			      cf <- cs[[idx]]
			      spills <- spillover(cf)
			      # Make search order match cytolib::CytoFrame::get_compensation
			      spills <- spills[c("$SPILLOVER", "SPILL", "spillover")]
			      found <- !sapply(spills, is.null)
			      if(!any(found))
			        stop("No spillover matrix found for sample: ", sampleNames(cs)[idx])
			      # return first found in search order
			      spills[[min(which(found))]]
			    })
			    names(comp) <- sampleNames(cs)
			    compensate(gs, comp)
			  }
			  #post transform the data and copy over the R trans to new gs
			  #because c++ code only compensate but doesn't transform data
			  gs <- transform(gs, x@transformation[[1]])
			  
			  recompute(gs)
			}	
			
			return(gs)
}

#' Construct a \code{GatingSet} using a template and FCS files
#' 
#' This uses a \code{\link{GatingHierarchy}} as a template to apply to other loaded samples in the form of a list of FCS files,
#' resulting in a \code{\link{GatingSet}}. The transformations and gates from the template are applied to all samples.
#' 
#' This method is still included to support legacy scripts but will deprecated for the more modular workflow of loading a \code{\link{cytoset}}
#' via \code{\link{load_cytoset_from_fcs}} followed by \code{\link{gh_apply_to_cs}}.
#' 
#' @inheritParams gh_apply_to_cs
#' @param backend the backend storage mode to use for \code{\link{load_cytoset_from_fcs}}
#' @param ... other arguments passed to \code{\link{load_cytoset_from_fcs}}
#' @export
gh_apply_to_new_fcs <- function(x, files
                                , swap_cols = FALSE #for diva parsing
                                , backend = get_default_backend()
                                , compensation_source = "sample"
                                , ...){
  .Deprecated("gh_apply_to_cs")
  cs <- load_cytoset_from_fcs(files, backend = backend, ...)
  gh_apply_to_cs(x, cs, swap_cols = swap_cols, compensation_source = compensation_source) #for diva parsing
}

#' Swap the colnames
#' Perform some validity checks before returning the updated colnames
#'  
#' @param cols the original colname vector
#' @param swap_cols a named list specifying the pairs to be swapped
#' @return the new colname vector that has some colnames swapped
#' @export 
#' @examples 
#' library(flowCore)
#' data(GvHD)
#' fr <- GvHD[[1]]
#' colnames(fr)
#' new <- swap_data_cols(colnames(fr), list(`FSC-H` = "SSC-H", `FL2-H` = "FL2-A"))
#' colnames(fr) <- new
swap_data_cols <- function(cols, swap_cols)
{
	if(!is.null(swap_cols))
		if(!isFALSE(swap_cols) && length(swap_cols) > 0) 
		{
			left <- names(swap_cols)
			right <- as.vector(unlist(swap_cols))
			
			update <- FALSE
			for(left in names(swap_cols))
			{
				right <- swap_cols[[left]]
				
				lidx <- match(left, cols)
				ridx <- match(right, cols)
				
				if(!is.na(lidx) && !is.na(ridx))
				{
					if(length(lidx) > 1) 
						stop("Multiple cols matched to ", left)
					if(length(ridx) > 1)
						stop("Multiple cols matched to ", right)
					message("swap cols: ", left, ":", right)  
					
					cols[c(lidx, ridx)]  <- c(right, left)
				}
			}
		}
	cols
}


fix_y_axis <- function(gs, x, y){
	chnls <- colnames(gs_pop_get_data(gs))
	y.candidates <- chnls[-match(x,chnls)]
	
	if(y%in%y.candidates)
		yParam <- y
	else{
		if(!y %in% chnls)
			stop("default y '", y, "' is not valid channel name!Try to reset it")
		#pick other channel for y axis
		y.candidates <- y.candidates[!grepl("[Tt]ime", y.candidates)]
		yParam <- y.candidates[1]
		warning("Y axis is set to '", yParam, "' because default y '", y, "' can not be used as y axis!\n To eliminate this warning, change the default y channel")
	}
	return(yParam)
}


#' @rdname gs_clone
#' @param h5_dir h5 dir for the new gs
#' @export 
gs_clone <- function(x, h5_dir = tempdir()){
  new("GatingSet", pointer = cpp_CloneGatingSet(x@pointer, h5_dir, is_copy_data = TRUE))
  
}

#' @rdname gs_clone
#' @export 
gs_copy_tree_only <- function(x){
  new("GatingSet", pointer = cpp_CloneGatingSet(x@pointer, h5_dir = "", is_copy_data = FALSE))
  
}
#' @name recompute
#' @export
#' @param ... arguments
recompute <- function(x, y = "root", alwaysLoadData = FALSE, verbose = FALSE, leaf.bool = TRUE)UseMethod("recompute")
#' Compute the cell events by the gates stored within the gating tree.
#'
#' Compute each cell event to see if it falls into the gate stored within the gating tree
#' and store the result as cell count.
#'
#' It is usually used immediately after \link{add} or \link{gs_pop_set_gate} calls.
#' 
#' @name recompute
#' @param x \code{GatingSet or GatingSetList}
#' @param y \code{character} node name or node path. Default "root". Optional.
#' @param alwaysLoadData \code{logical}. Specifies whether to load the flow raw data for gating boolean gates. Default 'FALSE'. Optional. Sometime it is more efficient to skip loading the raw data if all the reference nodes and parent are already gated. 'FALSE' will check the parent node and reference to determine whether to load the data.
#' This check may not be sufficient since  the further upstream ancestor nodes may not be gated yet.
#' In that case, we allow the gating to fail and prompt user to recompute those nodes explictily.
#'  When TRUE, then it forces data to be loaded to guarantee the gating process to be uninterrupted at the cost of unnecessary data IO.
#' @param leaf.bool whether to compute the leaf boolean gate, default is TRUE
#' @param verbose default is FALSE
#' @export
recompute.GatingSet <- function(x,y = "root", alwaysLoadData = FALSE, verbose = FALSE, leaf.bool = TRUE){
  cpp_gating(x@pointer, y, alwaysLoadData, verbose, leaf.bool)
  message("done!")
  invisible()
}

#' apply \code{FUN} to each sample (i.e. \code{GatingHierarchy} or \code{cytoframe})
#' in a \code{GatingSet} or \code{cytoset}
#' 
#' sample names are used for names of the returned list
#'
#' @name lapply-methods
#' @aliases lapply lapply,GatingSet-method lapply,cytoset-method
#' @usage lapply(X, FUN, ...)
#' @param X \code{GatingSet} or \code{cytoset}
#' @param FUN \code{function} to be applied to each sample in 'GatingSet' or 'cytoset'
#' @param ... other arguments to be passed to 'FUN'
#' @export
setMethod("lapply","GatingSet",function(X,FUN,...){
      sapply(sampleNames(X),function(thisSample,...){
            gh <- X[[thisSample]]
            FUN(gh, ...)
          }, simplify = FALSE, ...)


    })



#' Get/update sample names in a GatingSet
#'
#' Return  a sample names contained in a GatingSet
#' 
#' @name sampleNames
#' @aliases sampleNames,GatingSet-method
#' sampleNames,cytoset-method sampleNames,cytoset,
#' @usage sampleNames(object)
#' @param object a \code{GatingSet}
#' 
#' @details
#' The sample names comes from pdata of fs.
#'
#' @return
#' A character vector of sample names
#'
#' @examples
#'       \dontrun{
#'         #G is  a GatingSet
#'         sampleNames(G)
#'       }
#' @export
setMethod("sampleNames","GatingSet",function(object){
      cpp_getSamples(object@pointer)
    })

#' @param value \code{character} new sample names
#' @usage sampleNames(object) <- value
#' @aliases
#' sampleNames<-
#' sampleNames<-,GatingSet-method
#' sampleNames<-,GatingSet,ANY-method
#' @importMethodsFrom Biobase sampleNames<-
#' @rdname sampleNames
#' @export
setReplaceMethod("sampleNames",
    signature=signature(object="GatingSet"),
    definition=function(object, value)
    {
      oldNames <- sampleNames(object)
      #update c++ data structure
      mapply(oldNames,value, FUN = function(oldName, newName){
            cpp_setSample( object@pointer, oldName, newName)
      })

      object
    })

# to speed up reading data from disk later on,
# we can optionally pass j to ncdfFlow::[ to subset on channel
#' @export
setMethod("getData",signature(obj="GatingSet",y="ANY"),function(obj,y, ...){
  .Deprecated("gs_pop_get_data")
  if(missing(y)){
    gs_pop_get_data(obj, ...)
  }else{
    gs_pop_get_data(obj, y, ...)
  }
})

#' @export
gs_pop_get_data <- function(obj, y = "root", inverse.transform = FALSE, ...){

	if(class(obj) == "GatingSetList")
	{
		samples_orig <- obj@samples
		if(missing(y))
			y <- NULL
		else if(!is.character(y))
			stop(" 'numeric` indexing is no longer safe . Please use node name instead!")        
		res <- lapply(obj,function(gs){
					
					if(is.null(y))
						ncfs <- gs_pop_get_data(gs,inverse.transform=inverse.transform, ...)
					else
						ncfs <- gs_pop_get_data(gs,y,inverse.transform=inverse.transform, ...)
					ncfs
				}, level =1)
		ncdfFlowList(res, samples_orig)
		
	}else
	{
		cs <- new("cytoset", pointer = get_cytoset_from_node(obj@pointer, y))
		if(inverse.transform)
		  cs <- transform(gs_cyto_data(gs_clone(cs)), gs_get_transformlists(obj, inverse = TRUE))
		
		cs[,...]
		
	  }

}

#' @templateVar old flowData
#' @templateVar new gs_cyto_data
#' @template template-depr_pkg
NULL
#' @export
setGeneric("flowData", function(x) standardGeneric("flowData"))

#' @templateVar old flowData<-
#' @templateVar new gs_cyto_data<-
#' @template template-depr_pkg
NULL

#' @export
setGeneric("flowData<-", function(x,value) standardGeneric("flowData<-"))


#' @export
setMethod("flowData",signature("GatingSet"),function(x){
  .Deprecated("gs_cyto_data")
  gs_cyto_data(obj)
  
})
#' Fetch or replace the flowData object associated with a GatingSet .
#'
#' Accessor method that gets or replaces the \code{\link{cytoset}}/\code{\link[flowCore]{flowSet}}/\code{\link[ncdfFlow:ncdfFlowSet-class]{ncdfFlowSet}} object in a GatingSet or GatingHierarchy
#' 
#' @name gs_cyto_data
#' @aliases flowData flowData<- gs_cyto_data<- flowData,GatingSet-method flowData<-,GatingSet-method
#' gs_cyto_data,GatingSet-method gs_cyto_data<-,GatingSet-method
#' @usage gs_cyto_data(x, ...)
#' @param x A \code{GatingSet}
#' @param ... other arugments
#'
#' @details Accessor method that sets or replaces the ncdfFlowSet object in the GatingSet or GatingHierarchy.
#'
#' @return the object with the new flowSet in place.
#'
#' @export
setGeneric("gs_cyto_data", function(x, ...) standardGeneric("gs_cyto_data"))

#' @name gs_cyto_data
#' @param inverse.transform logical flag indicating whether to inverse transform the data
#' @export
setMethod("gs_cyto_data",signature("GatingSet"),function(x, inverse.transform=FALSE){
	
	if(inverse.transform)
	{
	  data <- gs_cyto_data(gs_clone(x))#make a copy before transform to keep the original data intact
	  
    transform(data, gs_get_transformlists(x, inverse = inverse.transform))
	  
	}else
	  data <- new("cytoset", pointer = get_cytoset(x@pointer))
	
	
	data
})

gs_get_transformlists<- function(gs, inverse = FALSE){
  lapply(gs, function(gh){
    trans <- gh_get_transformations(gh, inverse=inverse)
    if(length(trans)==0)
      stop("No transformation is found from the GatingSet!")
    transformList(names(trans), trans)
  })
  
}
#' @export
setGeneric("gs_cyto_data<-", function(x,value) standardGeneric("gs_cyto_data<-"))

#' @export
setReplaceMethod("flowData",signature(x="GatingSet"),function(x,value){
    .Deprecated("gs_cyto_data<-")
    `gs_cyto_data<-`(x,value)
    })

#' @param value The replacement \code{flowSet} or \code{ncdfFlowSet} object
#' @usage gs_cyto_data(x) <- value
#' @rdname gs_cyto_data
#' @export
setReplaceMethod("gs_cyto_data",signature(x="GatingSet"),function(x,value){
      if(class(value) != "cytoset") {
        if(inherits(value, "flowSet")) {
          value <- flowSet_to_cytoset(value)
        } else {
          stop(
            "Data to replace in GatingSet must be either a flowSet or cytoset!"
          )
        }
      }
			set_cytoset(x@pointer, value@pointer)
			x
		})
#' read/set pData of flow data associated with \code{GatingHierarchy}, \code{GatingSet}, or \code{GatingSetList}
#'
#' Accessor method that gets or replaces the pData of the flowset/ncdfFlowSet object in a GatingHierarchy, GatingSet, or GatingSetList
#' @name pData-methods
#' @aliases pData pData,GatingHierarchy-method pData,GatingSet-method
#' @param object \code{GatingSet} or \code{GatingSetList}
#' @usage pData(object)
#' @return a \code{data.frame}
#'
#' @importFrom Biobase pData description exprs sampleNames pData<-
#'
#' @export
setMethod("pData","GatingSet",function(object){
			pData(gs_cyto_data(object))
		})

#' @param value \code{data.frame} The replacement of pData for \code{flowSet} or \code{ncdfFlowSet} object
#' @usage pData(object) <- value
#' @aliases
#' pData<-
#' pData<-,GatingSet,data.frame-method
#' pData<-,GatingSetList,data.frame-method
#' @export
#' @rdname pData-methods
setReplaceMethod("pData",c("GatingSet","data.frame"),function(object,value){

			fs <- gs_cyto_data(object)
            new.rownames <- rownames(value)
            if(is.null(new.rownames))
              new.rownames <- value[["name"]] #use name column when rownames are absent

            rownames(value) <- new.rownames

			pData(fs) <- value

			return (object)
		})

#' @description \code{[} subsets a \code{GatingSet} or \code{GatingSetList} using the familiar bracket notation
#'
#' @rdname brackets
#' @aliases  [ [,GatingSet,ANY-method [,GatingSetList,ANY-method
#' @export
setMethod("[",c("GatingSet"),function(x,i,j,...,drop){
#            browser()
      if(extends(class(i), "numeric")||class(i) == "logical"){
        i <- sampleNames(x)[i]
      }
	  if(length(x@transformation) >0)
	  	trans <- x@transformation[i]
	  else
		  trans <- list()
      new("GatingSet", pointer = subset_gs_by_sample(x@pointer, i), transformation = trans)
    })


#' subset the GatingSet/GatingSetList based on 'pData'
#'
#' @name subset
#' @param x \code{GatingSet} or \code{GatingSetList}
#' @param subset logical expression(within the context of pData) indicating samples to keep. see \code{\link[base:subset]{subset}}
#' @param ... other arguments. (not used)
#' @return a code{GatingSet} or \code{GatingSetList} object
#' @export
subset.GatingSet <- function (x, subset, ...)
{
  pd <- pData(x)
  r <- if (missing(subset))
        rep_len(TRUE, nrow(x))
      else {
        e <- substitute(subset)
        r <- eval(e, pd, parent.frame())
        if (!is.logical(r))
          stop("'subset' must be logical")
        r & !is.na(r)
      }

  x[as.character(rownames(pd)[r])]
}

#' @export
setMethod("getGate",signature(obj="GatingSet",y="character"),function(obj,y){
			.Deprecated("gs_pop_get_gate")
			gs_pop_get_gate(obj, y)
		})

#' @rdname gs_pop_get_gate
#' @export
gs_pop_get_gate <- function(obj,y){
			lapply(obj,function(x)gh_pop_get_gate(x,y))
		}

#' @export
setMethod("setNode"
    ,signature(x="GatingSet",y="character",value="ANY")
    ,function(x,y,value){
    if(is(value, "character")){
      .Deprecated("gs_pop_set_name")
      gs_pop_set_name(x, y, value)
    }else
    {
      .Deprecated("gs_pop_set_visibility")
      gs_pop_set_visibility(x, y, value)
    }
})
#' @rdname gs_pop_set_name
#' @export
gs_pop_set_name <- function(x,y,value){
  lapply(x,function(gh){
			  gh_pop_set_name(gh,y,value)
  })
  
}
#' @rdname gs_pop_set_visibility
#' @export
gs_pop_set_visibility <- function(x,y,value){
  lapply(x,function(gh){
    gh_pop_set_visibility(gh,y,value)
  })
  
}

#' @templateVar old getLoglevel
#' @templateVar new get_log_level
#' @template template-depr_pkg
NULL
#' get/set the log level
#'
#' It is helpful sometime to get more detailed print out for the purpose of trouble shooting
#'
#' @return a character that represents the internal log level
#' @rdname loglevel
#' @export
get_log_level <- function(){
  level <- cpp_getLogLevel()
  c("none", "GatingSet", "GatingHierarchy", "Population", "Gate")[level + 1]
}


#' @templateVar old setLoglevel
#' @templateVar new set_log_level
#' @template template-depr_pkg
NULL
#' @param level a \code{character} that represents the log level
#'                              , can be value of c("none", "GatingSet", "GatingHierarchy", "Population", "gate")
#'                                 default is "none" , which does not print any information from C parser.
#'
#' @examples
#' get_log_level()
#' set_log_level("Population")
#' get_log_level()
#'
#' @rdname loglevel
#' @export
set_log_level <- function(level = "none"){
  valid_levels <- c("none", "GatingSet", "GatingHierarchy", "Population", "Gate")
  level <- match.arg(level, valid_levels)
  cpp_setLogLevel( as.integer(match(level, valid_levels) - 1))
  level
}

#' Bracket operators on \code{GatingSet} and \code{GatingSetList} objects
#' 
#' @description \code{[[} extracts a \code{GatingHierarchy} object from a \code{GatingSet}.
#'
#' @name brackets
#' @aliases
#' [[ [[,GatingSet,numeric-method [[,GatingSet,logical-method [[,GatingSet,character-method
#' [[<-,GatingSet,ANY,ANY,GatingHierarchy-method
#' @param x a \code{GatingSet} or \code{GatingSetList}
#' @param i \code{numeric} or \code{logical} or \code{character} used as sample indices
#' @param j,...,drop unused
#' @return The \code{[} operator returns an object of the same type as \code{x} corresponding to the subset of indices
#' in i, while the \code{[[} operator returns a single \code{GatingHierarchy}
#' @export
setMethod("[[",c(x="GatingSet",i="numeric"),function(x,i,j,...){
      x[[sampleNames(x)[i]]]

    })


setMethod("[[",c(x="GatingSet",i="logical"),function(x,i,j,...){

      x[[sampleNames(x)[i]]]

    })
setMethod("[[",c(x="GatingSet",i="character"),function(x,i,j,...){
      as(x[i], "GatingHierarchy")
      
    })

#' @export
setReplaceMethod("[[",
		signature=signature(x="GatingSet",value="GatingHierarchy"),
		definition=function(x, i, j = "missing", ..., value)
			
		{
		  #dummy replacement method that only does some validity checking without doing the actual replacement 
		  #since whatever changes (at least for the existing setter for gh) made directly to gh should already be synced to gs
		  #gh is intended to be used as reference-type of object for any modification operations
			## stopifnot(identical(x@pointer, value@pointer))
		  stopifnot(identical(sampleNames(x[[i]]), sampleNames(value)))
		  #return gs as it is 
		  x
		})
#' Methods to get the length of a GatingSet
#'
#' Return the length of a \code{GatingSet} or \code{GatingSetList} object (number of samples).
#' @name length
#' @aliases length length,GatingSet-method
#' @param x \code{GatingSet}
#' @param object \code{object}
#' @export
setMethod("length","GatingSet",function(x){
      length(gs_cyto_data(x));
    })

#' @rdname length
#' @export
setMethod("show","GatingSet",function(object){
      cat("A GatingSet with",length(object), "samples\n")
    })

#' @export
setMethod("getPopStats", "GatingSet", function(x, statistic = c("freq", "count"), xml = FALSE, subpopulations = NULL, format = c("long", "wide"), path = "full", ...) {
.Deprecated("gs_pop_get_count_fast")
gs_pop_get_count_fast(x, statistic, xml, subpopulations, format, path, ...)
})

#' Return a table of population statistics for all populations in a GatingHierarchy/GatingSet
#'   or the population proportions or the total number of events of a node (population) in a GatingHierarchy
#'
#' gs_pop_get_count_fast is more useful than getPop. Returns a table of population statistics for all populations in a \code{GatingHierarchy}/\code{GatingSet}. Includes the xml counts, openCyto counts and frequencies.
#' 
#' @name gs_pop_get_count_fast
#' @aliases getPopStats,GatingSet-method
#' @param x A \code{GatingHierarchy} or \code{GatingSet}
#' @param statistic \code{character} specifies the type of population statistics to extract.(only valid when format is "wide"). Either "freq" or "count" is currently supported.
#' @param xml \code{logical} indicating whether the statistics come from xml (if parsed from xml workspace) or from openCyto.
#' @param path \code{character} see \link{gs_get_pop_paths}
#' @param format \code{character} value of c("wide", "long") specifing whether to origanize the output in long or wide format
#' @param subpopulations \code{character} vector to specify a subset of populations to return. (only valid when format is "long")
#' @param ... Additional arguments passed to \link{gs_get_pop_paths}
#'
#' @details
#' gs_pop_get_count_fast returns a table population statistics for all populations in the gating hierarchy. The output is useful for verifying that the import was successful, if the xml and openCyto derived counts don't differ much (i.e. if they have a small coefficient of variation.) for a GatingSet, returns a matrix of proportions for all populations and all samples
#'
#' @return
#' gs_pop_get_count_fast returns a \code{data.frame} with columns for the population name, xml derived counts, openCyto derived counts, and the population proportions (relative to their parent pouplation).
#' @seealso \code{\link{gs_get_pop_paths}}
#' @examples
#'         \dontrun{
#'         #gh is a GatingHierarchy
#'         gs_pop_get_count_fast(gh);
#'         gh_pop_get_stats(gh,gs_get_pop_paths(gh,tsort=T)[5])
#'
#'         #gs is a GatingSet
#'         gs_pop_get_count_fast(gs)
#'         #optionally output in long format as a data.table
#'         gs_pop_get_count_fast(gs, format = "long", path = "auto")
#'         #only get stats for a subset of populations
#'         gs_pop_get_count_fast(gs, format = "long", subpopulations = gs_get_pop_paths(gs)[4:6])
#'         }
#' @import data.table
#' @export
gs_pop_get_count_fast <- function(x, statistic = c("count", "freq"), xml = FALSE, subpopulations = NULL, format = c("long", "wide"), path = "full", ...) {
	 if(is(x, "GatingSetList"))
		 return(.gslist_get_pop_stats(x, format, statistic, xml, subpopulations, path, ...))
      # Based on the choice of statistic, the population statistics are returned for
      # each Gating Hierarchy within the GatingSet.
      statistic <- match.arg(tolower(statistic), c("count", "freq"))
      format <- match.arg(format, c("long", "wide"))
      path <- match.arg(path, c("full", "auto"))

      if(format == "long"){

        if(is.null(subpopulations))
          subpopulations <- gs_get_pop_paths(x, path = path, ...)[-1]

        pop_stats <- getPopCounts_cpp(x@pointer, statistic == "freq", subpopulations, xml, path == "full")
        if(statistic == "freq"){
        	pop_stats <- data.table(name = pop_stats[["name"]]
        							, Population = pop_stats[["Population"]]
        							, Parent = pop_stats[["Parent"]]
        							, Frequency = pop_stats[["Frequency"]]
        							, ParentFrequency = pop_stats[["ParentFrequency"]]
        	)
        }else{
        	pop_stats <- data.table(name = pop_stats[["name"]]
        							, Population = pop_stats[["Population"]]
        							, Parent = pop_stats[["Parent"]]
        							, Count = pop_stats[["Count"]]
        							, ParentCount = pop_stats[["ParentCount"]]
        	)
        }
      }else{


        # The 'xml' flag determines whether the 'xml' or 'openCyto' statistics
        # are returned.
        if (xml) {
          statistic <- paste("xml", statistic, sep = ".")
        } else {
          statistic <- paste("openCyto", statistic, sep = ".")
        }

        stats <- lapply(x,function(y){
                d<-gh_pop_compare_stats(y, path = path,...)
                d$key<-rownames(d)
                setkeyv(d,"key")
                d<-d[,list(key,get(statistic))]
                setnames(d,c("key",sampleNames(y)))
                setkeyv(d,"key")
                d
        })
        pop_stats <- Reduce(function(x,y)merge(x,y,all=TRUE),stats)

        rownames(pop_stats) <- pop_stats[,key]
        setkey(pop_stats,NULL)
        pop_stats$key<-NULL
        rn<-rownames(pop_stats)
        pop_stats<-as.matrix(pop_stats)
        rownames(pop_stats)<-rn

    }
    pop_stats
}

#' calculate the coefficient of variation
#' 
#' This builds matrix with all node labels for all GH's
#' so expect many NAs if the GH's don't have matching trees
#' 
#' @noRd 
.computeCV <- function(x, ...){
  #columns are populations
  #rows are samples
  statList <- lapply(x,function(gh){

        thisStat <- gh_pop_compare_stats(gh, ...)
        thisStat
      })

  cv <- do.call(rbind
              ,lapply(statList,function(x){

                    res <- apply(x[,list(xml.count,openCyto.count)],1,function(x){
                          cv <- IQR(x)/median(x)
                          ifelse(is.nan(cv),0,cv)
                        })
                    names(res) <- rownames(x)
                    res
                  })
             )

  cv

}

#' Plot the coefficient of variation between xml and openCyto population statistics for each population in a gating hierarchy.
#'
#' This function plots the coefficient of variation calculated between the xml population statistics and the openCyto population statistics for each population in a gating hierarchy extracted from a xml Workspace.
#' 
#' @name gs_plot_pop_count_cv
#' @param x A \code{GatingHierarchy} from or a \code{GatingSet}.
#' @param scales \code{list} see \link{barchart}
#' @param path \code{character} see \link{gs_get_pop_paths}
#' @param \dots Additional arguments to the \code{barplot} methods.
#' @details The CVs are plotted as barplots across panels on a grid of size \code{m} by \code{n}.
#' @return Nothing is returned.
#' @seealso \code{\link{gs_pop_get_count_fast}}
#' @examples
#'   \dontrun{
#'     #G is a GatingHierarchy
#'     gs_plot_pop_count_cv(G,4,4);
#'   }
#' @aliases gs_plot_pop_count_cv
#' @export
gs_plot_pop_count_cv <- function(x, scales = list(x = list(rot = 90)), path = "auto",...){
  .Defunct() 
   }

#' @export
setMethod("keyword",c("GatingSet", "missing"),function(object,keyword = "missing", ...){
        lapply(object, flowCore::keyword, ...)

    })

#' @export
setMethod("keyword",c("GatingSet","character"),function(object,keyword){
      tmp<-data.frame(unlist(lapply(object,function(x)keyword(x,keyword)),use.names=FALSE));
      tmp<-data.frame(matrix(tmp[[1]],ncol=length(keyword),byrow=T))
      colnames(tmp)<-keyword
      tmp
    })

#' @rdname keyword-mutators
#' @export
gs_keyword_insert <- function(gs, keys, values){
  cs <- gs_pop_get_data(gs)
  if(missing(values))
    cs_keyword_insert(cs, keys)
  else
    cs_keyword_insert(cs, keys, values)
}

#' @rdname keyword-mutators
#' @export
gs_keyword_delete <- function(gs, keys){
  cs <- gs_pop_get_data(gs)
  cs_keyword_delete(cs, keys)
}

#' @rdname keyword-mutators
#' @export
gs_keyword_rename <- function(gs, old_keys, new_keys){
  cs <- gs_pop_get_data(gs)
  if(missing(new_keys))
    cs_keyword_rename(cs, old_keys)
  else
    cs_keyword_rename(cs, old_keys, new_keys)
}

#' @rdname keyword-mutators
#' @export
gs_keyword_set <- function(gs, keys, values){
  cs <- gs_pop_get_data(gs)
  if(missing(values))
    cs_keyword_set(cs, keys)
  else
    cs_keyword_set(cs, keys, values)
}


#' tranform the flow data asssociated with the GatingSet
#'
#' The transformation functions are saved in the GatingSet and can be retrieved by \link{gh_get_transformations}.
#' Currently only flowJo-type biexponential transformation(either returned by \link{gh_get_transformations} or constructed by \link{flowJoTrans})
#' is supported.
#' 
#' @name transform
#' @aliases transform,GatingSetList-method transform,GatingSet-method
#' @param _data \code{GatingSet} or \code{GatingSetList}
#' @param translist expect a \code{transformList} object or a list of \code{transformList} objects(with names matched to sample names)
#' @param ... other arguments passed to 'transform' method for 'ncdfFlowSet'.(e.g. 'ncdfFile')
#' @return a \code{GatingSet} or \code{GatingSetList} object with the underling flow data transformed.
#' @examples
#' \dontrun{
#' library(flowCore)
#' data(GvHD)
#' fs <- GvHD[1:2]
#' gs <- GatingSet(fs)
#'
#' #construct biexponential transformation function
#' biexpTrans <- flowjo_biexp_trans(channelRange=4096, maxValue=262144, pos=4.5,neg=0, widthBasis=-10)
#'
#' #make a transformList object
#' chnls <- c("FL1-H", "FL2-H")
#' transList <- transformerList(chnls, biexpTrans)
#'
#' #add it to GatingSet
#' gs_trans <- transform(gs, transList)
#'
#' }
#' @export
setMethod("transform",
    signature = signature(`_data` = "GatingSet"),
    definition = function(`_data`, translist, ...)
    {
      
      gs <- `_data`
      # browser()
      if(missing(translist))
        stop("Missing the second argument 'translist'!")
      else if(is(translist, "transformerList"))
      {
        translist <- sapply(sampleNames(gs), function(obj)translist, simplify = FALSE)
      }
      
      if(is(translist, "list"))
      {
        tList <- lapply(translist, function(trans){
          if(!is(trans, "transformerList"))
            stop("All the elements of 'translist' must be 'transformerList' objects!")
          
          res <- lapply(trans, function(obj)obj[["transform"]])
          transformList(names(trans), res)  
        })
      }else
        stop("expect the second argument as a 'transformerList' object or a list of 'transformerList' objects!")
  	
	#check if all trans are supported by Rcpp
  unrecognized <- FALSE
	for(sn in names(translist))
	{
	  
		for(trans in translist[[sn]])
		{
  		  
  		transobj <- parse_transformer(trans)
  		if(length(transobj)==0)
  		{
  			unrecognized <- TRUE
  			break
  		}
  		
		}
	  
	  if(unrecognized)
	    break
	}
	if(unrecognized)#transform in R
	{
		gs@transformation <- translist

		cs <- gs_pop_get_data(gs)

		suppressMessages(transform(cs, tList, ...))
		
	}else
	{ #transform data and store trans in c++
		for(sn in names(translist))
		{
			transobjs <- sapply(translist[[sn]], parse_transformer, simplify = FALSE)
			# browser()
			set_transformations(gs@pointer, sn, transobjs)

		}
		gs_transform_data(gs@pointer)
	}
	gs
    })

#' Constructor for transformerList object
#'
#' Similar to \code{transformList} function, it constructs a list of transformer objects generated by \code{trans_new}
#' method from \code{scales} so that the inverse and breaks functions are also included.
#' @param from channel names
#' @param trans a \code{trans} object or a list of \code{trans} objects constructed by \code{trans_new} method.
#' @export
#' @examples
#' library(flowCore)
#' library(scales)
#' #create tranformer object from scratch
#' trans <- logicleTransform(w = 0.5, t = 262144, m = 4.5, a = 0)
#' inv <- inverseLogicleTransform(trans = trans)
#' trans.obj <- flow_trans("logicle", trans, inv, n = 5, equal.space = FALSE)
#'
#' #or simply use convenient constructor
#' #trans.obj <- logicle_trans(n = 5, equal.space = FALSE, w = 0.5, t = 262144, m = 4.5, a = 0)
#'
#' transformerList(c("FL1-H", "FL2-H"), trans.obj)
#'
#' #use different transformer for each channel
#' trans.obj2 <- asinhtGml2_trans()
#' transformerList(c("FL1-H", "FL2-H"), list(trans.obj, trans.obj2))
transformerList <- function (from, trans)
{
  from <- unique(from)
  if(is(trans, "transform"))trans <- list(trans)
  if (!is.character(from))
    stop("'from' must be character vectors.", call. = FALSE)
  if (!is.list(trans))
    trans <- list(trans)
  if (!all(sapply(trans, is, "transform")))
    stop("'trans' must be a list of transformer objects (generated by scales::trans_new method)", call. = FALSE)
  trans <- rep(trans, length(from))
  trans <- trans[1:length(from)]
  names(trans) <- from
  attr(trans, "class") <- c("transformerList", "list")

  return(trans)
}

#' compensate the flow data asssociated with the GatingSet
#'
#' The compensation is saved in the GatingSet and can be retrieved by \link{gh_get_compensations}.
#' @name compensate
#' @aliases compensate,GatingSetList,ANY-method compensate,cytoset,ANY-method
#' compensate,cytoset,list-method compensate,cytoset,matrix-method compensate,cytoframe,matrix-method
#' compensate,GatingSet,ANY-method
#' @param x \code{GatingSet}, \code{GatingSetList}, \code{cytoframe}, or \code{cytoset}
#' @param spillover \code{compensation} object or spillover matrix or a list of \code{compensation} objects
#' @return a \code{GatingSet}, \code{GatingSetList}, \code{cytoframe}, or \code{cytoset} object with the underling flow data compensated.
#' @examples
#' \dontrun{
#'
#' cfile <- system.file("extdata","compdata","compmatrix", package="flowCore")
#' comp.mat <- read.table(cfile, header=TRUE, skip=2, check.names = FALSE)
#' ## create a compensation object
#' comp <- compensation(comp.mat,compensationId="comp1")
#' #add it to GatingSet
#' gs <- compensate(gs, comp)
#' }
#' @export
setMethod("compensate", signature=signature(x="GatingSet", spillover="ANY"),
    definition=function(x, spillover){
      selectMethod("compensate", signature=c(x="cytoset", spillover="ANY"))(x, spillover)
      
    })
#' @rdname gh_get_compensations
#' @export
gs_get_compensations <- function(x){
			lapply(x, gh_get_compensations)
		}
            
#' @export
setMethod("markernames",
          signature=signature(object="GatingSet"),
          definition=function(object){

            markernames(gs_cyto_data(object))

          })

#' @export
setReplaceMethod("markernames",
                 signature=signature(object="GatingSet", value="ANY"), function(object, value){

                   markernames(gs_cyto_data(object)) <- value

                   object
                 })

#' @export
setMethod("colnames",
          signature=signature(x="GatingSet"),
          definition=function(x, do.NULL="missing", prefix="missing"){

            colnames(gs_cyto_data(x))

          })

#' @export
setReplaceMethod("colnames",
                 signature=signature(x="GatingSet", value="ANY"), function(x, value){

                   colnames(gs_cyto_data(x)) <- value

                   x
                 })

#' @rdname cleanup_temp
#' @export
gs_cleanup_temp <- function(x, temp_dir = NULL){
	cs_cleanup_temp(gs_cyto_data(x), temp_dir)
}
