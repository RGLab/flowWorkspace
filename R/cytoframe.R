#' \code{cytoframe}: A reference class for efficiently managing the data representation of
#' a \code{flowFrame}
#'
#' This class serves the same purpose as the \code{\link[flowCore]{flowFrame}} class from the \code{flowCore} package: 
#' to store quantitative data on cell populations from a single FCS run. The primary difference is in the underlying representation
#' of the data. While \code{flowFrame} objects store the underlying data matrix in the \code{exprs} slot 
#' as an R object, \code{cytoframe} objects store the matrix (as well as the data from the other slots) in a 
#' C data structure that is accessed through an external pointer. This allows for greater optimization of 
#' data operations including I/O, parsing, transformation, and gating.
#' 
#' @details
#' From the user's standpoint, interacting with a \code{cytoframe} is very similar to interacting with a
#' \code{flowframe}, with one important difference. While operations such as subsetting or copying a \code{flowFrame}
#' using the standard R assignment operator (<-) will perform a deep copy of the data in its slots, the same
#' operations on a \code{cytoframe} will produce a view to the same underlying data as the original object.
#' This means that changes made to the \code{cytoframe} resulting from subsetting or copying will affect
#' the original \code{cytoframe}. If a deep copy of the underyling data is desired, the \code{realize_view} method
#' will accomplish this.
#' 
#' Because the \code{cytoframe} class inherits from \code{flowFrame}, the \code{flowFrame} slots are present but
#' not utilized. Thus, attempting to access them directly will yield empty data structures. However, the
#' \code{\link{exprs}}, \code{\link{parameters}}, or \code{\link{description}} methods work in a manner similar
#' to a \code{flowFrame} by accessing the same information from the underlying data structure.
#' 
#' @name cytoframe
#' @aliases cytoframe-class realize_view realize_view,cytoframe-method [,cytoframe,ANY-method
#' keyword,cytoframe,missing-method markernames,cytoframe-method markernames<-,cytoframe-method
#' @docType class
#' 
#' @section Methods:
#'   Many of the methods here have their own documentation pages or are more extensively explained
#'   in the documentation for \code{\link[flowCore]{flowFrame}}, so those documentation pages may
#'   be consulted as well for more details.
#'   \describe{
#'   \item{\code{[}}{Subsetting. Returns an object of class \code{cytoframe}.
#'     The syntax for subsetting is similar to that of \code{\link[=data.frame]{data.frames}}. 
#'     In addition to the usual index vectors (integer and logical by
#'     position, character by parameter names), \code{cytoframe}s can be
#'     subset via \code{\link{filterResult}} and
#'     \code{\linkS4class{filter}} objects.
#'     
#'     \emph{Usage:}\cr\cr
#'     \code{   cytoframe[i,j]}\cr\cr
#'     \code{   cytoframe[filter,]}\cr\cr
#'     \code{   cytoframe[filterResult,]}\cr\cr
#'     Note that the value of argument \code{drop} is ignored when
#'     subsetting \code{cytoframes}.\cr\cr
#'   }
#'   \item{$}{Subsetting by channel name. This is similar to subsetting
#'     of columns of \code{\link[=data.frame]{data.frames}}, i.e.,
#'     \code{frame$FSC.H} is equivalent to \code{frame[, "FSC.H"]}. Note
#'     that column names may have to be quoted if they are not valid R
#'     symbols (e.g. \code{frame$"FSC-H"} or \code{frame$`FSC-H`}).\cr\cr
#'   }
#'   \item{exprs, exprs<-}{
#'     \code{exprs} returns an object of class \code{matrix} containing the
#'     measured intensities. Rows correspond to cells, columns to the
#'     different measurement channels. The \code{colnames} attribute of
#'     the matrix should hold the names or identifiers for the
#'     channels. The \code{rownames} attribute would usually not be set.\cr\cr
#'     \code{exprs<-} replaces the raw data intensities. The replacement value 
#'     must be a numeric matrix with \code{colnames} matching the parameter definitions. 
#'     Implicit subsetting is allowed (i.e. less columns in the replacement value 
#'     compared to the original \code{cytoframe}), but all columns must be defined in the original \code{cytoframe}.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   exprs(cytoframe)}\cr\cr
#'     \code{   exprs(cytoframe) <- value}\cr\cr
#'   }
#'   \item{head, tail}{Show first/last elements of the raw data matrix\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   head(cytoframe)}\cr\cr
#'     \code{   tail(cytoframe)}\cr\cr
#'   }
#'   \item{keyword, keyword<-}{Extract all entries or a single entry
#'     from the annotations by keyword or replace
#'     the entire list of key/value pairs with a new named
#'     list. See \code{\link{keyword}} for details.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   keyword(cytoframe)}\cr\cr
#'     \code{   keyword(cytoframe, character)}\cr\cr
#'     \code{   keyword(cytoframe) <- list(value) }\cr\cr
#'   }
#'   \item{parameters, parameters<-}{
#'     Extract parameters and return an object of class 
#'     \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}}
#'     containing information about each column of the \code{cytoframe},
#'     or replace such an object.\cr\cr
#'     This information will generally be filled in by
#'     \code{load_cytoframe_from_fcs} or similar functions using data from the
#'     \acronym{FCS} keywords describing the parameters. To access the actual parameter
#'     annotation, use \code{pData(parameters(cytoframe))}.\cr\cr
#'     Replacement is only valid with
#'     \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrames}}
#'     containing all varLabels \code{name}, \code{desc}, \code{range},
#'     \code{minRange} and \code{maxRange}, and matching entries in the
#'     \code{name} column to the colnames of the \code{exprs} matrix. See
#'     \code{\link{parameters}} for more details.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   parameters(cytoframe)}\cr\cr
#'     \code{   parameters(cytoframe) <- value}\cr\cr
#'   }
#'   \item{show}{
#'     Display details about the \code{cytoframe} object.\cr\cr
#'   }
#'   \item{summary}{Return descriptive statistical summary (min, max, mean and quantile) for each channel\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   summary(cytoframe)}\cr\cr
#'   }
#'   \item{plot}{Basic plots for \code{cytoframe} objects. If the object
#'     has only a single parameter this produces a \code{\link[graphics:hist]{histogram}}. 
#'     For exactly two parameters we plot a bivariate density map (see \code{\link[graphics]{smoothScatter}})
#'     and for more than two parameters we produce a simple splom plot. 
#'     To select specific parameters from a \code{flowFrame} for plotting, either subset the object or
#'     specify the parameters as a character vector in the second argument to \code{plot}. 
#'     The smooth parameters lets you toggle between density-type \code{\link[graphics]{smoothScatter}}
#'     plots and regular scatterplots.  For far more sophisticated plotting of flow cytometry data, 
#'     see the \code{ggcyto} package.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   plot(cytoframe, ...)}\cr\cr
#'     \code{   plot(cytoframe, character, ...)}\cr\cr
#'     \code{   plot(cytoframe, smooth=FALSE, ...)}\cr\cr
#'   }
#'   \item{ncol, nrow, dim}{Extract the dimensions of the data matrix.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   ncol(cytoframe)}\cr\cr
#'     \code{   nrow(cytoframe)}\cr\cr
#'     \code{   dim(cytoframe)}\cr\cr
#'   }
#'   \item{featureNames, colnames, colnames<-}{\code{colnames} and
#'     \code{featureNames} are synonyms. They extract parameter names 
#'     (i.e., the colnames of the data matrix).
#'     For \code{colnames} there is also a replacement method. This will
#'     update the \code{name} column in the \code{parameters} slot as well.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   featureNames(cytoframe)}\cr\cr
#'     \code{   colnames(cytoframe)}\cr\cr
#'     \code{   colnames(cytoframe) <- value}\cr\cr
#'   }
#'   \item{markernames, markernames<-}{Access or replace the marker names associated
#'   with the channels of the \code{cytoframe}. For replacement, \code{value} should
#'   be a named list or character vector where the names correspond to the channel names
#'   and the values correpond to the marker names.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{markernames(object)}\cr\cr
#'   \code{markernames(object) <- value}\cr\cr
#'   }
#'   \item{names}{Extract pretty formatted names of the parameters
#'     including parameter descriptions.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   names(cytoframe)}\cr\cr
#'   }
#'   \item{identifier}{Extract GUID of a \code{cytoframe}. Returns the
#'     file name if no GUID is available. See \code{\link{identifier}}
#'     for details.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   identifier(cytoframe)}\cr\cr
#'   }
#'   \item{range}{Get instrument or actual data range of the \code{cytoframe}. Note that
#'     instrument dynamic range is not necessarily the same as the range of the actual data values, but
#'     the theoretical range of values the measurement instrument was
#'     able to capture. The values of the dynamic range will be
#'     transformed when using the transformation methods for\code{cytoframe} objects.\cr\cr
#'     \emph{Parameters:}\cr\cr
#'       x: cytoframe object.\cr\cr
#'       type: Range type. either "instrument" or "data". Default is "instrument"\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   range(x, type = "data")}\cr\cr
#'   }
#'   \item{each_row, each_col}{Apply functions over rows or columns of
#'     the data matrix. These are convenience methods. See
#'     \code{\link[flowCore]{each_col}} for details.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   each_row(cytoframe, function, ...)}\cr\cr
#'     \code{   each_col(cytoframe, function, ...)}\cr\cr
#'   }
#'   \item{transform}{Apply a transformation function on a
#'     \code{cytoframe} object. This uses R's
#'     \code{\link[base]{transform}} function by treating the
#'     \code{cytoframe} like a regular \code{data.frame}. \code{flowCore}
#'     provides an additional inline mechanism for transformations (see
#'     \code{\link{\%on\%}}) which is strictly more limited
#'     than the out-of-line transformation described here.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   transform(cytoframe, translist, ...)}\cr\cr
#'   }
#'   \item{filter}{Apply a \code{\linkS4class{filter}} object on a
#'     \code{cytoframe} object. This returns an object of class
#'     \code{\link{filterResult}}, which could then be used for
#'     subsetting of the data or to calculate summary statistics. See
#'     \code{\link{filter}} for details.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   filter(cytoframe, filter)}\cr\cr
#'     }
#'   \item{split}{Split \code{cytoframe} object according to a
#'     \code{\link[flowCore:filter-class]{filter}}, a \code{\link[flowCore]{filterResult}} or a
#'     \code{factor}. For most types of filters, an optional
#'     \code{flowSet=TRUE} parameter will create a
#'     \code{\linkS4class{flowSet}} rather than a simple list. See
#'     \code{\link{split}} for details.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   split(cytoframe, filter, flowSet=FALSE, ...)}\cr\cr
#'     \code{   split(cytoframe, filterResult, flowSet=FALSE, ...)}\cr\cr
#'     \code{   split(cytoframe, factor, flowSet=FALSE, ...)}\cr\cr
#'     }
#'   \item{Subset}{Subset a \code{cytoframe} according to a \code{filter}
#'     or a logical vector. The same can be done using the standard
#'     subsetting operator with a \code{filter}, \code{filterResult}, or
#'     a logical vector as first argument.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   Subset(cytoframe, filter)}\cr\cr
#'     \code{   Subset(cytoframe, logical)}\cr\cr
#'     }
#'   \item{cbind2}{\strong{Not yet implemented}.\cr Expand a \code{cytoframe} by the data in a
#'     \code{numeric matrix} of the same length. The \code{matrix} must
#'     have column names different from those of the
#'     \code{cytoframe}. The additional method for \code{numerics} only
#'     raises a useful error message.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   cbind2(cytoframe, matrix)}\cr\cr
#'     \code{   cbind2(cytoframe, numeric)}\cr\cr
#'     }
#'   \item{compensate}{Apply a compensation matrix (or a
#'     \code{\linkS4class{compensation}} object) on a \code{cytoframe}
#'     object. This returns a compensated \code{cytoframe}.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   compensate(cytoframe, matrix)}\cr\cr
#'     \code{   compensate(cytoframe, data.frame)}\cr\cr
#'     \code{   compensate(cytoframe, compensation)}\cr\cr
#'     }
#'   \item{decompensate}{\strong{Not yet implemented}.\cr Reverse the application of a compensation matrix (or a
#'     \code{\linkS4class{compensation}} object) on a \code{cytoframe}
#'     object. This returns a decompensated \code{cytoframe}.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   decompensate(cytoframe, matrix)}\cr\cr
#'     \code{   decompensate(cytoframe, data.frame)}\cr\cr
#'     }
#'   \item{spillover}{Extract spillover matrix from description slot if
#'     present. It is equivalent to 
#'     \code{keyword(x, c("spillover", "SPILL"))}
#'     Thus will simply return a list of keyword values for "spillover" and "SPILL".\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{   spillover(cytoframe)}\cr\cr
#'     }
#'   \item{realize_view}{Returns a new \code{cytoframe} with its own copy of the
#'   underlying data (a deep copy). The optional \code{filepath} argument accepts
#'   a string to specify a full filename for storing the new copy of the data in h5
#'   format.\cr\cr
#'     \emph{Usage:}\cr\cr
#'     \code{realize_view(cytoframe, filepath)}\cr\cr
#'   }
#'   
#'   
#'   
#' }
#' 
#' @seealso
#' 
#' \code{\linkS4class{flowSet}}, \code{\link{read.FCS}}
#' @keywords classes
#'
#' @importClassesFrom flowCore flowFrame
#' @export 
setClass("cytoframe", contains = "flowFrame" ,               
    representation=representation(pointer = "externalptr"
                                  , use.exprs = "logical" #for the purpose  of backward compatible (e.g. fs[[1, use.exprs = F]]
                                  ),
                                  prototype = list(use.exprs = FALSE) 
                              )


# compensate the data in place
#' @export
setMethod("compensate",
    signature=signature(x="cytoframe",
        spillover="matrix"),
    definition=function(x, spillover)
    {
      frm_compensate(x@pointer, spillover)
      x
    })

cf_colnames <- function(cf){
  get_channels(cf@pointer)
}

cf_rownames <- function(cf){
  res <- get_rownames(cf@pointer)
  if(length(res) == 0)
    res <- NULL
  res
}

#' @export
setMethod("colnames", "cytoframe",
          function (x, do.NULL = TRUE, prefix = "col") 
          {
            cf_colnames(x)
          })
#' @export
#' @importFrom BiocGenerics rownames rownames<-
setMethod("rownames", "cytoframe",
    function (x, do.NULL = TRUE, prefix = "row") 
    {
      dimnames(x)[[1L]]
      })
#' @export
setMethod("dimnames", "cytoframe",
          function(x)
          {
            ans <- list(cf_rownames(x), cf_colnames((x)))
            DelayedArray:::simplify_NULL_dimnames(ans)
          })
#' @export
setReplaceMethod("rownames", c("cytoframe"),
                 function(x, value)
                 {
                    if(is.null(value))
                      del_rownames(x@pointer)
                    else
                      set_rownames(x@pointer, value)
                    x
                 })

setMethod("nrow",
    signature=signature(x="cytoframe"),
    definition=function(x)
      getnrow(x@pointer)
)

setMethod("ncol",
    signature=signature(x="cytoframe"),
    definition=function(x)
      getncol(x@pointer)
)
#' @importFrom S4Vectors coolcat
setMethod("show",
          signature=signature(object="cytoframe"),
          definition=function(object)
          {
            
            selectMethod("show", "flowFrame")(object)
            coolcat("row names(%d): %s\n", rownames(object))
            
            if(cf_is_subsetted(object))
            {
              cat("cytoframe has been subsetted and can be realized through 'realize_view()'.\n")  
            }
            
            
            
          })

#' check whether a cytoframe/cytoset is a subsetted(by column or by row) view
#' 
#' @param x a cytoset or cytoframe
#' @rdname is_subsetted
#' @export
cf_is_subsetted <- function(x){
  cf_is_indexed(x@pointer)
}

#' @export
realize_view <- function(x, filepath)UseMethod("realize_view")

#' @export 
realize_view.cytoframe <- function(x, filepath = NULL){
  if(is.null(filepath))
    filepath <- tempfile(fileext = paste0(".", cf_backend_type(x)))
  new("cytoframe", pointer = realize_view_cytoframe(x@pointer, filepath), use.exprs = TRUE)
}

#it is equivalent to x[] thus not really necessary to be exposed to users
#to avoid the confusion to users we keep it private
copy_view <- function(x, ...)UseMethod("copy_view")

copy_view.cytoframe <- function(x){
  new("cytoframe", pointer = copy_view_cytoframe(x@pointer), use.exprs = TRUE)
}

setMethod("[",
    signature=signature(x="cytoframe"),
    definition=function(x, i, j, ..., drop=FALSE)
    {
      fr <- copy_view(x)
      if(drop)
        warning("Argument 'drop' ignored for subsetting of flowFrame")
      msg <- "Subset out of bounds"
      if(!missing(j)){
        if(is.logical(j))
         j <- which(j)
        else if(is.character(j))
        {
          j <- match(j, colnames(x))
          if(any(is.na(j)))
            stop(msg, call.=FALSE)
        }
            
        if(is.numeric(j)||is.integer(j)){
        	if(any(j < 0)){
        		if(!all(j <= 0)){
        			stop("Cannot mix positive and negative subscripts")
        		}
        		j <- (1:length(colnames(x)))[j]
        	}
        	subset_cytoframe_by_cols(fr@pointer, as.integer(j - 1))
        }    
        else
          stop("invalid j index!")
      }
      
      if(!missing(i))
      {
        if(is.logical(i))
          i <- which(i)
        if(is.numeric(i)||is.integer(i))    
          subset_cytoframe_by_rows(fr@pointer, as.integer(i - 1))
        else
          stop("invalid i index!")
      }

      fr
    })

setMethod("exprs",
    signature=signature(object="cytoframe"),
    definition=function(object){
      if(object@use.exprs)
        mat <- cf_getData(object@pointer)
      else
      {
        cn <- colnames(object)
        mat <- matrix(nrow = 0, ncol = length(cn), dimnames = list(NULL, cn))
      }
	  # browser()
      # rownames(mat) <- rownames(object) #strange that rownames method always fall back to ANY signature
      mat
    })

setReplaceMethod("exprs",
		signature=signature(object="cytoframe",
				value="matrix"),
		definition=function(object, value)
		{
			cf_setData(object@pointer, value)
			object
		})
 
 setReplaceMethod("colnames",
     signature=signature(x="cytoframe",
         value="ANY"),
     definition=function(x, value)
     {
       old.names <- colnames(x)
       if(length(value) != length(old.names))
         stop("colnames don't match dimensions of data matrix",
             call.=FALSE)
       
 		set_all_channels(x@pointer, value)
       
       return(x)
     })

#' Methods to change channel and marker names for \code{cytoframe} and \code{cytoset} objects
#' 
#' The methods allow direct alteration of channel names or marker names
#' of \code{\link{cytoframe}} and \code{\link{cytoset}} objects. These objects are accessed
#' by reference and changed in place, so there is no need to assign the return
#' value of these methods.
#' 
#' @name cytoframe-labels
#' @aliases cf_swap_colnames cf_rename_channel cf_rename_marker cs_swap_colnames
#' @param x a \code{cytoframe}
#' @param old old channel or marker name to be changed
#' @param new new channel or marker name after change
#' @param col1 first channel name to swap
#' @param col2 second channel name to swap
#' 
#' @export 
cf_swap_colnames <- function(x, col1, col2){
	tmp <- "MagicStringUgly"
	
	cf_rename_channel(x, col1, tmp)
	cf_rename_channel(x, col2, col1)
	cf_rename_channel(x, tmp, col2)
	
}
#' @rdname cytoframe-labels
#' @export
cf_rename_channel <- function(x, old, new){
	stopifnot(is(x, "cytoframe"))
	setChannel(x@pointer, old, new)
  
}
#' @rdname cytoframe-labels
#' @export
cf_rename_marker <- function(x, old, new){
	stopifnot(is(x, "cytoframe"))
	pd <- getpdata(x@pointer)
  pd <- subset(pd, desc == old)
  nr <- nrow(pd)
  if(nr == 0)
    stop("old marker is not found: ", old)
  else if(nr > 1)
    stop("old marker is ambiguous: ", old)
  else
  {
    chnl <- pd[["name"]]
    names(new) <- chnl
    markernames(x) <- new
  }
  
}
setReplaceMethod("markernames",
    signature=signature(object="cytoframe", value="ANY"), function(object, value){
		pdata <- getpdata(object@pointer)
		channel.names <- pdata[["name"]]
      if(!is.character(value)){
        stop("value must be a named character vector!")
      }else{
        chnls <- names(value)
        if(is.null(chnls))
          stop("value must be a named character vector!")
        inds <- match(chnls, channel.names)
        misMatch <- is.na(inds)
        if(any(misMatch))
          stop("channel names not found in flow data: ", paste0(chnls[misMatch], collapse = ","))
        #validity check
		oldmarkers <- pdata[["desc"]]		
		oldmarkers[inds] <- value
		oldmarkers <- oldmarkers[!is.na(oldmarkers) & oldmarkers != ""]
		dup <- duplicated(oldmarkers)
		if(any(dup))
		{
			
			stop("Trying to assign the marker: ", oldmarkers[dup][1], " to multiple channels")
		}
			
        for(i in seq_along(inds)){
          ind <- inds[i]
          if(is.na(value[i]))
            value[i] <- ""
          setMarker(object@pointer, channel.names[ind], value[i])
        }
          
        
        
        
        object
      }
      
      
    })

setMethod("parameters",
    signature=signature(object="cytoframe"),
    definition=function(object, names=FALSE)
    {
      if(!names)
      {
        pdata <- getpdata(object@pointer)
        new("AnnotatedDataFrame",
            data=pdata,
            varMetadata=data.frame(row.names=I(c("name","desc","range",
                        "minRange", "maxRange")),
                labelDescription=I(c("Name of Parameter","Description of Parameter",
                        "Range of Parameter", "Minimum Parameter Value after Transforamtion",
                        "Maximum Parameter Value after Transformation"))))
        
        
      }else
        as.character(parameters(object)[["name"]])
    })

setReplaceMethod("parameters",
		signature=signature(object="cytoframe",
				value="AnnotatedDataFrame"),
		definition=function(object, value){
			if(!all(c("name", "desc", "range", "minRange",
							"maxRange") %in% varLabels(value)))
				stop("varLabels of this AnnotatedDataFrame don't ",
						"match the specifications", call.=FALSE)
			if(!all(colnames(exprs(object)) ==  value$name))
				stop("parameter names don't match colnames of the ",
						"exprs matrix", call.=FALSE)
			pd <- pData(value)
			pd[["desc"]][is.na(pd[["desc"]])] <- ""#we currently don't handle NA in Rcpp (for the sake of simplicity of c code)
			setpdata(object@pointer, pd)
			return(object)
		})

setMethod("transform",
          signature=signature(`_data`="cytoframe"),
          definition=function(`_data`, translist, ...)
          {
            if(missing(translist))
              stop("Missing the second argument 'translist'!")
            else if(!(is(translist, "transformerList") || is(translist, "transformList")))
              stop("expect the second argument as a 'transformList' or 'transformerList' object!")
            
            if(is(translist, "transformList")){
              tList <- translist
            }else if(is(translist, "transformerList")){
              unrecognized <- FALSE
              # This is checked at the GatingSet level too
              for(trans in translist)
              {
                transobj <- parse_transformer(trans)
                if(length(transobj)==0)
                {
                  unrecognized <- TRUE
                  break
                }
              }
              if(unrecognized){
                # If any transformers unsupported at C++ level, transform in R
                res <- lapply(translist, function(obj)obj[["transform"]])
                tList <- transformList(names(translist), res)
              }else{
                tList <- sapply(translist, parse_transformer, simplify = FALSE)
              }
            }else{
              stop("expect the second argument as a 'transformList' or 'transformerList' object!")
            }
            
            # Transform in R due to current lack of support at C++ level
            if(is(tList, "transformList")){
              # flowCore %on% method for flowFrame
              # Also updates keywords: "transformation", "PnRmax", "PnRmin"
              return(tList %on% `_data`)
            }else{
              # Do the transformation at the C++ level
              cf_transform_data(`_data`@pointer, tList)
              return(`_data`)
            }
            
          })

process_spill_keyword <- function(desc){
  ## the spillover matrix
  for(sn in flowCore:::.spillover_pattern){
    sp <- desc[[sn]]
    if(!is.null(sp)){
      desc[[sn]] <- flowCore:::txt2spillmatrix(sp)
    }
  }
  desc
}


## this is equivalent to the description method
setMethod("keyword",
    signature=signature(object="cytoframe",
        keyword="missing"),
    function(object, compact = FALSE)
    {           
      desc <- cf_getKeywords(object@pointer)

      if(compact)
        desc <- flowCore:::kwfilter(desc)
      desc <- as.list(desc) 
	
      FCSversion <- desc[["FCSversion"]]
      desc[["FCSversion"]] <- NULL
      desc <- c(FCSversion = FCSversion, desc)  
      process_spill_keyword(desc)
    })

#' @importFrom flowCore collapse_desc
setReplaceMethod("keyword",
    signature=signature(object="cytoframe",
        value="list"),
    definition=function(object, value)
    {
      n <- names(value)
      if(length(n) == 0)
        stop(kwdError, call.=FALSE)
	  value <- collapse_desc(value) #flattern and coerce any R object to string
      cf_setKeywords(object@pointer, value)
      return(object)
    })

#' Methods to alter keywords in \code{cytoframe}, \code{cytoset}, \code{GatingHierarchy}, or \code{GatingSet} objects
#' 
#' These methods allow for direct insertion, deletion, or renaming
#' of keywords in \code{\link{cytoframe}}, \code{\link{cytoset}}, \code{\link{GatingHierarchy}}, 
#' or \code{\link{GatingSet}} objects.
#' 
#' @param cf a \code{\link{cytoframe}}
#' @param cs a \code{\link{cytoset}}
#' @param gh a \code{\link{GatingHierarchy}}
#' @param gs a \code{\link{GatingSet}}
#' @param keys the keyword names to insert/delete/replace -- single value or vector
#' @param values the values to associate with the supplied keywords -- single value or vector of sample length as keys
#' @param old_keys the old keyword name (for renaming)
#' @param new_keys the new keyword name (for renaming)
#' 
#' @name keyword-mutators
#' @rdname keyword-mutators
#' @aliases 
#' cf_keyword_insert cf_keyword_rename cf_keyword_delete cf_keyword_set
#' cs_keyword_insert cs_keyword_rename cs_keyword_delete cs_keyword_set
#' gh_keyword_insert gh_keyword_rename gh_keyword_delete gh_keyword_set
#' gs_keyword_insert gs_keyword_rename gs_keyword_delete gs_keyword_set
#' 
#' @details
#' Each of the methods taking two character vectors (keys/values or old_keys/new_keys)
#' will also accept a single named vector for flexibility in usage. 
#' 
#' For the functions that take a vector of keys and a vector of values (the \code{keyword_insert} and \code{keyword_set} functions), 
#' the names of this vector should be the keys to which the values of the vector will be assigned. 
#' 
#' For the \code{keyword_rename} functions, the names of this vector should be the existing keyword names (\code{old_keys})
#' while the values should be the replacement keyword names (\code{new_keys}).
#' 
#' See examples for details
#' 
#' @examples 
#' library(flowCore)
#' data(GvHD)
#' cs <- flowSet_to_cytoset(GvHD[1:2])
#' 
#' keys <- c("CYTNUM", "CREATOR")
#' 
#' # Values before changes
#' keyword(cs, keys)
#' 
#' # Set two keyword values using separate key and values vectors
#' values <- c("E3598", "CELLQuest  3.4")
#' cs_keyword_set(cs, keys, values)
#' 
#' # Values after changes
#' keyword(cs, keys)
#' 
#' # Change the values again using a single named vector
#' values <- c("E3599", "CELLQuest  3.5")
#' names(values) <- keys
#' cs_keyword_set(cs, values)
#' 
#' # Values after changes
#' keyword(cs, keys)
#' 
#' @export
cf_keyword_insert <- function(cf, keys, values){
  if(!is(cf, "cytoframe"))
    stop("cf must be a cytoframe object")
  if(missing(values)){
    if(!is.vector(keys) || is.null(names(keys)) || any(is.na(names(keys))))
      stop("If you are providing a single vector of values, it must have valid names providing the keys")
    values <- keys
    keys <- names(values)
  }
  if(!(is.vector(keys) && is.vector(values) && length(keys) == length(values)))
    stop("keys and values must be vectors of equal length")

  kw <- keyword(cf)
  kn <- names(kw)
  idx <- match(keys, kn)
  dup_idx <- !is.na(idx)
  if(any(dup_idx))
    stop("keywords already exist!:", paste(keys[dup_idx], collapse = ", "))
  cf_setKeywordsSubset(cf@pointer, keys, as.character(values))
}

#' @rdname keyword-mutators
#' @export
cf_keyword_delete <- function(cf, keys){
  if(!is(cf, "cytoframe"))
    stop("cf must be a cytoframe object")
  if(!is.vector(keys))
    stop("keys must be a vector")
  kw <- keyword(cf)
  kn <- names(kw)
  idx <- match(keys, kn)
  na_idx <- is.na(idx)
  if(any(na_idx))
    stop("keyword not found:", paste(keys[na_idx], collapse = ", "))
 	cf_removeKeywords(cf@pointer, keys)
}

#' @rdname keyword-mutators
#' @export
cf_keyword_rename <- function(cf, old_keys, new_keys){
  if(!is(cf, "cytoframe"))
    stop("cf must be a cytoframe object")
  if(missing(new_keys)){
    if(!is.vector(old_keys) || is.null(names(old_keys)) || any(is.na(names(old_keys))))
      stop("If you are providing a single vector of new keys, it must have valid names providing the old keys")
    new_keys <- old_keys
    old_keys <- names(new_keys)
  }
  if(!(is.vector(old_keys) && is.vector(new_keys) && length(old_keys) == length(new_keys)))
    stop("old_keys and new_keys must be vectors of equal length")
  kw <- keyword(cf) 
  kn <- names(kw)
  idx <- match(old_keys, kn)
  if(any(is.na(idx)))
    stop("keyword not found:", paste(old_keys[na_idx], collapse = ", "))
  cf_renameKeywords(cf@pointer, old_keys, new_keys)
}

#' @rdname keyword-mutators
#' @export
cf_keyword_set <- function(cf, keys, values){
  if(missing(values)){
    if(!is.vector(keys) || is.null(names(keys)) || any(is.na(names(keys))))
      stop("If you are providing a single vector of values, it must have valid names providing the keys")
    values <- keys
    keys <- names(values)
  }
  if(!is(cf, "cytoframe"))
    stop("cf must be a cytoframe object")
  if(!(is.vector(keys) && is.vector(values) && length(keys) == length(values)))
    stop("keys and values must be character vectors of equal length")
  cf_setKeywordsSubset(cf@pointer, keys, as.character(values))
}

#' Methods for conversion between flowCore and flowWorkspace data classes
#'
#' These methods perform conversions between flowWorkspace classes (\link{cytoframe}/\link{cytoset}) and 
#' flowCore classes (\link{flowFrame}/\link{flowSet}) as well as between single-sample and aggregated classes
#' (e.g. between \code{cytoset} and a list of \code{cytoframe}s)
#' 
#' The first set of methods consist of a pair of methods to coerce a \code{cytoframe}
#' to or from a \code{flowFrame} and another pair to coerce a \code{cytoset}
#' to or from a \code{flowSet}.
#' 
#' The conversion between the two sets of data container classes mostly entails
#' a conversion of the back-end representation of the data. \code{cytoframe}
#' and \code{cytoset} objects contain \code{flowFrame} and \code{flowSet} objects
#' respectively, so coercion of a \code{cytoframe} to \code{flowFrame} entails
#' moving the data from the 'C'-level data structure to the corresponding
#' \code{exprs}, \code{description}, and \code{parameters} slots. Coercion of
#' a \code{flowFrame} to a \code{cytoframe} entails creation of the 'C'-level
#' data structure from the \code{flowFrame} slots. The names of each of the
#' methods are pretty self-explanatory.
#' 
#' The second set of methods perform disaggregation of data objects that represent
#' multiple samples in to lists of data objects that represent a single sample. The opposite
#' direction is handled by the constructors for the aggregate data classes.
#' 
#' 
#' @section Methods:
#' 
#' \describe{
#' \item{cytoframe_to_flowFrame(object = "cytoframe")}{Returns a \code{flowFrame} object
#' coerced from a \code{cytoframe} object.}
#' 
#' \item{flowFrame_to_cytoframe(object = "flowFrame")}{Returns a \code{cytoframe} object
#' coerced from a \code{flowFrame} object.}
#' 
#' \item{cytoset_to_flowSet(object = "cytoset")}{Returns a \code{flowSet} object
#' coerced from a \code{cytoset} object.}
#' 
#' \item{flowSet_to_cytoset(object = "flowSet")}{Returns a \code{cytoset} object
#' coerced from a \code{flowSet} object.}
#' 
#' \item{flowSet_to_list(object = "flowSet")}{Returns a list of \code{cytoframe} objects
#' with names provided by the sampleNames of the original \code{cytoset}}
#' 
#' \item{flowSet(object = "list)}{Constructs a \code{cytoset} object from a list of \code{cytoframe}
#' objects. See documentation for \link{cytoset}}
#' 
#' \item{cytoset_to_list(object = "cytoset")}{Returns a list of \code{cytoframe} objects
#' with names provided by the sampleNames of the original \code{cytoset}}
#' 
#' \item{cytoset(object = "list)}{Constructs a \code{cytoset} object from a list of \code{cytoframe}
#' objects. See documentation for \link{flowSet}}
#' }
#' 
#' @name convert
#' @aliases cytoframe_to_flowFrame flowFrame_to_cytoframe cytoset_to_flowSet
#' flowSet_to_cytoset cytoset_to_list flowSet_to_list
#' 
#' @docType methods
#' @keywords methods
#' 
#' @examples
#' library(flowCore)
#' data("GvHD")
#' fs <- GvHD[1]
#' cs <- flowSet_to_cytoset(fs)
#' cf <- cs[[1, returnType="cytoframe"]]
#' ff <- cytoframe_to_flowFrame(cf)
#' 
#' @seealso \link{merge_list_to_gs}
#' @param cf cytoframe object 
#' @export
cytoframe_to_flowFrame <- function(cf){
  cf@exprs <- exprs(cf)
  cf@description = keyword(cf)
  cf@parameters <- parameters(cf)
  as(cf, "flowFrame")
}
#can't define coerce method for cytoframe, i.e. SetAS for as(cf, "flowFrame")
#since there is already existing implicit coerce available
#and by doing so it will cause infinite recursive calls

#' @rdname convert
#' @param fr flowframe
#' @param ... arguments passed to 'load_cytoframe_from_fcs' call
#' @export
flowFrame_to_cytoframe <- function(fr, ...){
	tmp <- tempfile()
	write.FCS(fr, tmp)
	load_cytoframe_from_fcs(tmp, ...)
}

#' Save the cytoframe to disk
#' 
#' @param cf cytoframe object
#' @param filename the full path of the output file
#' @param backend either "h5" or "tile"
#' @inheritParams load_cytoframe
#' @family cytoframe/cytoset IO functions
#' @export
cf_write_disk <- function(cf, filename, backend = get_default_backend()){
  backend <- match.arg(backend, c("h5", "tile"))
  stopifnot(is(cf, "cytoframe"))

  write_to_disk(cf@pointer,filename, backend == "h5")
}

#' Save the cytoframe as h5 format
#' 
#' @param cf cytoframe object
#' @param filename the full path of the output h5 file
#' @inheritParams load_cytoframe
#' @family cytoframe/cytoset IO functions
#' @export
cf_write_h5 <- function(cf, filename){
	cf_write_disk(cf, filename, backend = "h5")
}



#' Load the cytoframe from disk
#' 
#' @param uri path to the cytoframe file
#' @param on_disk logical flag indicating whether to keep the data on disk and load it on demand. Default is TRUE.
#' @param readonly logical flag indicating whether to open h5 data as readonly. Default is TRUE.
#'                 And it is valid when on_disk is set to true.
#' @family cytoframe/cytoset IO functions
#' @export
load_cytoframe <- function(uri, on_disk = TRUE, readonly = on_disk){
	if(!on_disk)
	{
	  if(readonly)
	  {
	    stop("'readonly = TRUE' is only valid when 'on_disk' is TRUE! ")
	  }
	}
	uri <- suppressWarnings(normalizePath(uri))
	
	p <- load_cf(uri, readonly, on_disk)
	
	new("cytoframe", pointer = p, use.exprs = TRUE)
}

#' return the cytoframe backend storage format
#' @param cf cytoframe
#' @return one of "mem","h5", "tile"
cf_backend_type <- function(cf)
{
  backend_type(cf@pointer)
}

is_http_path <- function(x){
  grepl("^https://", x, ignore.case = TRUE)
}

#' Return the file path of the underlying h5 file
#' 
#' Return the file path of the underlying h5 file
#' 
#' For the in-memory version of cytoframe, it returns an empty string. This can be used to check whether it is on-disk format.
#' @param cf cytoframe object
#' @family cytoframe/cytoset IO functions
#' @export 
#' @rdname cf_get_uri
cf_get_uri <- function(cf){
  stopifnot(is(cf, "cytoframe"))
  get_uri(cf@pointer)
  
}

#' @rdname cf_get_uri
#' @export 
cf_get_h5_file_path <- function(cf){
	.Deprecated("cf_get_uri")
	cf_get_uri(cf)
	
}
#' Lock/Unlock the cytoset/cytoframe by turning on/off its read-only flag
#' @name lock
#' @param cf cytoframe object
#' @export 
cf_lock <- function(cf){
	stopifnot(is(cf, "cytoframe"))
	cf_set_readonly(cf@pointer, TRUE)
}
#' @export 
#' @rdname lock
cf_unlock <- function(cf){
	stopifnot(is(cf, "cytoframe"))
	cf_set_readonly(cf@pointer, FALSE)
}

#' Flush/load meta data (keywords, pData, channels/markers) to/from disk (only valid for on-disk cytoset/cytoframe)
#' @name load_meta
#' @aliases flush_meta
#' @param cf cytoframe object
#' @export
cf_flush_meta <- function(cf){
	stopifnot(is(cf, "cytoframe"))
	cf_flush_meta_cpp(cf@pointer)
}
#' @export 
#' @rdname load_meta
cf_load_meta <- function(cf){
	stopifnot(is(cf, "cytoframe"))
	cf_load_meta_cpp(cf@pointer)
}

cf_scale_time_channel <- function(cf){
	stopifnot(is(cf, "cytoframe"))
	cf_scale_time_channel_cpp(cf@pointer)
}

#' Remove temporary files associatated with flowWorkspace data classes
#' 
#' These methods immediately delete the on-disk h5 storage associated with \link{cytoframe},
#' \link{cytoset}, \linkS4class{GatingHierarchy}, or \linkS4class{GatingSet} objects, but only if it is
#' under the directory pointed to by tempdir() or alternatively specified by the temp_dir option.
#' The temp_dir option should be used with caution as it acts as a guard against accidental
#' removal of non-temporary storage.
#' 
#' Use of these functions will generally be unnecessary for most users, but they are provided
#' for workflows that involve repeated creation of such data structures within the same R session
#' to avoid overwhelming temporary storage.
#' 
#' @name cleanup_temp
#' @aliases cf_cleanup_temp cs_cleanup_temp gh_cleanup_temp gs_cleanup_temp
#' @param x a cytoframe, cytoset, GatingHierarchy, or GatingSet object
#' @param temp_dir an optional argument designating another path as temporary storage. If specified
#' this will override tempdir() in determining the top directory under which files can safely be removed.
#' @export
cf_cleanup_temp <- function(x, temp_dir = NULL){
	if(is.null(temp_dir))
		temp_dir <- normalizePath(tempdir(), winslash = "/")
	h5_path <- normalizePath(cf_get_uri(x), winslash = "/")
	if(grepl(paste0("^", temp_dir), h5_path))
		unlink(h5_path, recursive = TRUE)
}

#' Remove on-disk files associatated with flowWorkspace data classes
#' 
#' These methods immediately delete the on-disk  storage associated with \link{cytoframe},
#' \link{cytoset}, \linkS4class{GatingHierarchy}, or \linkS4class{GatingSet} objects
#' 
#' @name cleanup
#' @aliases cf_cleanup cs_cleanup gh_cleanup gs_cleanup
#' @param cf a cytoframe, cytoset, GatingHierarchy, or GatingSet object
#' @inheritParams load_cytoframe
#' @details this will override tempdir() in determining the top directory under which files can safely be removed.
#' @export
cf_cleanup <- function(cf){
  uri <- cf_get_uri(cf)
  
  unlink(uri, recursive = TRUE)
  message(uri, " is deleted!")
} 
#' Append data columns to a flowFrame
#' 
#' Append data columns to a flowFrame
#' 
#' It is used to add extra data columns to the existing flowFrame.  It handles
#' keywords and parameters properly to ensure the new flowFrame can be written
#' as a valid FCS through the function \code{write.FCS} .
#' 
#' @name cf_append_cols
#' @param cf A \code{cytoframe}.
#' @param cols A numeric matrix containing the new data columns to be added.
#' Must has column names to be used as new channel names.
#' 
#' @examples
#' 
#'   library(flowCore)
#'   data(GvHD)
#'   tmp <- GvHD[[1]]
#'   cf <- flowFrame_to_cytoframe(tmp)
#'   kf <- kmeansFilter("FSC-H"=c("Pop1","Pop2","Pop3"), filterId="myKmFilter")
#'   fres <- filter(cf, kf)
#'   cols <- as.numeric(fres@subSet)
#'   cols <- matrix(cols, dimnames = list(NULL, "km"))
#'   cf <- cf_append_cols(cf, cols)
#'   
#' 
#' 
#' @export
cf_append_cols <- function(cf, cols){
  stopifnot(typeof(cols) == "double")
  if(cf_is_subsetted(cf))
    stop("Columns cannot be added to subsetted cytoframes. This cytoframe must first be realized with `realize_view()`.\n")
  append_cols(cf@pointer, colnames(cols), cols)
  cf <- new("cytoframe", pointer = cf@pointer, use.exprs = TRUE);

}
