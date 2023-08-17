#' @include cytoframe.R
NULL

#' @include cytoframe.R
NULL

#' \code{cytoset}: a reference class for efficiently managing the data representation
#' of a \code{flowSet}
#'
#' This class is a container for a set of \code{\link{cytoframe}} objects, analagous to
#' a \code{\link[flowCore]{flowSet}}. 
#' 
#' Similar to the distinction between the \code{cytoframe} and
#' \code{flowFrame} classes, the primary difference between the \code{cytoset} 
#' and \code{flowSet} classes is in the underlying representation of the data.
#' Because \code{cytoset} is a reference class, copying or subsetting a \code{cytoset}
#' object will return a \code{cytoset} pointing to the same underlying data. A
#' deep copy of the data can be obtained via the \code{realize_view} method.
#' 
#' There is one notable exception to the typical behavior of most methods returning a \code{cytoframe}.
#' The standard extraction operator (\code{[[]]}) will by default perform a deep
#' copy of the subset being extracted and return a \code{flowFrame}. This is for the sake of compatibility
#' with existing user scripts.
#' 
#' @name cytoset
#' @aliases cytoset-class realize_view,cytoset-method get_cytoframe_from_cs [,cytoset,ANY-method
#' [[,cytoset,ANY-method [[<-,cytoset,ANY,ANY,flowFrame-method identifier,cytoset-method
#' identifier<-,cytoset,ANY-method pData,cytoset-method pData<-,cytoset,data.frame-method
#' phenoData,cytoset-method phenoData<-,cytoset,ANY-method sampleNames<-,cytoset,ANY-method
#' show,cytoset-method transform,cytoset-method gs_get_cytoframe cs_get_cytoframe
#' @docType class
#'
#' @section Creating Objects:
#' 
#' Objects can be created using \code{cytoset()} and then adding samples
#' by providing a \code{cytoframe} and sample name to \code{cs_add_cytoframe}:\cr\cr
#' 
#' \preformatted{
#' cs <- cytoset()
#' cs_add_cytoframe(cs, "Sample Name", cytoframe)
#' }
#' 
#' The safest and easiest way to create \code{cytoset}s directly from
#' \acronym{FCS} files is via the \code{\link{load_cytoset_from_fcs}} function, and
#' there are alternative ways to specify the files to read. See the separate
#' documentation for details.
#' 
#' @section Methods:
#'   \describe{
#' 
#' \item{[, [[}{Subsetting. \code{x[i]} where \code{i} is a scalar,
#'   returns a \code{cytoset} object, and \code{x[[i]]} a
#'   \code{\linkS4class{flowFrame}} object. In this respect the
#'   semantics are similar to the behavior of the subsetting operators
#'   for lists. \code{x[i, j]} returns a \code{cytoset} for which the
#'   parameters of each \code{\linkS4class{cytoframe}} have been subset
#'   according to \code{j}, \code{x[[i,j]]} returns the subset of a
#'   single \code{\linkS4class{flowFrame}} for all parameters in
#'   \code{j}.\cr\cr
#'   The reason for the default behavior of the extraction operator \code{[[]]}
#'   returning a \code{flowFrame} rather than \code{cytoframe}
#'   is for backwards compatibility with existing user scripts. This behavior
#'   can be overridden to instead return a \code{cytoframe} with the additional
#'   \code{returnType} argument.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   cytoset[i]}\cr\cr
#'   \code{   cytoset[i,j]}\cr\cr
#'   \code{   cytoset[[i]]}\cr\cr
#'   \code{cytoset[[i, returnType = "cytoframe"]]}\cr\cr
#' }
#' 
#' \item{get_cytoframe_from_cs}{
#'   Extract a \code{cytoframe} from a \code{cytoset} by supplying either
#'   a sample name or index and optionally supplying a subset of columns.
#'   
#'   The \code{cytoframe} to be extracted (\code{i} argument) can be specified using its sample name (character)
#'   or index in the cytoset (int/numeric). Columns (\code{j} argument) can be specified using channel name (character),
#'   index (int/numeric), or logical vector. If this argument is missing, all columns will be selected.
#'   
#'   \emph{Usage:}\cr\cr
#'   (Assuming \code{cs} is a \code{cytoset} and \code{cf} is the extracted \code{cytoframe})
#'   \code{cf <- get_cytoframe_from_cs(cs, i, j)}
#'   \code{cf <- get_cytoframe_from_cs(cs, i)}
#' }
#' 
#' \item{$}{Subsetting by frame name. This will return a single
#'   \code{\linkS4class{cytoframe}} object. Note that names may have to
#'   be quoted if they are not valid R symbols
#'   (e.g. \code{cytoset$"sample 1"}).\cr\cr
#' }
#' 
#' \item{colnames, colnames<-}{Extract or replace
#'   the \code{character} object with the (common)
#'   column names of all the data matrices in the
#'   \code{\link{cytoframe}s}.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   colnames(cytoset)}\cr\cr
#'   \code{   colnames(cytoset) <- value}\cr\cr
#' }
#' 
#' \item{identifier, identifier<-}{Extract or replace the \code{name}
#'   item from the environment.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   identifier(cytoset)}\cr\cr
#'   \code{   identifier(cytoset) <- value}\cr\cr
#' }
#' 
#' 
#' \item{phenoData, phenoData<-}{Extract or replace the
#'   \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame}}
#'   containing the phenotypic data for the whole data set. Each row
#'   corresponds to one of the \code{\link{cytoframe}}s.  
#'   The \code{sampleNames} of \code{phenoData}
#'   (see below) must match the names of the
#'   \code{cytoframe}s in the \code{frames} environment.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   phenoData(cytoset)}\cr\cr
#'   \code{   phenoData(cytoset) <- value}\cr\cr
#' }
#' 
#' \item{pData, pData<-}{Extract or replace the data frame (or columns
#'   thereof) containing actual phenotypic information from the
#'   \code{phenoData} of the underlying data.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   pData(cytoset)}\cr\cr
#'   \code{   pData(cytoset)$someColumn <- value}\cr\cr
#' }
#' 
#' \item{varLabels, varLabels<-}{ \strong{Not yet implemented.}\cr
#'   Extract and set \code{varLabels} in the \code{\link[Biobase]{AnnotatedDataFrame}}
#'   of the \code{phenoData} of the underyling data.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   varLabels(cytoset)}\cr\cr
#'   \code{   varLabels(cytoset) <- value}\cr\cr
#' }
#' 
#' \item{sampleNames}{Extract and replace sample names from the
#'   \code{phenoData}. Sample names correspond to frame
#'   identifiers, and replacing them will also replace the \code{GUID}
#'   for each cytoframe. Note that each sample name needs to be
#'   unique.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   sampleNames(cytoset)}\cr\cr
#'   \code{   sampleNames(cytoset) <- value}\cr\cr
#' }
#' 
#' \item{keyword}{Extract or replace keywords specified in a character
#'   vector or a list from the \code{description} slot of each
#'   frame. See \code{\link{keyword}} for details.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   keyword(cytoset, list(keywords))}\cr\cr
#'   \code{   keyword(cytoset, keywords)}\cr\cr
#'   \code{   keyword(cytoset) <- list(foo="bar") }\cr\cr
#' }
#' 
#' \item{length}{The number of \code{\link{cytoframe}} objects in
#'   the set.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   length(cytoset)}\cr\cr
#' }
#' 
#' \item{show}{display object summary.\cr\cr}
#' 
#' \item{summary}{Return descriptive statistical summary (min, max,
#'   mean and quantile) for each channel of each \code{\link{cytoframe}}.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   summary(cytoset)}\cr\cr
#' }
#' 
#' 
#' \item{fsApply}{Apply a function on all frames in a \code{cytoset}
#'   object. Similar to \code{\link{sapply}}, but with additional
#'   parameters. See \code{\link[flowCore]{fsApply}} for details.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   fsApply(cytoset, function, ...)}\cr\cr
#'   \code{   fsApply(cytoset, function, use.exprs=TRUE, ...)}\cr\cr
#' }
#' 
#' \item{compensate}{Apply a compensation matrix on all frames in a
#'   \code{cytoset} object. See \code{\link{compensate}} for details.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   compensate(cytoset, matrix)}\cr\cr
#' }
#' 
#' \item{transform}{Apply a transformation function on all frames of a
#'   \code{cytoset} object. See \code{\link{transform}} for details.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   transform(cytoset, ...)}\cr\cr
#' }
#' 
#' \item{filter}{Apply a filter on a \code{cytoset}
#'   object. There are methods for \code{\link[flowCore:filter-class]{filter}} objects,
#'   and lists of \code{filter} objects. The latter has to
#'   be a named list, where names of the list items are matching
#'   the \code{sampleNames} of the \code{cytoset}. See \code{\link[flowCore]{filter}}
#'   for details.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   filter(cytoset, filter)}\cr\cr
#'   \code{   filter(cytoset, list(filters))}\cr\cr
#' }
#' 
#' \item{split}{Split all \code{cytoframe} objects according to a
#'   \code{\link[flowCore:filter-class]{filter}}, \code{\link[flowCore]{filterResult}} or a list of such
#'   objects, where the length of the list has to be the same as the
#'   length of the \code{cytoset}. This returns a list of
#'   \code{\link{cytoframe}}s or an object of class
#'   \code{cytoset} if the \code{flowSet} argument is set to
#'   \code{TRUE}. Alternatively, a \code{cytoset} can be split into
#'   separate subsets according to a factor (or any vector that can be
#'   coerced into a factor), similar to the behaviour of
#'   \code{\link[base]{split}} for lists. This will return a list of
#'   \code{cytoset}s. See \code{\link[flowCore]{split}} for details.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   split(cytoset, filter)}\cr\cr
#'   \code{   split(cytoset, filterResult)}\cr\cr
#'   \code{   split(cytoset, list(filters))}\cr\cr
#'   \code{   split(cytoset, factor)}\cr\cr
#' }
#' 
#' \item{Subset}{Returns a \code{cytoset} of
#'   \code{\linkS4class{cytoframe}}s that have been subset according
#'   to a \code{\link[flowCore:filter-class]{filter}} or
#'   \code{\link[flowCore]{filterResult}}, or according to a list of such
#'   items of equal length as the \code{cytoset}. See \code{\link[flowCore]{Subset}}
#'   for details.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   Subset(cytoset, filter)}\cr\cr
#'   \code{   Subset(cytoset, filterResult)}\cr\cr
#'   \code{   Subset(cytoset, list(filters))}\cr\cr
#' }
#' 
#' 
#' \item{rbind2}{\strong{Not yet implemented.}\cr Combine two \code{cytoset} objects, or one
#'   \code{cytoset} and one \code{\link{cytoframe}} object.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{   rbind2(cytoset, cytoset)}\cr\cr
#'   \code{   rbind2(cytoset, cytoframe)}\cr\cr
#' }
#' 
#' \item{spillover}{Compute spillover matrix from a compensation
#'   set. See \code{\link[flowStats]{spillover}} for details.
#' }
#' 
#' \item{realize_view}{Returns a new \code{cytoset} with its own copy of the
#' underlying data (a deep copy). The optional \code{filepath} argument accepts
#' a string to specify a full directory name for storing the new copies of the data 
#' from the FCS files in h5 format.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{realize_view(cytoset, filepath)}\cr\cr
#' }
#' \item{cs_add_cytoframe}{Adds a \code{cytoframe} to the \code{cytoset} with sample name given
#' by a string.\cr\cr
#'   \emph{Usage:}\cr\cr
#'   \code{cs_add_cytoframe(cytoset, "SampleName", cytoframe)}\cr\cr
#' }
#' }
#'   
#' @importClassesFrom flowCore flowSet
#' @export 
setClass("cytoset", contains = "flowSet"
          ,representation=representation(pointer = "externalptr"))

#' @export 
cytoset <- function(x, ...){
	
	cs <- new("cytoset", pointer = new_cytoset())
	if(!missing(x))
	{
		if(is.list(x))
		{
			samplenames <- names(x)
			if(length(samplenames) == 0)
				stop("input is not a named list!")
			for(i in samplenames)
				cs_add_cytoframe(cs, i, x[[i]])
		}else
			stop("input is not a list!")
	}
	cs
}

#' @rdname convert
#' @export 
cytoset_to_flowSet <- function(cs){
  fs <- as(fsApply(cs, function(fr)fr), "flowSet")
  pData(fs) <- pData(cs)
  fs
}

#' @rdname convert
#' @param fs flowSet or ncdfFlowSet
#' @param path the h5 path for cytoset
#' @param tmp the temp folder when the temporary files are written to during
#'   conversion by default, it is system temp path. And it can be changed to the
#'   customized location when there is not enough space at system path.
#' @param ... additional arguments passed to
#'   \code{\link{load_cytoframe_from_fcs}} or
#'   \code{\link{load_cytoset_from_fcs}}.
#' @export
flowSet_to_cytoset <- function(fs,
                               path = tempfile(),
                               backend = get_default_backend(),
                               tmp = tempfile(),
                               ...){
  backend <- match.arg(backend, c("h5", "mem",  "tile"))
  # Set up mapping to ensure that the sampleNames 
  # come back in without additional ".fcs" and allow
  # for potential re-ordering
  sns <- sampleNames(fs)
  filenames <- sns

  write.flowSet(fs, tmp, filename = sampleNames(fs))
  cs <- load_cytoset_from_fcs(phenoData = list.files(tmp, pattern = ".txt")
                              , path = tmp
                              , backend = backend
                              , backend_dir = path
                              , file_col_name = "FCS_File"
                              , check.names = FALSE,
                              ...)
  # Remove the temporary intermediate flowSet
  unlink(normalizePath(tmp), recursive = TRUE)
  # Fix any potential change or re-ordering of sampleNames
  sns_matched <- sampleNames(cs)
  sns_matched <- sns[match(sns_matched, filenames)]
  sampleNames(cs) <- sns_matched
  cs
}
#' @export
setMethod("phenoData",
		signature=signature(object="cytoset"),
		definition=function(object){
			df <- pData(object)
			cols <- colnames(df)
			new("AnnotatedDataFrame",
					data=df,
					varMetadata=data.frame(labelDescription=cols,
							row.names=cols))
		})

#' @export
setMethod("phenoData<-",
		signature=signature(object="cytoset",
				value="ANY"),
		definition=function(object, value)
		{
			current <- phenoData(object)
			## Sanity checking
			if(nrow(current) != nrow(value))
				stop("phenoData must have the same number of rows as ",
						"flow files")
			## Make sure all of the original frames appear in the new one.
			if(!all(sampleNames(current)%in%sampleNames(value)))
				stop("The sample names no longer match.")
			#validity check for 'name' column
			df <- pData(value)
			if(!"name" %in% colnames(df))
				pData(value)[["name"]] = rownames(df)
			
			
			pData(object) <- df
			object
		})
#' @export 
setMethod("pData",
          signature=signature(object="cytoset"),
          definition=function(object) {
            pd = get_pheno_data(object@pointer)
             #remedy for dealing cpp1 bug see https://github.com/r-lib/cpp11/issues/206
            cn <- names(pd)
            names(pd) <- cn[cn!=""]
            pd
          })

#' @export 
setReplaceMethod("pData",
                 signature=signature(object="cytoset",
                                     value="data.frame"),
                 definition=function(object,value)
                 {
                   for(i in seq_along(value))
                     value[[i]] <- as.character(value[[i]])
                   set_pheno_data(object@pointer, value)
                   object
                 })

setMethod("colnames",
          signature=signature(x="cytoset"),
          definition=function(x, do.NULL="missing", prefix="missing")
           {
            if(length(x) == 0)
              character()
            else
            get_colnames(x@pointer)
          }
          )
  
setReplaceMethod("colnames",
	signature=signature(x="cytoset",
			value="ANY"),
	definition=function(x, value)
	{
       for(i in sampleNames(x))
	   {
         fr <- x[[i, returnType = "cytoframe"]]
		     colnames(fr) <- value
	   }
         
				
	   x
	})

#' @rdname cytoframe-labels
#' @export 
cs_swap_colnames <- function(x, col1, col2){
	invisible(lapply(x, cf_swap_colnames, col1, col2))
	
}
setMethod("markernames",
    signature=signature(object = "cytoset"),
    definition=function(object){
      res <- lapply(sampleNames(object), function(sn){
        markernames(object[[sn, returnType = "cytoframe", use.exprs = FALSE]])
      })
      
      res <- unique(res)
      if(length(res) > 1)
        warning("marker names are not consistent across samples within cytoset")
      else
        res <- res[[1]]
      res
    })
      

#' @export
setReplaceMethod("markernames",
                 signature=signature(object="cytoset", value="ANY"), function(object, value){
                   for(i in sampleNames(object))
                   {
                     fr <- object[[i, returnType = "cytoframe", use.exprs = FALSE]]
                     markernames(fr) <- value
                   }
                   object
                 })

#' @rdname is_subsetted
#' @export
cs_is_subsetted <- function(x){
  any(unlist(lapply(x, cf_is_subsetted)))
}

setMethod("show",
          signature=signature(object="cytoset"),
          definition=function(object)
          { 
            cat("A cytoset with", length(object),"samples.\n")
            
            cat("\n")
            #			}
            cat("  column names:\n  ")
            cat(" ", paste(colnames(object), collapse = ", "))
             cat("\n")
            cat("\n")
            if(cs_is_subsetted(object))
            {
              cat("cytoset has been subsetted and can be realized through 'realize_view()'.\n")  
            }
            
            
          })
setMethod("sampleNames",
          signature=signature(object="cytoset"),
          definition=function(object) 
            rownames(pData(object)))

setMethod("[[",
          signature=signature(x="cytoset"),
          definition=function(x, i, j,  use.exprs = TRUE, returnType = c("cytoframe", "flowFrame"))
          {
            
            returnType <- match.arg(returnType)
            if(missing(j))
              j <- character()
            if(!is.character(j))
              j <- colnames(x)[j]

            if(length(i) != 1||(is.numeric(i)&&i<=0))
            	stop("subscript out of bounds (index must have length 1 and be positive)")
            if(!is.character(i))
              i <- sampleNames(x)[i]
            fr <- get_cytoframe_from_cs(x, i, j, use.exprs)
            if(returnType == "flowFrame")
              fr <- cytoframe_to_flowFrame(fr)
            fr
            
          })
#TODO: how to clean up on-disk h5 after replacement with new cf
setReplaceMethod("[[",
	  signature=signature(x="cytoset",
			  value="flowFrame"),
	  definition=function(x, i, j, ..., value)
	  {
	    
	  	if(length(i) != 1 || i <= 0)
	  		stop("subscript out of bounds (index must have length 1 and be positive)")
		  cnx <- colnames(x)
		  cnv <- colnames(value)
		  if(length(cnx) != length(cnv) || !all(sort(cnv) == sort(cnx)))
			  stop("The colnames of this cytoframe don't match ",
					  "the colnames of the cytoset.")
			  
			  sel <- if(is.numeric(i)) sampleNames(x)[[i]] else i
			  cf <- get_cytoframe_from_cs(x, sel)
			  parameters(cf) <- parameters(value)
			  keyword(cf) <- keyword(value)
			  exprs(cf) <- exprs(value)
			  return(x)
		  })
  

  
  
#' @export
setMethod("compensate", signature=signature(x="cytoset", spillover="ANY"),
  definition=function(x, spillover){
	  samples <- sampleNames(x)
	  
	  if(!is.list(spillover)||is.data.frame(spillover)){
		  spillover <- sapply(samples, function(guid)spillover, simplify = FALSE)
	  }
	  #can't use NextMethod() for x could be gs due to manual dispatching S4 from compensate method
	  selectMethod("compensate", signature=c(x="cytoset", spillover="list"))(x, spillover)
	  
  })

check_comp <- function(compensation){
	if(is(compensation, "compensation")){
		compensation <- compensation@spillover
	}else if(is.data.frame(compensation)){
		compensation <- as.matrix(compensation)
	}
	if(!is.matrix(compensation))
		stop("'compensation' should be a compensation object, matrix or data.frame!")
	compensation
}

#' @export
setMethod("compensate", signature=signature(x="cytoset", spillover="list"),#explicitly define this to avoid dispatching (cs, list) to (flowSet,list)
          definition=function(x, spillover){
            spillover <- sapply(spillover, check_comp, simplify = FALSE)
            
            suppressMessages(cs_set_compensation(x@pointer, spillover, TRUE))
            x
          })

#' @export
setMethod("transform",
	  signature=signature(`_data`="cytoset"),
	  definition=function(`_data`, translist,...)
	  {
		  if(missing(translist))
			  stop("Missing the second argument 'translist'!")
		  else if(is(translist, "transformList"))
		  {
			  translist <- sapply(sampleNames(`_data`), function(obj)translist, simplify = FALSE)
		  }
	    if(is(translist, "list"))
	    {
	      tList <- lapply(translist, function(trans){
	        if(!is(trans, "transformList"))
	          stop("All the elements of 'translist' must be 'transformList' objects!")
	        })
	      sns <- sampleNames(`_data`)
	      if(!setequal(sns, names(translist)))
	        stop("names of 'translist' must be consistent with flow data!")
	      for(sn in sampleNames(`_data`))
	      {
	        cf <- get_cytoframe_from_cs(`_data`, sn)
	        transform(cf, translist[[sn]], ...)
	      }
      }else
			  stop("expect the second argument as a 'transformList' object or a list of 'transformList' objects!")
		  

		  
		  `_data`
	  })
setMethod("identifier",
		signature=signature(object="cytoset"),
		definition=function (object)
		{
			get_gatingset_id(object@pointer)
		})

# TODO: define its behavior and handle the h5 issue of "unable to truncate a file which is already open"
# csApply <- function(x,FUN,..., new = FALSE)
# 		{
# 			
# 			if(missing(FUN))
# 				stop("csApply function missing")
# 			FUN <- match.fun(FUN)
# 			if(!is.function(FUN))
# 				stop("This is not a function!")
# 			cs.new <- cytoset()
# 			if(new)
# 			{
# 				h5_dir <- identifier(x)
# 				dir.create(h5_dir)
# 			}else
# 			{
# 				h5_dir <- cs_get_h5_file_path(x)
# 				if(h5_dir=="")
# 					stop("in-memory version of cytoset is not supported!")
# 				
# 			}
# 				
# 			for(n in sampleNames(x))
# 			{
# 				fr <- x[[n]]
# 				fr <- try(
# 						FUN(fr,...)
# 				)
# 				if(is(fr, "try-error"))
# 					stop("failed on sample: ", n)
# 				else if(!is(fr, "cytoframe"))
# 				{
# 					
# 					fr <- flowFrame_to_cytoframe(fr, is_h5 = TRUE, h5_filename = file.path(h5_dir, n))
# 				}
# 					
# 				
# 				if(new)
# 					cs_add_cytoframe(cs.new, n, fr)
# 				else
# 					x[[n]]<- fr
# 			}           
# 			if(new)
# 				cs.new
# 			else
# 				x
# 		}

#' Add a cytoframe to a cytoset
#' 
#' @param cs cytoset
#' @param sn sample name to be added
#' @param cf cytoframe to be added
#' @export
cs_add_cytoframe <- function(cs, sn, cf){
	stopifnot(is(cs, "cytoset"))
	add_cytoframe(cs@pointer, sn, cf@pointer)
}

#' update a cytoframe in a cytoset
#' 
#' @param cs cytoset
#' @param sn sample name 
#' @param cf cytoframe 
#' @export
cs_set_cytoframe <- function(cs, sn, cf){
	stopifnot(is(cs, "cytoset"))
	set_cytoframe(cs@pointer, sn, cf@pointer)
}
#' Return the path of the underlying data files
#' 
#' @family cytoframe/cytoset IO functions
#' @export
#' @rdname cs_get_uri  
cs_get_uri <- function(x){
	stopifnot(is(x, "cytoset")||is(x, "GatingSet"))
	cf <- get_cytoframe_from_cs(x, 1)
	h5file <- cf_get_uri(cf)
	dirname(h5file)
	
}

#' @export
#' @rdname cs_get_uri
cs_get_h5_file_path <- function(x){
	.Deprecated("cs_get_uri")
	
}

#' @export
cs_get_cytoframe <- function(x, i, j = character(), use.exprs = TRUE){
	stopifnot(is(x, "cytoset")||is(x, "GatingSet"))
  if(length(x) == 0)
    stop("Empty cytoset!")
  if(!is.character(i))
      i <- sampleNames(x)[i]
  if(!is.character(j))
      j <- colnames(x)[j]

  new("cytoframe", pointer = get_cytoframe(x@pointer, i, j), use.exprs = use.exprs)
}
#' @export
get_cytoframe_from_cs <- cs_get_cytoframe

setMethod("[",
	signature=signature(x="cytoset"),
	definition=function(x, i, j, ..., drop=FALSE)
	{
    if(length(x) == 0)
      stop("Empty cytoset!")
		if(missing(i))
		  i <- character()
		else if(any(i < 0)){
			if(!all(i <= 0)){
				stop("Cannot mix positive and negative subscripts")
			}
			i <- (1:length(x))[i]
		}
		if(!is.character(i))
      i <- sampleNames(x)[i]

    if(missing(j))
      j <- character()
    if(is.numeric(j)||is.integer(j)){
      if(any(j < 0)){
        if(!all(j <= 0)){
          stop("Cannot mix positive and negative subscripts")
        }
        j <- (1:length(colnames(x)))[j]
      }
    }
    if(!is.character(j))
      j <- colnames(x)[j]

    x <- copy_view(x)
    subset_cytoset(x@pointer, i, j)
    x
	})

# Dispatching to the flowSet-version of fsApply by changing simplify default value from TRUE from FALSE
setMethod("fsApply",
    signature=signature(x="cytoset",
        FUN="ANY"),
    definition=function(x,FUN,...,simplify=FALSE, use.exprs=FALSE)
    {
		if(length(x) == 0)
			stop("Empty cytoset!")
      callNextMethod()
    })
setMethod("Subset",
          signature=signature(x="cytoset",
                              subset="filterResultList"),
          definition=function(x, subset, ...)
          {
            flowCore:::validFilterResultList(subset, x, strict=FALSE)
            Subset(x, sapply(subset, function(i)as(i, "logical"), simplify = FALSE))
          })
setMethod("Subset",
          signature=signature(x="cytoset",
                              subset="filter"),
          definition=function(x, subset, ...)
          {
            fres <- filter(x, subset, ...)
            Subset(x,fres)
          })

setMethod("Subset",
          signature=signature(x="cytoset",
                              subset="list"),
          definition=function(x, subset, select, validityCheck = TRUE, ...)
          {
            if(is.null(names(subset)))
              stop("Filter list must have names to do something reasonable")
            nn <- names(subset)
            if(validityCheck)
            {
              
              sn <- sampleNames(x)
              unused <- nn[!(nn %in% sn)]
              notfilter <- sn[!(sn %in% nn)]
              ##Do some sanity checks
              if(length(unused) > 0)
                warning(paste("Some filters were not used:\n",
                              paste(unused,sep="",collapse=", ")), call.=FALSE)
              if(length(notfilter) > 0)
                warning(paste("Some frames were not filtered:\n",
                              paste(notfilter,sep="",collapse=", ")),
                        .call=FALSE)	
              if(length(x) != length(subset))
                stop("You must supply a list of the same length as the ncdfFlowSet.")
              used <- nn[nn %in% sn]
            }else
              used <- nn
            
            
            cs = copy_view(x)
            for(sn in used)
            {
              
              ind <- subset[[sn]]
              if(is(ind, "logical"))
                ind <- which(ind)
              
              if(!is(ind, "integer"))
                stop("Invalid row indices for: ", sn)
              
              subset_cytoset_by_rows(cs@pointer, sn, as.integer(ind - 1))
            }
              
            cs         
        })
# copied from subset.gatingSet        
#' @export 
subset.cytoset <- function (x, subset, ...) 
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
	
	x[as.character(rownames(pd[r,,drop = FALSE ]))]
}

copy_view.cytoset <- function(x){
  new("cytoset", pointer = copy_view_cytoset(x@pointer))
}
#' @export 
realize_view.cytoset <- function(x, filepath = tempdir()){
  if(!dir.exists(filepath))
    dir.create(filepath)
  new("cytoset", pointer = realize_view_cytoset(x@pointer, filepath))
}


setMethod("nrow",
		signature=signature(x="cytoset"),
		definition=function(x)
			lapply(x, nrow)
)

## Note that the replacement method also replaces the GUID for each flowFrame
setReplaceMethod("sampleNames",
	signature=signature(object="cytoset"),
	definition=function(object, value)
	{
		selectMethod("sampleNames<-", signature = "GatingSet")(object, value)
		return(object)
	})

#' @export
setMethod("lapply","cytoset",function(X,FUN,...){
  sapply(sampleNames(X),function(sn,...){
    cf <- get_cytoframe_from_cs(X, sn)
    FUN(cf, ...)
  }, simplify = FALSE, ...)
  
  
})

#' @param cs cytoset object
#' @export 
#' @rdname lock
cs_lock <- function(cs){
	invisible(lapply(cs, cf_lock))
  }
#' @export 
#' @rdname lock
cs_unlock <- function(cs){
	invisible(lapply(cs, cf_unlock))
}

#' @param cs cytoset object
#' @export 
#' @rdname load_meta
cs_flush_meta <- function(cs){
	invisible(lapply(cs, cf_flush_meta))
}
#' @export 
#' @rdname load_meta
cs_load_meta <- function(cs){
	invisible(lapply(cs, cf_load_meta))
}

#' save/load a cytoset to/from disk.
#'
#' load_cytoset() can load a cytoset from either the archive previously saved by save_cytoset() call
#' or from a folder that contains a collection of inidivudal cytoframe files (either in h5 format or tiledb format)
#'
#' @param cs A \code{cytoset}
#' @param path A character scalar giving the path to save/load the cytoset to/from.
#' @param ... other arguments passed to \code{save_gs/load_gs}
#'
#'
#' @return
#' \code{load_cytoset} returns a cytoset object
#'
#'
#' @examples
#' \dontrun{
#' 	#cs is a cytoset
#' 	save_cytoset(cs, outdir)
#' 	cs <-load_cytoset(outdir)
#'
#' #or from cytoframe on-disk files
#' # e.g. h5_dir contains the cytoframes in h5 format
#' cs <- load_cytoset(h5_dir)
#'
#' }
#' @rdname save_cytoset
#' @export
save_cytoset <-function(cs, path, ...){
  suppressMessages(res <- try(save_gs(cs, path, ...), silent = TRUE))
  if(class(res) == "try-error")
  {
    res <- gsub(" GatingSet", ' cytoset', res)
    stop(res[[1]])
  }
  message("Done\nTo reload it, use 'load_cytoset' function\n")
  
  
}


#' @rdname save_cytoset
#' @param verbose whether to print details. Default is FALSE.
#' @export
load_cytoset<-function(path, verbose = FALSE, ...){
  files <- list.files(path, full.names = TRUE)
  if(any(grepl(".gs$", files)))
  {
    gs <- load_gs(path, ...)
    cs <- gs_cyto_data(gs)
    identifier(cs) <- identifier(gs)#preserve id  
  }else
  {
    #load from individual cytoframe files
    cflist <- sapply(files, function(i){
      message("loading :", i)
      load_cytoframe(i, ...)
    })
    #drop dir
    names(cflist) <- basename(names(cflist))
    cs <- cytoset(cflist)
  }
  
  cs
}

#' @rdname cleanup_temp
#' @export
cs_cleanup_temp <- function(x, temp_dir = NULL){
	if(is.null(temp_dir))
		temp_dir <- normalizePath(tempdir(), winslash = "/")
	h5_path <- normalizePath(cs_get_uri(x), winslash = "/")
	if(grepl(paste0("^", temp_dir), h5_path))
	   unlink(h5_path, recursive = TRUE)
}


#' @rdname convert
#' @param cs cytoset
#' @export
cytoset_to_list <- function(cs){
	cfs <- lapply(1:length(cs), function(idx) {cs[[idx, returnType="cytoframe"]]})
	names(cfs) <- sampleNames(cs)
	cfs
}

#' @rdname keyword-mutators
#' @export
cs_keyword_insert <- function(cs, keys, values){
  if(!is(cs, "cytoset"))
    stop("cs must be a cytoset object")
  if(missing(values)){
    if(!is.vector(keys) || is.null(names(keys)) || any(is.na(names(keys))))
      stop("If you are providing a single vector of values, it must have valid names providing the keys")
    values <- keys
    keys <- names(values)
  }
  if(!(is.vector(keys) && is.vector(values) && length(keys) == length(values)))
    stop("keys and values must be vectors of equal length")
  all_keys <- Reduce(union, lapply(cs, function(cf){
    names(keyword(cf))
  }))
  idx <- match(keys, all_keys)
  dup_idx <- !is.na(idx)
  if(any(dup_idx))
    stop("keywords already exist in one or more cytoframes!:", paste(keys[dup_idx], collapse = ", "))
  for(idx in seq_along(cs))
    cf_setKeywordsSubset(cs[[idx]]@pointer, keys, as.character(values))
}

#' @rdname keyword-mutators
#' @export
cs_keyword_delete <- function(cs, keys){
  if(!is(cs, "cytoset"))
    stop("cs must be a cytoset object")
  if(!is.vector(keys))
    stop("keys must be a vector")
  all_keys <- lapply(cs, function(cf){
    names(keyword(cf))
  })
  invisible(lapply(all_keys, function(cf_keys) {
    idx <- match(keys, cf_keys)
    na_idx <- is.na(idx)
    if(any(na_idx))
      stop("keywords not found in one or more cytoframes:", paste(keys[na_idx], collapse = ", "))
  }))
  for(idx in seq_along(cs))
    cf_removeKeywords(cs[[idx]]@pointer, keys)
}

#' @rdname keyword-mutators
#' @export
cs_keyword_rename <- function(cs, old_keys, new_keys){
  if(!is(cs, "cytoset"))
    stop("cs must be a cytoset object")
  if(missing(new_keys)){
    if(!is.vector(old_keys) || is.null(names(old_keys)) || any(is.na(names(old_keys))))
      stop("If you are providing a single vector of values, it must have valid names providing the keys")
    new_keys <- old_keys
    old_keys <- names(new_keys)
  }
  if(!(is.vector(old_keys) && is.vector(new_keys) && length(old_keys) == length(new_keys)))
    stop("old_keys and new_keys must be vectors of equal length")
  all_keys <- lapply(cs, function(cf){
    names(keyword(cf))
  })
  invisible(lapply(all_keys, function(cf_keys) {
    idx <- match(old_keys, cf_keys)
    na_idx <- is.na(idx)
    if(any(na_idx))
      stop("keyword not found in one or more cytoframes:", paste(keys[na_idx], collapse = ", "))
  }))
  for(idx in seq_along(cs))
    cf_renameKeywords(cs[[idx]]@pointer, old_keys, new_keys)
}

#' @rdname keyword-mutators
#' @export
cs_keyword_set <- function(cs, keys, values){
  if(!is(cs, "cytoset"))
    stop("cs must be a cytoset object")
  if(missing(values)){
    if(!is.vector(keys) || is.null(names(keys)) || any(is.na(names(keys))))
      stop("If you are providing a single vector of values, it must have valid names providing the keys")
    values <- keys
    keys <- names(values)
  }
  if(!(is.vector(keys) && is.vector(values) && length(keys) == length(values)))
    stop("keys and values must be character vectors of equal length")
  for(idx in seq_along(cs))
    cf_setKeywordsSubset(cs[[idx]]@pointer, keys, as.character(values))
}