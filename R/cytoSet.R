#' @include cytoFrame.R
NULL

#' @importClassesFrom flowCore flowSet
#' @export 
setClass("cytoSet", contains = "flowSet"
          ,representation=representation(pointer = "externalptr"))

#' @export 
cytoSet_to_flowSet <- function(cs){
  fs <- as(fsApply(cs, function(fr)fr), "flowSet")
  pData(fs) <- pData(cs)
  fs
}

#' @export 
flowSet_to_cytoSet <- function(fs, path = tempdir()){
  tmp <- tempfile()
  write.flowSet(fs, tmp, filename = sampleNames(fs))
  cs <- load_cytoset_from_fcs(phenoData = list.files(tmp, pattern = ".txt")
                              , path = tmp
                              , is_h5 = TRUE
                              , h5_dir = path)
  cs
}
setMethod("pData",
          signature=signature(object="cytoSet"),
          definition=function(object) get_pheno_data(object@pointer))

#' @export 
setReplaceMethod("pData",
                 signature=signature(object="cytoSet",
                                     value="data.frame"),
                 definition=function(object,value)
                 {
                   for(i in seq_along(value))
                     value[[i]] <- as.character(value[[i]])
                   set_pheno_data(object@pointer, value)
                   object
                 })

setMethod("colnames",
          signature=signature(x="cytoSet"),
          definition=function(x, do.NULL="missing", prefix="missing")
            get_colnames(x@pointer))
  
setReplaceMethod("colnames",
	signature=signature(x="cytoSet",
			value="ANY"),
	definition=function(x, value)
	{
       for(i in sampleNames(x))
	   {
         fr <- x[[i, returnType = "cytoFrame"]]
		     colnames(fr) <- value
	   }
         
				
	   x
	})



setMethod("markernames",
    signature=signature(object = "cytoSet"),
    definition=function(object){
      res <- lapply(sampleNames(object), function(sn){
        markernames(object[[sn, returnType = "cytoFrame", use.exprs = FALSE]])
      })
      
      res <- unique(res)
      if(length(res) > 1)
        warning("marker names are not consistent across samples within flowSet")
      else
        res <- res[[1]]
      res
    })
      

#' @export
setReplaceMethod("markernames",
                 signature=signature(object="cytoSet", value="ANY"), function(object, value){
                   for(i in sampleNames(object))
                   {
                     fr <- object[[i, returnType = "cytoFrame", use.exprs = FALSE]]
                     markernames(fr) <- value
                   }
                   object
                 })

setMethod("show",
          signature=signature(object="cytoSet"),
          definition=function(object)
          { 
            cat("A cytoSet with", length(object),"samples.\n")
            
            cat("\n")
            #			}
            cat("  column names:\n  ")
            cat(" ", paste(colnames(object), collapse = ", "))
             cat("\n")
            cat("\n")
            
          })
setMethod("sampleNames",
          signature=signature(object="cytoSet"),
          definition=function(object) 
            rownames(pData(object)))

setMethod("[[",
          signature=signature(x="cytoSet"),
          definition=function(x, i, j,  use.exprs = TRUE, returnType = c("flowFrame", "cytoFrame"))
          {
            returnType <- match.arg(returnType)
            if(missing(j))
              j <- NULL
            
            fr <- get_cytoFrame_from_cs(x, i, j, use.exprs)
            if(returnType == "flowFrame")
              fr <- cytoFrame_to_flowFrame(fr)
            fr
            
          })
#TODO:
#setReplaceMethod("[[",
#	  signature=signature(x="cytoSet",
#			  value="cytoFrame"),
#	  definition=function(x, i, j, ..., value)
#	  {
#		  if(length(i) != 1)
#			  stop("subscript out of bounds (index must have ",
#					  "length 1)")
#		  cnx <- colnames(x)
#		  cnv <- colnames(value)
#		  if(length(cnx) != length(cnv) || !all(sort(cnv) == sort(cnx)))
#		  stop("The colnames of this flowFrame don't match ",
#				  "the colnames of the flowSet.")
#			  
#			  sel <- if(is.numeric(i)) sampleNames(x)[[i]] else i
#			  x@frames[[sel]] <- value
#			  return(x)
#		  })
  
#' @export
get_cytoFrame_from_cs <- function(x, i, j = NULL, use.exprs = TRUE){
  
  new("cytoFrame", pointer = get_cytoFrame(x@pointer, i, j), use.exprs = use.exprs)
}
setMethod("[",
	signature=signature(x="cytoSet"),
	definition=function(x, i, j, ..., drop=FALSE)
	{
  
		if(missing(i))
		  i <- NULL
    if(missing(j))
      j <- NULL
    x <- shallow_copy(x)
    subset_cytoset(x@pointer, i, j)
    x
	})

# Dispatching to the flowSet-version of fsApply by changing simplify default value from TRUE from FALSE
setMethod("fsApply",
    signature=signature(x="cytoSet",
        FUN="ANY"),
    definition=function(x,FUN,...,simplify=FALSE, use.exprs=FALSE)
    {
      callNextMethod()
    })
setMethod("Subset",
          signature=signature(x="cytoSet",
                              subset="filterResultList"),
          definition=function(x, subset, ...)
          {
            flowCore:::validFilterResultList(subset, x, strict=FALSE)
            Subset(x, sapply(subset, function(i)as(i, "logical"), simplify = FALSE))
          })
setMethod("Subset",
          signature=signature(x="cytoSet",
                              subset="filter"),
          definition=function(x, subset, ...)
          {
            fres <- filter(x, subset, ...)
            Subset(x,fres)
          })

setMethod("Subset",
          signature=signature(x="cytoSet",
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
            
            
            cs = shallow_copy(x)
            for(sn in used)
            {
              
              ind <- subset[[sn]]
              if(is(ind, "logical"))
                ind <- which(ind)
              
              if(!is(ind, "integer"))
                stop("Invalid row indices for: ", sn)
              
              subset_cytoset_by_rows(cs@pointer, sn, ind - 1)
            }
              
            cs         
        })
        
#' @export 
shallow_copy.cytoSet <- function(x){
  new("cytoSet", pointer = shallow_copy_cytoset(x@pointer))
}
#' @export 
realize_view.cytoSet <- function(x, filepath = tempfile()){
  if(!dir.exists(filepath))
    dir.create(filepath)
  new("cytoSet", pointer = realize_view_cytoset(x@pointer, filepath))
}

#
#
#
### Note that the replacement method also replaces the GUID for each flowFrame
#setReplaceMethod("sampleNames",
#		signature=signature(object="cytoSet"),
#		definition=function(object, value)
#		{
#			oldNames <- sampleNames(object)
#			value <- as.character(value)
#			if(length(oldNames)!=length(value) ||
#					!is.character(value))
#				stop(" replacement values must be character vector ",
#						"of length equal to number of frames in the set'",
#						call.=FALSE)
#			if(any(duplicated(value)))
#				stop("Replacement values are not unique.", call.=FALSE)
#			env <- new.env(hash=TRUE,parent=emptyenv())
#			for(f in seq_along(oldNames)){
#				tmp <- get(oldNames[f], object@frames)
#				identifier(tmp) <- value[f]
#				assign(value[f], tmp, env)
#			}
#			object@frames <- env
#			return(object)
#		})
#
#
#
# setMethod("fsApply",
# 	signature=signature(x="cytoSet",
# 			FUN="ANY"),
# 	definition=function(x,FUN,...,simplify=TRUE, use.exprs=FALSE)
# 	{
# 		if(missing(FUN))
# 			stop("fsApply function missing")
# 		FUN <- match.fun(FUN)
# 		if(!is.function(FUN))
# 			stop("This is not a function!")
# 
# 		res <- structure(lapply(sampleNames(x),function(n) {
# 							y <- x[[n]]
# 							FUN(if(use.exprs) exprs(y) else y,...)
# 						}),names=sampleNames(x))
# 		if(simplify) {
# 			if(all(sapply(res,is,"cytoFrame"))) {
# 				res <- as(res,"cytoSet")
# 
# 			} else if(all(sapply(res,is.numeric)) || all(sapply(res,is.character)) &&
# 					diff(range(sapply(res,length))) == 0) {
# 				res <- do.call(rbind,res)
# 			}
# 		}
# 		res
# 	})


