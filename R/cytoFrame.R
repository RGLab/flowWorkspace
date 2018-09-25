#' @importClassesFrom flowCore flowFrame
#' @export 
setClass("cytoFrame", contains = "flowFrame" ,               
    representation=representation(pointer = "externalptr"
                                  , use.exprs = "logical" #for the purpose  of backward compatible (e.g. fs[[1, use.exprs = F]]
                                  ),
                                  prototype = list(use.exprs = FALSE) 
                              )

#' @import flowCore 
setMethod("spillover",
    signature=signature(x="cytoFrame"),
    definition=function(x, key = "SPILL")
    {
      
      get_spillover(x@pointer, key)
    })

setMethod("compensate",
    signature=signature(x="cytoFrame",
        spillover="matrix"),
    definition=function(x, spillover)
    {
      frm_compensate(x@pointer, spillover)
      x
    })


setMethod("nrow",
    signature=signature(x="cytoFrame"),
    definition=function(x)
      getnrow(x@pointer)
)

setMethod("ncol",
    signature=signature(x="cytoFrame"),
    definition=function(x)
      getncol(x@pointer)
)

#' @export
deep_copy <- function(x, ...)UseMethod("deep_copy")

#' @export 
deep_copy.cytoFrame <- function(x){
  new("cytoFrame", pointer = deep_copy_cytoframe(x@pointer))
}

#' @export
shallow_copy <- function(x, ...)UseMethod("shallow_copy")

#' @export 
shallow_copy.cytoFrame <- function(x){
  new("cytoFrame", pointer = shallow_copy_cytoframe(x@pointer))
}

setMethod("[",
    signature=signature(x="cytoFrame"),
    definition=function(x, i, j, ..., drop=FALSE)
    {
      fr <- shallow_copy(x)
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
            
        if(is.numeric(j)||is.integer(j))    
          subset_cytoframe_by_cols(fr@pointer, j - 1)
        else
          stop("invalid j index!")
      }
      
      if(!missing(i))
      {
        if(is.logical(i))
          i <- which(i)
        if(is.numeric(i)||is.integer(i))    
          subset_cytoframe_by_rows(fr@pointer, i - 1)
        else
          stop("invalid i index!")
      }

      fr
    })

setMethod("exprs",
    signature=signature(object="cytoFrame"),
    definition=function(object){
      if(object@use.exprs)
        getData(object@pointer)
      else
      {
        cn <- colnames(object)
        matrix(nrow = 0, ncol = length(cn), dimnames = list(NULL, cn))
      }
    })

setReplaceMethod("colnames",
    signature=signature(x="cytoFrame",
        value="ANY"),
    definition=function(x, value)
    {
      old.names <- colnames(x)
      if(length(value) != length(old.names))
        stop("colnames don't match dimensions of data matrix",
            call.=FALSE)
      
      for(i in seq_along(value))
        setChannel(x@pointer, old.names[i], value[i])
      return(x)
    })

setReplaceMethod("markernames",
    signature=signature(object="cytoFrame", value="ANY"), function(object, value){
      old.names <- parameters(object)[["desc"]]
      if(!is.character(value)){
        stop("value must be a named character vector!")
      }else{
        chnls <- names(value)
        if(is.null(chnls))
          stop("value must be a named character vector!")
        inds <- match(chnls, colnames(object))
        misMatch <- is.na(inds)
        if(any(misMatch))
          stop("channel names not found in flow data: ", paste0(chnls[misMatch], collapse = ","))
        
        for(i in seq_along(inds)){
          ind <- inds[i]
          setMarker(object@pointer, old.names[ind], value[i])
        }
          
        
        
        
        object
      }
      
      
    })

setMethod("parameters",
    signature=signature(object="cytoFrame"),
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
setMethod("keyword",
    signature=signature(object="cytoFrame",
        keyword="character"),
    function(object, keyword){
      val <- getKeyword(object@pointer,keyword)
      if(val=="")
        val <- NULL
      desc <- structure(list(val), names=keyword)
      process_spill_keyword(desc)
      
    })


## this is equivalent to the description method
setMethod("keyword",
    signature=signature(object="cytoFrame",
        keyword="missing"),
    function(object, compact = FALSE)
    {           
      
      desc <- getKeywords(object@pointer)

      if(compact)
        desc <- kwfilter(desc)
      desc <- as.list(desc) 
      FCSversion <- desc[["FCSversion"]]
      desc[["FCSversion"]] <- NULL
      desc <- c(FCSversion = FCSversion, desc)  
      process_spill_keyword(desc)
    })

setReplaceMethod("keyword",
    signature=signature(object="cytoFrame",
        value="list"),
    definition=function(object, value)
    {
      n <- names(value)
      if(length(n) == 0)
        stop(kwdError, call.=FALSE)
      setKeywords(object@pointer, value)
      return(object)
    })

# coerce cytoFrame to flowFrame
#' @export
as.flowFrame <- function(fr){
  fr@exprs <- exprs(fr)
  fr@description = keyword(fr)
  fr@parameters <- parameters(fr)
  as(fr, "flowFrame")
}

#' save the cytoFrame as h5 format
#' @param fr cytoFrame object
#' @param filename the full path of the output h5 file
#' @export
fr_write_h5 <- function(fr, filename){
  writeH5(fr@pointer,filename)
}

#' Load the cytoFrame from h5 format
#' @param filename the full path of the output h5 file
#' @param on_disk logical flag indicating whether to keep the data on disk and load it on demand. Default is TRUE.
#' @export
load_cytoframe_from_h5 <- function(filename, on_disk = TRUE){
  new("cytoFrame", pointer = load_cf_from_h5(filename, on_disk), use.exprs = TRUE)
}
#' return the file path of underlying h5 file
#' For the in-memory version of cytoFrame, it returns empty string.Thus can be used to check whether it is on-disk format.
#' @param fr cytoFrame object
#' @export 
fr_get_h5_file_path <- function(fr){
  get_h5_file_path(fr@pointer)
  
}