#' Retrieve/replace the GUID of a GatingSet or GatingSetList
#' 
#' Retrieve or replace the GUID (globally unique identifier) for a \code{\link{GatingSet-class}} or \code{\link{GatingSetList-class}}
#' 
#' @importFrom flowCore identifier
#' @name identifier-methods
#' @aliases identifier identifier,GatingSet-method identifier,GatingSetList-method
#' identifier<-,GatingSetList,ANY-method
#' @usage identifier(object)
#' @param object a \code{GatingSet} or \code{GatingSetList}
#' @export 
setMethod("identifier",
		signature=signature(object="GatingSet"),
		definition=function (object)
		{
			get_gatingset_id(object@pointer)
		})

setMethod("identifier",
		signature=signature(object="cytoset"),
		definition=function (object)
		{
			get_gatingset_id(object@pointer)
		})

#' @importFrom digest digest
#' @export 
setMethod("identifier",c("GatingSetList"),function(object){
  gs.ids <- lapply(object, identifier, level = 1)
  gs.ids <- paste(gs.ids, collapse = "")
  digest(gs.ids)
})

#' @param value string
#' @usage identifier(object) <- value
#' @rdname identifier-methods
#' @aliases identifier<-,GatingSet-method identifier<-,GatingSet,character-method
#' @export 
setReplaceMethod("identifier",
		signature=signature(object="GatingSet"),
		definition=function (object, value) 
		{
			set_gatingset_id(object@pointer, value)
			object
		})
setReplaceMethod("identifier",
		signature=signature(object="cytoset"),
		definition=function (object, value) 
		{
			set_gatingset_id(object@pointer, value)
			object
		})
#' @export 
setReplaceMethod("identifier",
                 signature=signature(object="GatingSetList",
                                     value="character"),
                 definition=function(object, value)
                 {
                  stop("guid of GatingSetList is read-only!")
                 })
