#' @importFrom flowCore identifier
#' @rdname GatingSet-methods
#' @export 
setMethod("identifier",
		signature=signature(object="GatingSet"),
		definition=function (object)
		{
			get_gatingset_id(object@pointer)
		})


#' @importFrom digest digest
#' @rdname GatingSet-methods
#' @export 
setMethod("identifier",c("GatingSetList"),function(object){
  gs.ids <- lapply(object, identifier, level = 1)
  gs.ids <- paste(gs.ids, collapse = "")
  digest(gs.ids)
})

#' @param object GatingSet
#' @param value string
#' @rdname GatingSet-methods
#' @export 
setReplaceMethod("identifier",
		signature=signature(object="GatingSet"),
		definition=function (object, value) 
		{
			set_gatingset_id(object@pointer, value)
			object
		})

#' @rdname GatingSet-methods
#' @export 
setReplaceMethod("identifier",
                 signature=signature(object="GatingSetList",
                                     value="character"),
                 definition=function(object, value)
                 {
                  stop("guid of GatingSetList is read-only!")
                 })
