#' @importFrom flowCore identifier
#' @rdname GatingSet-methods
#' @export 
setMethod("identifier",c("GatingSet"),function(object){
  object@guid
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
                 signature=signature(object="GatingSet",
                                     value="character"),
                 definition=function(object, value)
                 {
                   object@guid <- value
                   return(object)
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
