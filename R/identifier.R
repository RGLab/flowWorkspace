#' @importFrom flowCore identifier
#' @rdname GatingSet-methods
#' @export 
setMethod("identifier",c("GatingSet"),function(object){
  object@guid
})

#' @rdname GatingSet-methods
#' @export 
setMethod("identifier",c("GatingSetList"),function(object){
  object@guid
})

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
                   object@guid <- value
                   return(object)
                   return(object)
                 })
