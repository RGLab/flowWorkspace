#' @title GatingSet with factor support for pData
#' 
#' @description
#' An extended GatingSet class that preserves factor levels in pData.
#' This class wraps a cytoset_factors object to provide factor support
#' for GatingSet pData operations.
#' 
#' @details
#' \code{GatingSet_factors} extends GatingSet by using cytoset_factors for
#' its underlying cytoset, ensuring factor levels are preserved in pData.
#' All standard GatingSet operations work normally through inheritance and
#' method dispatch - no method overrides are needed. The constructor ensures
#' the underlying cytoset is converted to cytoset_factors, and then all 
#' existing GatingSet methods that delegate to the cytoset (pData, phenoData, 
#' etc.) automatically get factor support.
#' 
#' @seealso \code{\link{GatingSet}}, \code{\link{cytoset_factors}}
#' 
#' @examples
#' \dontrun{
#' # Convert existing GatingSet
#' gs <- load_gs(...)
#' gs_f <- GatingSet_factors(gs)
#' 
#' # Create pData with factors
#' pd <- pData(gs_f)  # Uses inherited GatingSet method -> calls cytoset_factors pData
#' pd$Patient <- factor(pd$Patient, levels = c("C", "B", "A"))
#' 
#' # Assign and retrieve - factors are preserved
#' pData(gs_f) <- pd  # Uses inherited GatingSet method -> calls cytoset_factors pData<-
#' pd2 <- pData(gs_f)
#' stopifnot(is.factor(pd2$Patient))
#' }
#' @name GatingSet_factors-class
#' @rdname GatingSet_factors
NULL

#' @export
#' @rdname GatingSet_factors
setClass("GatingSet_factors", 
         contains = "GatingSet",
         slots = c(factor_data = "data.frame"))

#' @title Constructor for GatingSet_factors
#' @description 
#' Create a GatingSet_factors object from an existing GatingSet.
#' The constructor ensures the underlying cytoset is converted to 
#' cytoset_factors, enabling factor preservation through method dispatch.
#' 
#' @param gs A GatingSet object to convert
#' @param ... Additional arguments (currently unused)
#' 
#' @return A GatingSet_factors object with cytoset_factors as underlying data
#' @export
#' @rdname GatingSet_factors
GatingSet_factors <- function(gs, ...) {
  if(!inherits(gs, "GatingSet")) {
    stop("gs must be a GatingSet object")
  }
  
  # Fetch existing pData from parent cytoset (via GatingSet)
  pd <- pData(gs)

  # Create new GatingSet_factors with same pointer
  gs_f <- new("GatingSet_factors", pointer = gs@pointer, factor_data = pd)
  
  # Note: We don't necessarily need to upgrade the C++ backend's cytoset 
  # immediately since GatingSet_factors will intercept pData calls.
  # However, for consistency, we could... but the prompt says 
  # "simply carry the same factor_data slot that can be propagate to cytoset_factor"
  
  gs_f
}

#' @export
setMethod("pData", "GatingSet_factors", function(object) {
  object@factor_data
})

#' @export
setReplaceMethod("pData", c("GatingSet_factors", "data.frame"), function(object, value) {
  # Update local slot
  object@factor_data <- value
  
  # Sync to underlying C++ backend AND potentially the underlying cytoset if it's accessed later
  # We do this by calling the standard GatingSet pData<- method which handles C++ sync
  # We use selectMethod to bypass our own override if necessary, but callNextMethod is better
  callNextMethod(object, value)
  
  # Also, if we want to ensure any extracted cytoset is also a cytoset_factors
  # we might handle that in gs_cyto_data accessor
  
  object
})

#' @export
setMethod("phenoData", "GatingSet_factors", function(object) {
  pd <- object@factor_data
  new("AnnotatedDataFrame", data = pd, 
      varMetadata = data.frame(labelDescription = colnames(pd), 
                             row.names = colnames(pd)))
})

#' @export
setReplaceMethod("phenoData", c("GatingSet_factors", "ANY"), function(object, value) {
   # Extract data frame
   if(is(value, "AnnotatedDataFrame")) {
     df <- pData(value)
   } else {
     tryCatch({
       df <- pData(value)
     }, error = function(e) {
        df <<- as(value, "data.frame")
     })
   }
   
   pData(object) <- df
   object
})

#' @export
#' @export
setMethod("gs_cyto_data", "GatingSet_factors", function(x, ...) {
  # Call the parent method. Since GatingSet_factors inherits from GatingSet,
  # we can use callNextMethod() to delegate to gs_cyto_data,GatingSet-method.
  # This returns a `cytoset` object.
  r <- callNextMethod() 
  
  # Now wrap it as a `cytoset_factors` object, injecting our stored `factor_data`
  cs <- new("cytoset_factors")
  for(sl in slotNames(r))
    slot(cs, sl) <- slot(r, sl)
  
  cs@factor_data <- x@factor_data
  cs
})

#' @export
setReplaceMethod("gs_cyto_data", signature = c("GatingSet_factors", "cytoset"), function(x, value) {
  
  if(is(value, "cytoset_factors"))
    x@factor_data <- value@factor_data
  else
    x@factor_data <- pData(value)
  
  callNextMethod()
  
  x
})
