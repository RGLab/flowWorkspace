#' @title cytoset with factor support for pData
#' 
#' @description
#' An extended cytoset class that preserves factor levels in pData by using
#' the inherited flowSet phenoData infrastructure instead of the C++ backend.
#' 
#' @details
#' When you assign pData with factor columns to a regular cytoset, the factors
#' are coerced to character strings and their level ordering is lost because
#' cytoset reads/writes pData directly from/to the C++ backend (which only
#' supports strings).
#' 
#' \code{cytoset_factors} solves this by delegating to the grandparent 
#' \code{flowSet} methods, which use the \code{@@phenoData} slot (an 
#' AnnotatedDataFrame from Biobase) that naturally preserves R data types
#' including factors. The C++ backend is still synchronized for C++ operations,
#' but pData queries go through the flowSet infrastructure.
#' 
#' **Key Insight**: flowSet already has infrastructure to store factors in pData!
#' We just need to use it instead of bypassing it for C++ storage.
#' 
#' @seealso \code{\link{cytoset}}, \code{\link{pData}}, \code{\link[flowCore]{flowSet}}
#' 
#' @examples
#' \dontrun{
#' # Load data
#' cs <- cytoset_factors(load_cytoset_from_fcs(...))
#' 
#' # Create pData with factors
#' pd <- pData(cs)
#' pd$Patient <- factor(c("A", "B"), levels = c("C", "B", "A"))
#' pd$Visit <- factor(c("V1", "V2"), levels = c("V3", "V2", "V1"))
#' 
#' # Assign and retrieve - factors are preserved
#' pData(cs) <- pd
#' pd2 <- pData(cs)
#' stopifnot(is.factor(pd2$Patient))
#' stopifnot(identical(levels(pd2$Patient), c("C", "B", "A")))
#' }
#' @name cytoset_factors-class
#' @rdname cytoset_factors
NULL

#' @export
#' @rdname cytoset_factors
setClass("cytoset_factors", 
         contains = "cytoset",
         slots = c(factor_data = "data.frame"))

#' @title Constructor for cytoset_factors
#' @description 
#' Create a cytoset_factors object from an existing cytoset.
#' Converts a regular cytoset to cytoset_factors to enable factor-level 
#' preservation in pData.
#' 
#' @param x A cytoset object to convert
#' @param ... Additional arguments (currently unused)
#' 
#' @return A cytoset_factors object
#' @export
#' @rdname cytoset_factors
cytoset_factors <- function(x, ...) {
  if(!inherits(x, "cytoset")) {
    stop("x must be a cytoset object")
  }
  
  # Convert to cytoset_factors using S4 coercion
  cs <- as(x, "cytoset_factors")
  
  # Fetch existing pData from parent cytoset (C++ backend)
  cs_pData <- selectMethod("pData", "cytoset")
  pd <- cs_pData(x)
  
  # Store in our new slot
  cs@factor_data <- pd
  
  cs
}

#' pData accessor for cytoset_factors
#' 
#' Reads pData from the local slot which preserves factors
#' 
#' @param object A cytoset_factors object
#' @return A data.frame with factors preserved
#' @export
#' @rdname cytoset_factors
setMethod("pData",
          signature = signature(object = "cytoset_factors"),
          definition = function(object) {
            object@factor_data
          })

#' pData replacement for cytoset_factors
#' 
#' Stores pData in the local slot (preserves factors) and syncs to C++ backend
#' 
#' @param object A cytoset_factors object
#' @param value A data.frame with the new pData
#' @return The updated cytoset_factors object
#' @export
#' @rdname cytoset_factors
setReplaceMethod("pData",
                 signature = signature(object = "cytoset_factors",
                                      value = "data.frame"),
                 definition = function(object, value) {
                   # Validation
                   if(nrow(value) != length(object)) {
                     stop("pData must have the same number of rows as the cytoset")
                   }
                   
                   # Update local slot
                   object@factor_data <- value
                   
                   # Sync to cytoset C++ backend using parent method
                   # This ensures downstream C++ operations still work was heroes (you )
                   cs_pData_replace <- selectMethod("pData<-", c("cytoset", "data.frame"))
                   object <- cs_pData_replace(object, value)
                   
                   object
                 })

#' phenoData accessor for cytoset_factors
#' 
#' Returns an AnnotatedDataFrame constructed from the local slot
#' 
#' @param object A cytoset_factors object
#' @return An AnnotatedDataFrame with factors preserved
#' @export
#' @rdname cytoset_factors
setMethod("phenoData",
          signature = signature(object = "cytoset_factors"),
          definition = function(object) {
            pd <- object@factor_data
            new("AnnotatedDataFrame", data = pd, 
                varMetadata = data.frame(labelDescription = colnames(pd), 
                                       row.names = colnames(pd)))
          })
