#' @import DelayedArray
#' @export
setClass("CytoArraySeed",
         contains="Array",
         representation(
           obj = "cytoframe"
           # ,filepath="character"       # Absolute path to the HDF5 file so the
           # object doesn't break when the user
           # changes the working directory (e.g. with
           # setwd()).
           # name="character",           # Name of the dataset in the HDF5 file.
           # dim="integer",
           ,first_val="ANY"           # First value in the dataset.
           # ,chunkdim="integer_OR_NULL"
         )
)

#' @export
CytoArraySeed <- function(obj, type=NA)
{
  stopifnot(is(obj, "cytoframe"))
  new("CytoArraySeed"
      , obj = obj
      # , filepath=cf_get_uri(obj)
      # name=name,
      # ,dim=as.integer(t(dim(obj)))
      , first_val=exprs(obj[1,1])
      # , chunkdim=chunkdim
  )
}


#' @importFrom BiocGenerics path
#' @export
setMethod("path", "CytoArraySeed", function(object, ...)cf_get_uri(object@obj))

#' @export
setMethod("dim", "CytoArraySeed", function(x)as.integer(rev(dim(x@obj))))

setMethod("dimnames", "CytoArraySeed",
          function(x) list(colnames(x@obj), NULL)
)
.extract_array <- function(x, index){
  #transpose i,j
  j <- j0 <- index[[1]]
  i <- i0 <- index[[2]]
  #tackle integer(0) case since cytoframe currently doesn't support it
  if(!is.null(j0)&&length(j0)==0)
    j <- 1
  if(!is.null(i0)&&length(i0)==0)
    i <- 1
  
  if(!is.null(i) > 0&&!is.null(j) > 0)
    thiscall <- quote(x[i,j, drop = FALSE])
  else if(!is.null(i) > 0)
    thiscall <- quote(x[i,, drop = FALSE])
  else if(!is.null(j) > 0)
    thiscall <- quote(x[,j, drop = FALSE])
  else
    thiscall <- quote(x[,, drop = FALSE])
  
  mat <- exprs(eval(thiscall))
  #realize 0 idx case after matrix is generated
  if(!is.null(i0)&&length(i0)==0)
    mat <- mat[0,, drop = FALSE ]
  if(!is.null(j0)&&length(j0)==0)
    mat <- mat[, 0, drop = FALSE]
  #transpose the data
  t(mat)
}
.extract_array_cytoframe <- function(x, index){
  .extract_array(x@obj, index)
}

#' @importFrom DelayedArray extract_array
#' @export
setMethod("extract_array", "CytoArraySeed", .extract_array_cytoframe)

#' @export
setClass("CytoArray",
         contains="DelayedArray",
         representation(seed="CytoArraySeed")
)

# needed to be able to generate new matrix class through new_DelayedArray
# which can be treated by scran as abstract matrix instead of Default delayedMatrix
#' @export
#' setClass("CytoMatrix", contains=c("CytoArray", "DelayedMatrix"))
#' @export
setMethod("matrixClass", "CytoArray", function(x) "CytoMatrix")

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###
#' @export
setMethod("DelayedArray", "CytoArraySeed",
          function(seed) new_DelayedArray(seed, Class="CytoArray")
)

#' @export
CytoArray <- function(cf)
{
  DelayedArray(CytoArraySeed(cf))
}