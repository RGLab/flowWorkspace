#' @include filterObject_Methods.R
NULL

#' update the gate
#' 
#' update the population node with a flowCore-compatible gate object
#' 
#' Usually \link{recompute} is followed by this call since updating a gate
#' doesn't re-calculating the cell events within the gate automatically.
#' see \link{filterObject} for the gate types that are currently supported.
#' 
#' @param obj \code{GatingHierarchy} or \code{GatingSet}
#' @param y \code{character} node name or path
#' @param value \code{filter} or \code{filterList} or \code{list} of \code{filter} objects
#' @param negated \code{logical} see \link{add}
#' @param ... other aguments
#' @examples
#' \dontrun{  
#' rg1 <- rectangleGate("FSC-H"=c(200,400), "SSC-H"=c(250, 400), filterId="rectangle")
#' rg2 <- rectangleGate("FSC-H"=c(200,400), "SSC-H"=c(250, 400), filterId="rectangle")
#' flist <- list(rg1,rg2)
#' names(flist) <- sampleNames(gs[1:2])
#' setGate(gs[1:2], "lymph", flist)
#' recompute(gs[1:2], "lymph") 
#' }
#' @aliases 
#' setGate
#' @rdname setGate
#' @export
setMethod("setGate"
    ,signature(obj="GatingHierarchy",y="character",value="filter")
    ,function(obj,y,value, negated = FALSE,...){
      
      this_fobj <- filterObject(value)
      this_fobj$negated<-negated
      .cpp_setGate(obj@pointer,sampleNames(obj), y, this_fobj)
      
    })
#' @rdname setGate
#' @export 
setMethod("setGate",
    signature=c(obj="GatingSet",y="character", value = "list"),
    definition=function(obj, y, value,...)
    {
      
      flist<-filterList(value)
      setGate(obj,y,flist,...)
      
    })
#' @rdname setGate
#' @export
setMethod("setGate",
    signature=c(obj="GatingSet",y="character", value = "filterList"),
    definition=function(obj, y, value,...)
    {
      samples<-sampleNames(obj)
      
      if(!setequal(names(value),samples))
        stop("names of filterList do not match with the sample names in the gating set!")           
      
      lapply(samples,function(sample){
            curFilter<-value[[sample]]
            gh<-obj[[sample]]
            setGate(obj=gh,y,value=curFilter,...)
          })
      
      
    })

#' @export
transform_gate.GatingHierarchy <- function(obj, y, ...){
  gate <- getGate(obj, y)
  gate <- transform_gate(gate, ...)
  setGate(obj, y, gate)
}

#' @export
transform_gate.GatingSet <- function(obj, y, ...){
  gates <- getGate(obj, y)
  gates <- lapply(gates, function(gate) transform_gate(gate, ...)) 
  setGate(obj, y, gates)
}

#' @export
scale_gate.GatingHierarchy <- function(obj, y, ...){
  gate <- getGate(obj, y)
  gate <- scale_gate(gate, ...)
  setGate(obj, y, gate)
}

#' @export
scale_gate.GatingSet <- function(obj, y, ...){
  gates <- getGate(obj, y)
  gates <- lapply(gates, function(gate) scale_gate(gate, ...)) 
  setGate(obj, y, gates)
}

#' @export
rotate_gate.GatingHierarchy <- function(obj, y, ...){
  gate <- getGate(obj, y)
  gate <- rotate_gate(gate, ...)
  setGate(obj, y, gate)
}

#' @export
rotate_gate.GatingSet <- function(obj, y, ...){
  gates <- getGate(obj, y)
  gates <- lapply(gates, function(gate) rotate_gate(gate, ...)) 
  setGate(obj, y, gates)
}

#' @export
shift_gate.GatingHierarchy <- function(obj, y, ...){
  gate <- getGate(obj, y)
  gate <- shift_gate(gate, ...)
  setGate(obj, y, gate)
}

#' @export
shift_gate.GatingSet <- function(obj, y, ...){
  gates <- getGate(obj, y)
  gates <- lapply(gates, function(gate) shift_gate(gate, ...)) 
  setGate(obj, y, gates)
}
  
  
