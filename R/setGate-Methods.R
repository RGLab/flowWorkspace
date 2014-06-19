#' @include filterObject-Methods.R
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
      y <- .getNodeInd(obj,y)
      this_fobj <- filterObject(value)
      this_fobj$negated<-negated
      .Call("R_setGate",obj@pointer,getSample(obj),as.integer(y-1),this_fobj)
      
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


  
  
