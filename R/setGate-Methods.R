#' @include filterObject-Methods.R
NULL

setMethod("setGate"
    ,signature(obj="GatingHierarchy",y="numeric",value="filter")
    ,function(obj,y,value,negated=FALSE,...){
      this_fobj <- filterObject(value)
      this_fobj$negated<-negated
      .Call("R_setGate",obj@pointer,getSample(obj),as.integer(y-1),this_fobj)
      
    })
setMethod("setGate"
    ,signature(obj="GatingHierarchy",y="character",value="filter")
    ,function(obj,y,value,...){
      setGate(obj,.getNodeInd(obj,y),value,...)
    })
setMethod("setGate",
    signature=c(obj="GatingSet",y="ANY", value = "list"),
    definition=function(obj, y, value,...)
    {
      
      flist<-filterList(value)
      setGate(obj,y,flist,...)
      
    })
setMethod("setGate",
    signature=c(obj="GatingSet",y="ANY", value = "filterList"),
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


  
  
