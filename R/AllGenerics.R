#' @export
#' @docType methods
#' @rdname GatingSet-methods
setGeneric("GatingSet",function(x,y,...)standardGeneric("GatingSet"))

#' @export
setGeneric("filterObject",function(x,...)standardGeneric("filterObject"))
setGeneric("haveSameGatingHierarchy",function(object1,object2,...)standardGeneric("haveSameGatingHierarchy"))

#' @export
setGeneric("openWorkspace", function(file,...)standardGeneric("openWorkspace"))

#' @export
setGeneric("closeWorkspace",function(workspace)standardGeneric("closeWorkspace"))

#' @export
setGeneric("parseWorkspace",function(obj,...)standardGeneric("parseWorkspace"))

#' @export
setGeneric("getNodes",function(x,...)standardGeneric("getNodes"))

#' @export
setGeneric("setNode",function(x,y,value,...)standardGeneric("setNode"))

#' @export
setGeneric("addGate",function(obj,gate,parent,...)standardGeneric("addGate"))


#to deprecated by "flowSet"
setGeneric("getNcdf",function(obj)standardGeneric("getNcdf"))

#to  be  deprecated by "flowSet"
setGeneric("ncFlowSet", function(x) standardGeneric("ncFlowSet"))
#to  be  deprecated by "flowSet"
setGeneric("ncFlowSet<-", function(x,value) standardGeneric("ncFlowSet<-"))

#' @export
setGeneric("flowSet")
#' @export
setGeneric("flowSet<-", function(x,value) standardGeneric("flowSet<-"))


#' @export
setGeneric("plotGate",function(x,y,...)standardGeneric("plotGate"))

#' @export
setGeneric("getPopStats",function(x,...)standardGeneric("getPopStats"))
#' @export
setGeneric("plotPopCV",function(x,...)standardGeneric("plotPopCV"))
#' @export
setGeneric("getData",function(obj,y,...)standardGeneric("getData"))
#' @export
setGeneric("getGate",function(obj,y,...)standardGeneric("getGate"))
#' @export
setGeneric("setGate",function(obj,y,value,...)standardGeneric("setGate"))
#' @export
setGeneric("getParent",function(obj,y,...)standardGeneric("getParent"))
setGeneric("getAxisLabels",function(obj,y,...)standardGeneric("getAxisLabels"))
setGeneric("getBoundaries",function(obj,y,...)standardGeneric("getBoundaries"))
#' @export
setGeneric("getDimensions",function(obj,y,...)standardGeneric("getDimensions"))
#' @export
setGeneric("getChildren",function(obj,y,...)standardGeneric("getChildren"))



setGeneric("getProp",function(x,y,...)standardGeneric("getProp"))
setGeneric("getTotal",function(x,y,...)standardGeneric("getTotal"))

setGeneric("getSamples",function(x,...)standardGeneric("getSamples"))

setGeneric("getSample",function(x,...)standardGeneric("getSample"))

setGeneric("getSampleGroups",function(x)standardGeneric("getSampleGroups"))
setGeneric("getCompensationMatrices",function(x)standardGeneric("getCompensationMatrices"))
setGeneric("getTransformations",function(x)standardGeneric("getTransformations"))

setGeneric("getKeywords",function(obj,y)standardGeneric("getKeywords"))

setGeneric("setData",function(this,value)standardGeneric("setData"))

