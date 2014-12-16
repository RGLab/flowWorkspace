#' @importClassesFrom methods ANY character data.frame environment list logical matrix missing numeric oldClass
#' @importMethodsFrom methods coerce show
NULL

#' @export
setGeneric("summary",function(object,...)standardGeneric("summary"))

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
setGeneric("getData",function(obj,y,...)standardGeneric("getData"))

#' @export
setGeneric("flowData", function(x,...) standardGeneric("flowData"))

#' @export
setGeneric("flowData<-", function(x,value) standardGeneric("flowData<-"))

setGeneric("filterObject",function(x,...)standardGeneric("filterObject"))


#' @export
#' @docType methods
#' @rdname plotGate-methods
setGeneric("plotGate",function(x,y,...)standardGeneric("plotGate"))

#' @export
setGeneric("getPopStats",function(x,...)standardGeneric("getPopStats"))

#' @export
setGeneric("plotPopCV",function(x,...)standardGeneric("plotPopCV"))

#' @export
setGeneric("getGate",function(obj,y,...)standardGeneric("getGate"))

#' @export
setGeneric("setGate",function(obj,y,value,...)standardGeneric("setGate"))

#' @export
setGeneric("getParent",function(obj,y,...)standardGeneric("getParent"))

#' @export
setGeneric("getChildren",function(obj,y,...)standardGeneric("getChildren"))

#' @export
setGeneric("getProp",function(x,y,...)standardGeneric("getProp"))

#' @export
setGeneric("getTotal",function(x,y,...)standardGeneric("getTotal"))

#' @export
setGeneric("getSamples",function(x,...)standardGeneric("getSamples"))

#' @export
setGeneric("getSampleGroups",function(x)standardGeneric("getSampleGroups"))

#' @export
setGeneric("getCompensationMatrices",function(x)standardGeneric("getCompensationMatrices"))

#' @export
setGeneric("getTransformations",function(x, ...)standardGeneric("getTransformations"))

#' @export
setGeneric("getKeywords",function(obj,y, ...)standardGeneric("getKeywords"))

#' @export
setGeneric("getSingleCellExpression",function(x, nodes,...)standardGeneric("getSingleCellExpression"))
