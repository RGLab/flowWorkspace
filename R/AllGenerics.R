#' @importClassesFrom methods ANY character data.frame environment list logical matrix missing numeric oldClass
#' @importMethodsFrom methods coerce show
NULL

#' @export
setGeneric("summary",function(object,...)standardGeneric("summary"))


#' @templateVar old setNode
#' @templateVar new gs(/gh)_set_node/gs(/gh)_hide_node/gs(/gh)_unhide_node
#' @template template-depr_pkg
NULL
#' @export
setGeneric("setNode",function(x,y,value,...)standardGeneric("setNode"))

#' @templateVar old getData
#' @templateVar new gs(/gh)_cyto_data
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getData",function(obj,y,...)standardGeneric("getData"))

#' @templateVar old flowData
#' @templateVar new gs_cyto_data
#' @template template-depr_pkg
NULL
#' @export
setGeneric("flowData", function(x,...) standardGeneric("flowData"))

#' @templateVar old flowData<-
#' @templateVar new gs_cyto_data<-
#' @template template-depr_pkg
NULL
#' @export
setGeneric("flowData<-", function(x,value) standardGeneric("flowData<-"))

#' @templateVar old plotGate
#' @templateVar new autoplot
#' @template template-depr_pkg
NULL
#' @docType methods
#' @rdname plotGate-methods
#' @export
setGeneric("plotGate",function(x,y,...)standardGeneric("plotGate"))

#' @templateVar old getGate
#' @templateVar new gs(/gh)_get_gate
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getGate",function(obj,y,...)standardGeneric("getGate"))

#' @templateVar old setGate
#' @templateVar new gs(/gh)_set_gate
#' @template template-depr_pkg
NULL
#' @export
setGeneric("setGate",function(obj,y,value,...)standardGeneric("setGate"))


#' @templateVar old getSingleCellExpression
#' @templateVar new gs_get_singlecell_expression
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getSingleCellExpression",function(x, nodes,...)standardGeneric("getSingleCellExpression"))
