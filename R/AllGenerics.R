#' @importClassesFrom methods ANY character data.frame environment list logical matrix missing numeric oldClass
#' @importMethodsFrom methods coerce show
NULL

#' @export
setGeneric("summary",function(object,...)standardGeneric("summary"))

#' @title Deprecated functions in package \pkg{flowWorkspace}.
#' @templateVar old getNodes
#' @templateVar new gs(/gh)_get_nodes
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getNodes",function(x,...)standardGeneric("getNodes"))
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

#' @templateVar old filterObject
#' @templateVar new filter_to_list
#' @template template-depr_pkg
NULL
#' @export
setGeneric("filterObject",function(x,...)standardGeneric("filterObject"))

#' @templateVar old plotGate
#' @templateVar new autoplot
#' @template template-depr_pkg
NULL
#' @docType methods
#' @rdname plotGate-methods
#' @export
setGeneric("plotGate",function(x,y,...)standardGeneric("plotGate"))

#' @templateVar old getPopStats
#' @templateVar new gs(/gh)_get_popstats
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getPopStats",function(x,...)standardGeneric("getPopStats"))

#' @templateVar old plotPopCV
#' @templateVar new gh_plot_popcv
#' @template template-depr_pkg
NULL
#' @export
setGeneric("plotPopCV",function(x,...)standardGeneric("plotPopCV"))

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

#' @templateVar old getParent
#' @templateVar new gs(/gh)_get_parent
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getParent",function(obj,y,...)standardGeneric("getParent"))

#' @templateVar old getChildren
#' @templateVar new gs(/gh)_get_children
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getChildren",function(obj,y,...)standardGeneric("getChildren"))

#' @templateVar old getProp
#' @templateVar new gh_get_proportion
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getProp",function(x,y,...)standardGeneric("getProp"))

#' @templateVar old getTotal
#' @templateVar new gs(/gh)_get_count
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getTotal",function(x,y,...)standardGeneric("getTotal"))

#' @templateVar old getSingleCellExpression
#' @templateVar new gs_get_singlecell_expression
#' @template template-depr_pkg
NULL
#' @export
setGeneric("getSingleCellExpression",function(x, nodes,...)standardGeneric("getSingleCellExpression"))
