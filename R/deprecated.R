#' @templateVar old openWorkspace
#' @templateVar new open_workspace
#' @template template-depr_pkg
NULL
#' @export openWorkspace
#' @rdname openWorkspace
openWorkspace <- function(file, ...)UseMethod("openWorkspace")

#' It is now moved along with entire flowJo parser to CytoML package
#' @param file xml file
#' @param ... other arguments
#' @export
#' @rdname openWorkspace
openWorkspace.default <- function(file, ...){
  .Defunct(msg = "openWorkspace is now moved to CytoML package. Please library(CytoML) first!")
}