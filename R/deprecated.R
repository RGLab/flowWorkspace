#' @templateVar old openWorkspace
#' @templateVar new open_flowjo_xml
#' @template template-depr_pkg
NULL
#' It is now moved along with entire flowJo parser to CytoML package
#' @param file xml file
#' @param ... other arguments
#' @export
#' @rdname openWorkspace
openWorkspace <- function(file, ...){
  .Defunct(msg = "openWorkspace is now deprecated by CytoML::open_flowjo_xml. Please library(CytoML) first!")
}