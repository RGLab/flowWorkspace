#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @export
setClass("cytoexperiment", contains = c("cytoframe" , "SummarizedExperiment"))
         

cytoexperiment <- function(cf){
  new("cytoexperiment"
      , pointer = cf@pointer
      )
}