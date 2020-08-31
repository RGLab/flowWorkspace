#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @export
setClass("cytoexperiment", contains = c("SingleCellExperiment"))
         
#' @importFrom S4Vectors DataFrame SimpleList
#' @export
cytoexperiment <- function(cf){
  ca <- CytoArray(cf)
  pd <- pData(parameters(cf))
  
  ## somehow can't coerse df to DF
  # pd$name <- as.character(pd$name)
  # pd$desc <- as.character(pd$desc)
  # as(pd, "DataFrame")
  rd <- DataFrame(#name = pd$name
                   desc = pd$desc
#                  , range = pd$range
#                  , minRange = pd$minRange
#                  , maxRange = pd$maxRange
                 # , row.names = pd$name #rownames(pd)
                  )
  
  cse <- SingleCellExperiment(assays = SimpleList(intensity = ca)
                              , rowData = rd)
  as(cse, "cytoexperiment")
  
}
