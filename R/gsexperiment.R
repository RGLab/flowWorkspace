#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @export
setClass("gsexperiment", contains = c("SummarizedExperiment"))
         
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @export
gsexperiment <- function(gs, pop = "root"){
  leaf.all <- gs_get_leaf_nodes(gs)
  
  calist <-  lapply(gs, function(gh){
    cf <- gh_pop_get_data(gh, pop)
    ca <- CytoArray(cf)
    
    #rowData
    pd <- pData(parameters(cf))
    
    ## somehow can't coerse df to DF
    # pd$name <- as.character(pd$name)
    # pd$desc <- as.character(pd$desc)
    # as(pd, "DataFrame")
    rd <- DataFrame(#name = pd$name
                     desc = pd$desc
                    # , range = pd$range
                    # , minRange = pd$minRange
                    # , maxRange = pd$maxRange
                    , row.names = pd$name #rownames(pd)
                    )
    #colData
    
    
    SummarizedExperiment(assays = SimpleList(intensity = ca), rowData = rd)
  })
  as(cse, "gsexperiment")
  
}
