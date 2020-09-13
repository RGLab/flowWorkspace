#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @export
setClass("gsexperiment", contains = c("SingleCellExperiment"))
         
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @export
gsexperiment <- function(gs, pop = "root"){
  leaf <- gs_get_leaf_nodes(gs, pop, path = "auto", showHidden = FALSE)
  sns <- sampleNames(gs)
  selist <-  lapply(sns, function(sn){
    gh <- gs[[sn]]
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
    
    #label each cell with leaf nodes
    ncell <- nrow(cf)
    idx.ancestor <- gh_pop_get_indices(gh, pop)
    idx <- rep(F, ncell)
    pop.col <- rep(NA_character_, ncell)
    for(node in leaf)
    {
      #global idx
      idx1 <- gh_pop_get_indices(gh, node)
      #convert to local
      idx1 <- idx1[idx.ancestor]
      #check if idx overlap among leaf
      if(any(idx1&idx))
        stop(node, " cells overlap with other leaf nodes!")
      #label the cells with current node
      pop.col[idx1] <- node
    }
    #convert to factor to save space
    pop.col <- factor(pop.col, levels = leaf)
    cd <- DataFrame(pop = pop.col, sample = factor(rep(sn, ncell), levels = sns))
    SingleCellExperiment(assays = SimpleList(intensity = ca), rowData = rd, colData = cd)
  })
  #cbind samples
  se <- do.call(cbind, selist)
  as(se, "gsexperiment")
  
}

setMethod("GatingSet",c("SingleCellExperiment"),function(x, ...){
  
  sce_to_gs(x, ...)  
})

#' convert SingleCellExperiment to a GatingSet
#' 
#' note that only selected assay and rowData columns will be extracted into the GatingSet
#' @param sce SingleCellExperiment object
#' @param assay_type select assay to use
#' @param sample select the column from colData to represent sample ID
#' @param channel select the column from rowData to represent channels
#' @param marker select the column from rowData to represent markers
#' @export
sce_to_gs <- function(sce
                      , assay_type = "intensity" # expr matrix to use
                      , sample = "sample" # the column from colData to represent sample ID
                      , channel = NA # the column from rowData to represent channels
                      , marker = NA # the column from rowData to represent markers
                      , ...) 
{
  if(!assay_type %in% assayNames(sce))
    stop("assay_type: ", assay_type, " doesn't exist in sce object!")
  ##parse channel and marker
  rd <- rowData(sce)
  if(is.na(channel))
    chnls <- rownames(rd)
  else
    chnls <- rd[[channel]]
  
  mkrs <- rd[[marker]]
  names(mkrs) <- chnls
  
  #split sce by sample col
  sn_vec <- colData(sce)[[sample]]
  cflist <- sapply(as.character(unique(sn_vec)), function(sn){
                      sce1 <- sce[, sn_vec == sn]
                      #convert assay into cytoframes
                      mat <- assays(sce1)[[assay_type]]  
                      mat <- t(mat)
                      mat <- as.matrix(mat)
                      fr <- flowFrame(mat)
                      markernames(fr) <- mkrs
                      cf <- flowFrame_to_cytoframe(fr)
                      
                      rownames(cf) <- rownames(mat)
                      cf
                    })
  
  cs <- cytoset(cflist)
  GatingSet(cs)
  
} 

