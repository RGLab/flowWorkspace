#' Exact MFI from populations(or nodes) for all the markers
#'
#' It calculates the MFI for each marker.
#'
#' @param x a GatingSet or GatingHierarchy
#' @param ... arguments passed to \link{getNodes} method.
#' @return a data.table that contains MFI values for each marker per column along with 'pop' column and 'sample' column (when used on a 'GatingSet')
#' @import flowCore
#' @import ncdfFlow
#' @import data.table
#' @export
#' @rdname getStats
#' @examples
#' \dontrun{
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE)))
#'
#' # get stats all nodes
#' dt <- getStats(gs) #default is "count"
#'
#' nodes <- c("CD4", "CD8")
#' getStats(gs, nodes, "percent")
#'
#' # pass a build-in function
#' getStats(gs, nodes, type = pop.MFI)
#'
#' # compute the stats based on the raw data scale
#' getStats(gs, nodes, type = pop.MFI, inverse.transform = TRUE)
#'
#' # supply user-defined stats fun
#' pop.quantiles <- function(fr){
#'    chnls <- colnames(fr)
#'    res <- matrixStats::colQuantiles(exprs(fr), probs = 0.75)
#'    names(res) <- chnls
#'    res
#'    }
#' getStats(gs, nodes, type = pop.quantiles)
getStats <- function(x, ...)UseMethod("getStats")

#' @export
#' @rdname getStats
getStats.GatingSetList <- function(x, ...){
  getStats.GatingSet(x, ...)
}

#' @export
#' @rdname getStats
getStats.GatingSet <- function(x, ...){
  res <-  lapply(x, function(gh){
    getStats(gh, ...)

  })
  rbindlist(res, idcol = "sample")
}

#' @export
#' @rdname getStats
#' @param nodes the character vector specifies the populations of interest. default is all available nodes
#' @param type the character vector specifies the type of pop stats or
#'          a function used to compute population stats.
#'          when character, it is expected to be either "count" or "percent". Default is "count" (total number of events in the populations).
#'          when a function,  it takes a flowFrame object through 'fr' argument and return the stats as a named vector.
#' @param inverse.transform logical flag . Whether inverse transform the data before computing the stats.
#' @param stats.fun.arg a list of arguments passed to `type` when 'type' is a function.
getStats.GatingHierarchy <- function(x, nodes = NULL, type = "count", inverse.transform = FALSE, stats.fun.arg = list(), ...){
  gh <- x
  if(is.null(nodes))
    nodes <- getNodes(gh, ...)
  res <- sapply(nodes, function(node){
    if(is.character(type))
    {
      type <- match.arg(type, c("count", "percent"))
      if(type == "count")
      {
        res <- getTotal(gh, node)
        names(res) <- "count"
      }else if(type == "percent")
      {
        res <- getProp(gh, node)
        names(res) <- "percent"
      }else
        stop("unsupported stats type: ", type)
    }else{
      fr <- getData(gh, y = node)
      if(inverse.transform)
      {
        trans <- getTransformations(gh, inverse = TRUE)
        if(length(trans)==0)
          stop("No inverse transformation is found from the GatingSet!")
        trans <- transformList(names(trans), trans)
        fr <- transform(fr, trans)
      }
      thisCall <- quote(type(fr))
      thisCall <- as.call(c(as.list(thisCall), stats.fun.arg))
      
      res <- eval(thisCall)
    }

    as.data.table(t(res))
  }, simplify = FALSE)
  rbindlist(res, idcol = "pop")

}

#' built-in stats functions.
#'
#' pop.MFI computes and returns the median fluorescence intensity for each marker.
#' They are typically used as the arguments passed to \code{getStats} method to perform the sample-wise population stats calculations.
#'
#' @param fr a flowFrame represents a gated population
#' @return a named numeric vector
#'
#' @rdname stats.fun
#' @export
#' @importFrom  matrixStats colMedians
pop.MFI <- function(fr){
  pd <- pData(parameters(fr))
  pd <- data.table(pd)
  pd <- pd[!is.na(desc), ]
  chnls  <- pd[, name]
  markers <- pd[, desc]

  res <- colMedians(exprs(fr)[, chnls, drop = FALSE])
  names(res) <- markers
  res
}
