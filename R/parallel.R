#' @export
setClass("gscluster", contains = "GatingSet", representation(path = "character"))

#' @export
setMethod("show","gscluster",function(object){
  callNextMethod()
  cat("archive dir: ", object@path, "\n")
  })

#' @export
gscluster <- function(gs, path = tempfile()){
  stopifnot(is(gs, "GatingSet"))
  suppressMessages(save_gs(gs, path))
  suppressMessages(gs <- load_gs(path))
  gsc <- as(gs, "gscluster")
  gsc@path <- path
  gsc
}
#' @importFrom parallel mclapply
# gs_mclapply <- function(gs, FUN, mc.cores, ...){
#   stopifnot(is(gs, "gscluster"))
#   pbfiles <- mclapply(sampleNames(gs), function(sn){
#     gh <- gs[[sn]]
#     FUN(gh)
#     tmp <- tempfile()
#     # save_gs(gh, tmp, backend_opt = "skip")
#     tmp
#   }, mc.cores = mc.cores)
#   pbfiles
# }



#' @importFrom parallel clusterApply clusterExport
#' @export
gs_clusterApply <- function(cl = NULL, gs, FUN, ..., mutable = TRUE){
  stopifnot(is(gs, "gscluster"))
  gs_tmp <- tempfile()
  dir.create(gs_tmp)
  clusterExport(cl, "gs_tmp", envir = environment())
  res <- clusterApply(cl, sampleNames(gs), function(sn){
    suppressPackageStartupMessages({library(flowCore)
                                    library(flowWorkspace)
                                  })
    message("processing: ", sn)
    suppressMessages(gs1 <- load_gs(gs@path, select = sn, backend_readonly = FALSE))
    gh <- gs1[[sn]]
    thisres <- FUN(gh, ...)
    if(mutable)
    {
      tmp <- tempfile(tmpdir = gs_tmp)
      suppressMessages(save_gs(gh, tmp, backend_opt = "skip"))
    }
    thisres
  }, ...)
  
  if(mutable)#update pb
  {
    pbfiles <- list.files(gs_tmp, pattern = ".pb$", recursive = TRUE, full.names = TRUE)
    file.copy(pbfiles, gs@path, overwrite = TRUE)
    #reload gs
    # cs <- gs_cyto_data(gs)
    gsc_reload(gs) 
    # gs_cyto_data(gs) <- cs
  }
  res
}

gsc_reload <- function(gsc){
  #TODO: more options to retain the initial gs archive load property e.g. readonly flag)
  suppressMessages(gs <- load_gs(gsc@path))
  update_gs_ptr(gsc, gs@pointer)
}