#' @title save/load a GatingSet/GatingSetList to/from disk.
#'
#' @description
#' Save/load a GatingSet/GatingSetList which is the gated flow data including gates and populations to/from the disk.
#' The \code{GatingSet} object The internal C data structure (gating tree),\code{ncdfFlowSet} object(if applicable)
#'
#' @param G A \code{GatingSet}
#' @param gslist A \code{GatingSetList}
#' @param path A character scalar giving the path to save/load the GatingSet to/from.
#' @param h5_readonly whether to open h5 data as read-only. Default is TRUE
#' @param cdf a character scalar. The valid options are :"copy","move","skip","symlink","link" specifying what to do with the cdf data file.
#'              Sometime it is more efficient to move or create a link of the existing cdf file to the archived folder.
#'              It is useful to "skip" archiving cdf file if raw data has not been changed.
#' @param ... other arguments: not used.
#'
#'
#' @return
#' \code{load_gs} returns a GatingSet object
#' \code{load_gslist} returns a GatingSetList object
#'
#' @seealso \code{\link{GatingSet-class}},\code{\link{GatingSetList-class}}
#'
#' @examples
#' \dontrun{
#' 	#G is a GatingSet
#' 	save_gs(G,path="tempFolder")
#' 	G1<-load_gs(path="tempFolder")
#'
#' 	#G is a GatingSet
#'
#' 	save_gslist(gslist1,path="tempFolder")
#' 	gslist2<-load_gslist(path="tempFolder")
#' }
#' @rdname save_gs
#' @export
#' @aliases save_gs load_gs save_gslist load_gslist
save_gs<-function(gs, path
                  , cdf = c("copy","move","skip","symlink","link")
                  , ...){
  #  browser()
  cdf <- match.arg(cdf)
  .cpp_saveGatingSet(gs@pointer, path = path, cdf = cdf)
  message("Done\nTo reload it, use 'load_gs' function\n")
  
  
}


#' @rdname save_gs
#' @export
#' @aliases load_gs load_gslist
load_gs<-function(path, h5_readonly = TRUE){
  if(length(list.files(path = path, pattern = ".rds")) >0)
  {
    stop("'", path, "' appears to be the legacy GatingSet archive folder!\nPlease use 'convert_gs_legacy()' to convert it to the new format.")
  }
  h5_acc_flags <- as.integer(!h5_readonly)
  new("GatingSet", pointer = .cpp_loadGatingSet(path, h5_acc_flags))
  
}


#' convert the legacy(mixed with R and C++ files) GatingSet archive to the new format(C++ only)
#' 
#' Note that it is likely some of the keyword values (mainly offsets e.g. BEGINDATA) may change slightly after the converting due to the rewriting data
#' to FCS files through write.FCS.
#' 
#' @param from the old archive path
#' @param to the new archive path
#' @export 
#' @examples 
#' \dontrun{
#' convert_gs_legacy(old_gs_path, new_gs_path)
#' }
convert_gs_legacy <- function(from, to){
  subdir <- list.dirs(from, recursive = FALSE)
  if(length(subdir) == 0){
    message("loading legacy archive...")
    suppressMessages(gs <- .load_legacy(from, to))
    message("saving to new archive...")
    suppressMessages(save_gs(gs, to, cdf = "skip"))
    message("GatingSet is now saved in new format and can be loaded with 'load_gs'")
  }else{
    message("loading legacy archive...")
    suppressMessages(gs <- load_gslist_legacy(from, to))
    message("saving to new archive...")
    suppressMessages(save_gslist(gs, to, cdf = "skip"))
    message("GatingSetList is now saved in new format and can be loaded with 'load_gslist'")
  }
  
}  



.load_legacy <- function(from, to){
  from <- normalizePath(from,mustWork = TRUE)
  if(!file.exists(from))
    stop(from,"' not found!")
  files<-list.files(from)
  
  dat.file <- file.path(from,files[grep(".pb$",files)])
  rds.file<-file.path(from,files[grep(".rds$",files)])
  
  nc.file<-file.path(from,files[grep(".nc$|.nc.trans$",files)])
  #   browser()
  if(length(dat.file)==0)
    stop(".dat file missing in ",from)
  if(length(dat.file)>1)
    stop("multiple .pb files found in ",from)
  if(length(rds.file)==0)
    stop(".rds file missing in ",from)
  if(length(rds.file)>1)
    stop("multiple .rds files found in ",from)
  
  message("loading R object...")
  gs.old <- readRDS(rds.file)
  if(attr(gs.old, "class") == "GatingSet")
  {
    attr(gs.old, "class") <- "GatingSet_legacy"
  }else
    stop("Invalid legacy GatingSet object file: ", rds.file)
  #convert fs to cs
  fs <- gs.old@"data"
  if(length(nc.file)==0)
    stop(".nc file missing in ",from)
  fs@file <- nc.file
  cs <- flowSet_to_cytoset(fs, to)
  
  gs <-  new("GatingSet", pointer = load_legacy_gs(dat.file, cs@pointer))
  
  
  comp <- gs.old@compensation
  if(!is.null(comp))
  {
    if(!is.list(comp)||is.data.frame(comp)){
      comp <- sapply(sampleNames(gs), function(sn)comp, simplify = FALSE)
    }
    gs_set_comp(gs@pointer, comp)
  }
  trans <- gs.old@transformation
  if(length(trans)!=0)
  {
    if(is(trans , "transformerList")){
      trans <- sapply(sampleNames(gs), function(sn)trans, simplify = FALSE)
    }
    gs_set_comp(gs@pointer, trans)
  }
  
  message("Done")
  gs
}

#' @rdname save_gs
#' @export
save_gslist<-function(gslist,path,...){
  
  if(file.exists(path)){
    expect <- unlist(lapply(gslist, function(gs)get_gatingset_id(gs@pointer), level = 1))
    expect <- c(expect, "samples.rds")
    if(!setequal(list.files(path), expect))
      stop("The existing target path '", path, "' does not seem to match the source 'GatingSetList'!")
  }else{
    dir.create(path = path)
  }
  
  #do the dir normalization again after it is created
  path <- normalizePath(path,mustWork = TRUE)
  
  lapply(gslist,function(gs){
    #        this_dir <- tempfile(pattern="gs",tmpdir=path)
    #        dir.create(path = this_dir)
    #        browser()
    guid <- get_gatingset_id(gs@pointer)
    if(length(guid)==0){
      guid <- .uuid_gen()
      set_gatingset_id(gs@pointer, guid)
      
    }
    this_dir <- file.path(path,guid) 
    
    #        invisible(.save_gs(gs,path = this_dir, ...))
    suppressMessages(save_gs(gs,path = this_dir, ...))
  }, level =1)
  #  browser()
  #save sample vector
  saveRDS(names(gslist@samples),file=file.path(path,"samples.rds"))
  message("Done\nTo reload it, use 'load_gslist' function\n")
  
  
}

#' @rdname save_gs
#' @export
load_gslist<-function(path){
  #  browser()
  path <- normalizePath(path,mustWork = TRUE)
  if(!file.exists(path))
    stop(path,"' not found!")
  dirs<-list.dirs(path,full.names = TRUE, recursive = FALSE)
  #   browser()
  res <- lapply(dirs,function(this_dir){
    #        browser()
    load_gs(this_dir)
  })
  samples <- readRDS(file.path(path,"samples.rds"))
  GatingSetList(res, samples = samples)
  
}
