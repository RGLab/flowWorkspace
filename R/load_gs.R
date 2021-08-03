#' @title save/load a GatingSet/GatingSetList to/from disk.
#'
#' @description
#' Save/load a GatingSet/GatingSetList which is the gated flow data including gates and populations to/from the disk.
#' The \code{GatingSet} object The internal C data structure (gating tree),\code{ncdfFlowSet} object(if applicable)
#'
#' @param gs A \code{GatingSet}
#' @param gslist A \code{GatingSetList}
#' @param path A character scalar giving the path to save/load the GatingSet to/from.
#' @param h5_readonly whether to open h5 data as read-only. Default is TRUE
#' @param select an integer or character vector to select a subset of samples to load
#' @param verbose logical flag to optionally print the versions of the libraries that were used to archive the GatingSet for troubleshooting purpose.
#' @param backend_opt a character scalar. The valid options are :"copy","move","skip","symlink" specifying what to do with the backend data file.
#'              Sometimes it is more efficient to move or create a symlink of the existing backend file to the archived folder.
#'              It is useful to "skip" archiving backend file if raw data has not been changed.
#' @inheritParams load_cytoframe
#' @param ... other arguments: not used.
#'
#'
#' 
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
                  , cdf = NULL
                  , backend_opt = c("copy","move","skip","symlink","link")
                  , ...){
  if(!is.null(cdf))
  {
    warning("'cdf' argument is deprecated by 'backend_opt'")
    backend_opt <- cdf
  }
  backend_opt <- match.arg(backend_opt)
  path <- suppressWarnings(normalizePath(path))
  if(is_s3_path(path))
  {
    
    cf <- get_cytoframe_from_cs(gs_cyto_data(gs), 1)
    back_type <- cf_backend_type(cf)
    if(back_type != "tile")
      stop("Saving gs to remote only support tiledb backend!Please use 'convert_backend()` first.")
    
  }
  if(backend_opt == "link")
  	stop("'link' option for save_gs is no longer supported")
  
  suppressMessages(res <- try(cpp_saveGatingSet(gs@pointer, path = path, backend_opt = backend_opt), silent = TRUE))

  if(class(res) == "try-error")
  {
    res <- gsub(" the indexed CytoFrameView object", ' the GatingSet has been subsetted', res)
    
    stop(res[[1]])
  }
  message("Done\nTo reload it, use 'load_gs' function\n")
}

#TODO
gs_is_dirty<- function(x){
  FALSE
  }
is_s3_path <- function(x){
  grepl("^s3://", x, ignore.case = TRUE)
}
is_http_path <- function(x){
  grepl("^https://", x, ignore.case = TRUE)
}

parse_s3_path <- function(url){
  if(is_s3_path(url))
  {
    url <- sub("^s3://", "", url)
    tokens <- strsplit(url, "/")[[1]]
  }else if(is_http_path(url)){
    url <- sub("^https://", "", url)
    tokens <- strsplit(url, "/")[[1]]
    tokens[1] <- sub(".s3.amazonaws.com", "", tokens[1])
  }else
    stop("invalid s3 path", url)
  
  list(bucket = tokens[1], key = paste(tokens[-1], collapse = "/"))
}
#' delete the archive of GatingSet
#' 
#' @param path either a local path or s3 path (e.g. "s3://bucketname/gs_path)
#' @export
delete_gs <- function(path){
  unlink(path, recursive = TRUE)
  message(path, " is deleted")
}

#' @rdname save_gs
#' @export
#' @aliases load_gs load_gslist
load_gs<-function(path, h5_readonly = NULL, backend_readonly = TRUE, select = character(), verbose = FALSE){
  if(!is.null(h5_readonly))
  {
    warning("'h5_readonly' is deprecated by 'backend_readonly'!")
    backend_readonly <- h5_readonly
  }
  if(!is_s3_path(path))
  {
    
    
    if(length(list.files(path = path, pattern = ".rds")) >0)
    {
      stop("'", path, "' appears to be the legacy GatingSet archive folder!\nPlease use 'convert_legacy_gs()' to convert it to the new format.")
    }
	  path <- normalizePath(path)
    sns <- sampleNames(path)
    
  }
  if(!is.character(select))
  {
    select.sn <- sns[select]
    idx <- is.na(select.sn)
    if(any(idx))
      stop("sample selection is out of boundary: ", paste0(select[idx], ","))
  }else
    select.sn <- select
  new("GatingSet", pointer = cpp_loadGatingSet(path, backend_readonly, select.sn, verbose))
  
}

#' Get sample names from a GatingSet archive folder
#'
#' Retrieve  sample names by scanning h5 files from a GatingSet folder
#' 
#' @param object a \code{GatingSet} folder
#' 
#' @rdname save_gs
#' @examples
#'       \dontrun{
#'         sampleNames(gsdir)
#'       }
#' @export
setMethod("sampleNames","character",function(object){
  sub(".pb$", "" , list.files(object, ".pb"))
})

#' convert the legacy GatingSet archive (mixed with R and C++ files) to the new format (C++ only)
#' 
#' Older versions of flowWorkspace represented \code{\link{GatingSet-class}} objects using a combination of
#' R and C++ files, while newer versions have moved the representation entirely to the C++ level for
#' the sake of efficiency. In order to use \code{GatingSet} or \code{GatingSetList} archives created in older versions, 
#' they will need to be converted to the new format.
#' 
#' @details
#' Note that it is likely some of the keyword values (mainly offsets e.g. BEGINDATA) may change slightly after the conversion 
#' due to the process of rewriting data to FCS files through write.FCS.
#' 
#' @param from the old archive path
#' @param to the new archive path
#' @param ...
#'         tmp the path where the temporary files will be written to during the conversion. By default it is system temp folder and
#'          sometime it is helpful to be able to customize it to other location when system temp folder is fulll or not succicient when
#'           converting big data sets.
#' @export 
#' @rdname convert_legacy
#' @examples 
#' \dontrun{
#' convert_legacy_gs(old_gs_path, new_gs_path)
#' }
convert_legacy_gs <- function(from, to, ...){
    message("loading legacy archive...")
    suppressMessages(gs <- .load_legacy(from, to, ...))
    
    #TODO:optional skip generate_h5_folder in add_fcs api to be able to directly write to the target path
    #without needing to do this hack below
    h5dir <- cs_get_uri(gs)
    system(paste0("mv ", h5dir, "/* ", to))#mv h5 files to dest
    #clean the auto generated dir
    system(paste0("rmdir ", h5dir))
    message("saving to new archive...")
    suppressMessages(save_gs(gs, to, backend_opt = "skip"))
    message("GatingSet is now saved in new format and can be loaded with 'load_gs'")
  
}  

#' @export
#' @rdname convert_legacy
convert_legacy_gslist <- function(from, to, ...){
  if(file.exists(to)){
      stop("The existing target path '", to, ". Please specify a new destination path for saving the new 'GatingSetList'!")
  }else{
    dir.create(path = to)
    to <- normalizePath(to,mustWork = TRUE)
    
  }
  from <- normalizePath(from,mustWork = TRUE)
  if(!file.exists(from))
    stop(from,"' not found!")
  dirs<-list.dirs(from,full.names = TRUE, recursive = FALSE)
  res <- lapply(dirs,function(this_dir){
    message("converting legacy archive: ", this_dir)
    new_dir <- file.path(to, basename(this_dir)) 
    suppressMessages(convert_legacy_gs(this_dir, new_dir, ...))
  })
  file.copy(file.path(from,"samples.rds"), file.path(to,"samples.rds"))

  message("GatingSetList is now saved in new format and can be loaded with 'load_gslist'")
  
}

.load_legacy <- function(from, to, ...){
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
  if(is(fs, "ncdfFlowSet"))
  {
	  if(length(nc.file)==0)
		  stop(".nc file missing in ",from)
	  fs@file <- nc.file  
  }
  
  #modify/add flowCore_PnR to reflect the transformation (for the legacy gs archive)
  #(should have been taken care of automatically when data was transformed)
  for(sn in sampleNames(fs))
  {
    fr <- fs@frames[[sn]]
    fr@description <- flowCore:::updateTransformKeywords(fr)
    fs@frames[[sn]] <- fr
  }
  
  cs <- flowSet_to_cytoset(fs, to, ...)
  gs <-  new("GatingSet", pointer = load_legacy_gs(dat.file, cs@pointer))
  
  if(.hasSlot(gs.old, "compensation"))
  {
	  comp <- gs.old@compensation
	  if(!is.null(comp))
	  {
	    if(!is.list(comp)||is.data.frame(comp)){
	      comp <- sapply(sampleNames(gs), function(sn)comp, simplify = FALSE)
	    }
		comp <- sapply(comp, check_comp, simplify = FALSE)
		
		cs_set_compensation(gs@pointer, comp, FALSE)
	  }
  }
  if(.hasSlot(gs.old, "transformation"))
  {
	  translist <- gs.old@transformation
	  if(length(translist)!=0)
	  {
	    if(is(translist , "transformerList")){
			translist <- sapply(sampleNames(gs), function(sn)translist, simplify = FALSE)
		  
	    }else if(is(translist, "list"))
		{
			tList <- lapply(translist, function(trans){
						if(!is(trans, "transformerList"))
							stop("All the elements of 'transformation' slot must be 'transformerList' objects!")
						
					})
		}else
			stop("expect 'transformation' slot as a 'transformerList' object or a list of 'transformerList' objects!")
		
		for(sn in names(translist))
		{
			transobjs <- sapply(translist[[sn]], parse_transformer, simplify = FALSE)
			# browser()
			set_transformations(gs@pointer, sn, transobjs)
			
		}
	  }
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
    gs <- try(load_gs(this_dir), silent = TRUE)
    if(is(gs, "GatingSet"))
      return(gs)
    else
    {
      if(is(gs, "try-error"))
      {
        if(grepl("legacy GatingSet", gs))
          stop("'", path, "' appears to be the legacy GatingSetList archive folder!\nPlease use 'convert_legacy_gslist()' to convert it to the new format.")
        else
          stop(gs)
      }else
        stop("How did you end up with a ", class(gs))
    }
  })
  samples <- readRDS(file.path(path,"samples.rds"))
  GatingSetList(res, samples = samples)
  
}

#' convert h5 based gs archive to tiledb
#' @param gs_dir existing gs archive path
#' @param output_dir the new gs path
#' @export
convert_backend <- function(gs_dir, output_dir){
  #convert h5 to tile
  h5files <- list.files(gs_dir, "\\.h5$", full.names = TRUE)
  if(length(h5files)==0)
    stop("no h5 files to be converted!")
  if(dir.exists(output_dir))
    stop("output dir already exists: ", output)
  dir.create(output_dir)
  pb <- list.files(gs_dir, "\\.(pb|gs)$", full.names = TRUE)
  file.copy(pb, output_dir)
  
  for(h5 in h5files)
  {
    message("converting ", h5)
    cf <- load_cytoframe(h5)
    cf_write_tile(cf, file.path(output_dir, sub("\\.h5$", ".tile",basename(h5))))
  }
  
}
