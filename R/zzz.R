#' get/set the default backend format of cytoframe
#' @param backend one of c("h5", "mem",  "tile")
#' @rdname default_backend
#' @export
get_default_backend <- function(){
  getOption("backend")
}

#' @rdname default_backend
#' @export
set_default_backend <- function(backend = c("h5", "mem",  "tile")){
  backend <- match.arg(backend)
  if(backend=="tile"&&!is_tiledb_support())
    stop("Can't set backend to 'tile' because cytolib is not build with tiledb support!")
  options("backend" = backend)
}

#' report/set the on-disk idx flag
#' @param value TRUE/FALSE, when not specified, it is NULL and simply report the current setting
#' @return TRUE/FALSE
#' @export
#' @examples 
#' #report the current setting
#' use_on_disk_idx()
#' #turn on the ondisk idx
#' use_on_disk_idx(TRUE)
use_on_disk_idx <- function(value = NULL)
{
	if(!is.null(value))
		set_ondisk_idx_flag(value)
	get_ondisk_idx_flag()
}
.cytoctx_global <- NULL
.onLoad <- function(libname, pkgname){
  .cytoctx_global <<- cytoctx()
  set_default_backend()
  options("cyto_stats_type" = list(basic = c('Count','Freqoftotal','Freqofgrandparent','Freqofparent')
                                   , with_ancestor = 'Freqof'
                                   , with_channel = c('SD', 'CV', 'Robust CV','Robust SD'
                                                      ,'Median Abs Dev','Median','Mean','Geometric Mean'
                                                      ,'Mode','Percentile')
                                   , with_percent = "Percentile")
  )
  h5_set_error_handler()#set R stderr as output stream for error handler of libhdf5
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0('As part of improvements to flowWorkspace, some behavior of\n', 
                               'GatingSet objects has changed. For details, please read the section\n',
                               'titled "The cytoframe and cytoset classes" in the package vignette:\n\n',
                               '  vignette("flowWorkspace-Introduction", "flowWorkspace")'))
}