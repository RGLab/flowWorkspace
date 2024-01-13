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

.onLoad <- function(libname, pkgname){
  set_default_backend("h5")
  h5_set_error_handler()#set R stderr as output stream for error handler of libhdf5
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0('As part of improvements to flowWorkspace, some behavior of\n', 
                               'GatingSet objects has changed. For details, please read the section\n',
                               'titled "The cytoframe and cytoset classes" in the package vignette:\n\n',
                               '  vignette("flowWorkspace-Introduction", "flowWorkspace")'))
}