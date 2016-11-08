# copied from plyr to avoid the dependency on plyr
compact <- function (l) 
  Filter(Negate(is.null), l)

LdFlags <- function(lib=c("pb", "all")){
  lib <- match.arg(lib)
  
  libpaths <- paste0("lib", Sys.getenv("R_ARCH"), if(lib == "pb") "/libprotobuf.a" else c("/libflowWorkspace.a", "/libprotobuf.a"))
  libpaths <- lapply(libpaths, function(libpath)tools::file_path_as_absolute( base::system.file(libpath, package = "flowWorkspace" )))
  cat(paste(libpaths, collapse = " "))
}
