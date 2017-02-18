# copied from plyr to avoid the dependency on plyr
compact <- function (l) 
  Filter(Negate(is.null), l)

LdFlags <- function(lib=c("pb", "flowWorkspace", "all")){
  lib <- match.arg(lib)
  libs <- c("/libflowWorkspace.a", "/libprotobuf.a")
  if(lib == "pb") 
    lib <- libs[2]
  else if(lib == "flowWorkspace")
    lib <- libs[1]
  else
    lib <- libs
    
  libpaths <- paste0("lib", Sys.getenv("R_ARCH"), lib)
  libpaths <- lapply(libpaths, function(libpath)tools::file_path_as_absolute(base::system.file(libpath, package = "flowWorkspace" )))
  cat(paste(libpaths, collapse = " "))
}
