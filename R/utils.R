# copied from plyr to avoid the dependency on plyr
compact <- function (l) 
  Filter(Negate(is.null), l)

LdFlags <- function(){
  libpath <- paste0("lib", Sys.getenv("R_ARCH"), "/libprotobuf.a")
  cat(tools::file_path_as_absolute( base::system.file(libpath, package = "flowWorkspace" )))
}
