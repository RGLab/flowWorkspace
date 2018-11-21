#' parse the transformation function parameters from the transformer object 
#' @param x transformer object
#' @return a list that represents the data structure that is ready to be passed to Rcpp API 'set_transformations'
#' @noRd
parse_transformer <- function(x){
  stopifnot(is(x, "trans"))
  transobj <- as.list(environment(x[["transform"]]))
  transobj[["type"]] <- x[["name"]]
  if(!(transobj[["type"]] %in% c("logicle", "logtGml2", "flowJo_biexp", "asinhtGml2", "logicleGml2")))
  {
    transobj <- list()
  }
    
  return(transobj)
  
}
