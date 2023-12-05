#' parse the transformation function parameters from the transformer object 
#' @param x transformer object
#' @return a list that represents the data structure that is ready to be passed to Rcpp API 'set_transformations'
#' @noRd
parse_transformer <- function(x){
  stopifnot(is(x, "transform"))
  transobj <- as.list(environment(x[["transform"]]))
  transobj[["type"]] <- x[["name"]]
  if(transobj[["type"]] == "flowJo_biexp")
  {
	  transobj <- c(transobj["type"], attr(x[["transform"]], "parameters"))
  }else if(!(transobj[["type"]] %in% c("logicle", "flowJo_log", "logtGml2", "asinhtGml2", "logicleGml2", "flowJo_fasinh")))
  {
    transobj <- list()
  }
    
  return(transobj)
  
}
