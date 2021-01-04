#' Create cyto context that carries tiledb context parameters 
#' 
#' @param cred credentials for s3 access. It is a list containing elements of "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION"
#'                   when NULL, read the default credential file from disk (e.g., ~/.aws/credentials)
#' @param num_threads tiledb multithread parameters
#' @return a cytoctx object that is a wrapper around the external pointer,thus it is copy-by-reference object
#' @export
#' @rdname cytoctx
cytoctx <- function(cred = NULL, num_threads = 1L){
  cred <- check_credential(cred)
  cred[["num_threads"]] <- num_threads
  
  structure(list(pointer = new_cytoctx(cred))
    , class = "cytoctx")
}

#' @param x cytoctx
#' @export
#' @rdname cytoctx
print.cytoctx <- function(x, ...){
  ctx_to_list(x)
}

#' Convert cytoctx to a list
#' 
#' @param x cytoctx
#' @export
#' @rdname cytoctx
ctx_to_list <- function(x){
  read_cytoctx(x$pointer)
}