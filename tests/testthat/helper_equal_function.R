#' return nothing when pass the test
#' @param orig the original flowFrame served as the reference 
#' @param new the new flowFrame to test 
is_equal_flowFrame <- function(orig, new, exprs = TRUE, description = TRUE){
  
  if(exprs){
    expect_equal(orig@exprs, new@exprs, tol = 1e-07, check.attributes = FALSE)
  }
  
  
  expect_equal(orig@parameters, new@parameters, tol = 1e-07, check.attributes = FALSE)
  
  #keyword may have minor change
  if(description)
  {
    kw1 <- orig@description
    kw2 <- new@description
    #clean up ws from legacy parser
    kw1 <- sapply(kw1, function(kw){
      trimws(kw)
    }, simplify = FALSE)
    kw2 <- sapply(kw2, function(kw){
      trimws(kw)
    }, simplify = FALSE)
    #rm $FILENAME
    kw1[["FILENAME"]] <- NULL
    kw2[["FILENAME"]] <- NULL
    kw1[["ORIGINALGUID"]] <- NULL
    kw2[["ORIGINALGUID"]] <- NULL
    expect_true(all(is.element(kw1, kw2)))
  }
    
}

#' @param fs_orig the original flowSet served as the reference 
#' @param fs_new the new flowSet to test 
is_equal_flowSet <- function(fs_orig, fs_new, ...){
  all.equal(sampleNames(fs_orig), sampleNames(fs_new))
  invisible(lapply(sampleNames(fs_orig), function(sn){
    orig <- fs_orig[[sn]]
    target <- fs_new[[sn]]
    is_equal_flowFrame(orig, target, ...)
  }))  
} 
