#' return nothing when pass the test
#' @param orig the original flowFrame served as the reference 
#' @param new the new flowFrame to test 
is_equal_flowFrame <- function(orig, new, exprs = TRUE, description = TRUE){
  
  if(exprs){
    expect_equal(exprs(orig), exprs(new), tol = 1e-07, check.attributes = FALSE)
  }
  
  
  expect_equal(parameters(orig), parameters(new), tol = 1e-07, check.attributes = FALSE)
  
  #keyword may have minor change
  if(description)
  {
    kw1 <- keyword(orig)
    kw2 <- keyword(new)
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
    kw1[["$CYTOLIB_VERSION"]] <- NULL
    
    sp1 <- sp2 <- list()
    try(sp1 <- spillover(orig), silent = T)
    try(sp2 <- spillover(new), silent = T)
    
    expect_equal(sp1, sp2, tol = 6e-6)
    kw1[names(sp1)] <- NULL
    kw2[names(sp2)] <- NULL
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
