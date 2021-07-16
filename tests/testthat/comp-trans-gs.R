test_that("compensate & transform a GatingSet", {
  fs.raw <- read.flowSet(path = system.file("extdata","compdata","data",package="flowCore"))
  gs.raw <- GatingSet(fs.raw)
  comp.mat <- as.matrix(read.table(system.file("extdata","compdata","compmatrix",package="flowCore"),header=TRUE,skip=2,check.names=FALSE))
  
  
  # Compensate with single comp
  row.names(comp.mat) <- colnames(comp.mat)
  fs <- compensate(fs.raw, comp.mat)
  expectRes <- fsApply(fs, colMeans, use.exprs = TRUE)
  
  gs <- gs_clone(gs.raw)
  gs1 <- compensate(gs, comp.mat)
  expect_equal(cs_get_uri(gs)
               , cs_get_uri(gs1)
  )
  comp <- gs_get_compensations(gs1)
  expect_is(comp, "list")
  expect_equal(names(comp), sampleNames(gs))
  expect_equal(fsApply(gs_pop_get_data(gs1), colMeans, use.exprs = TRUE), expectRes)
  
  
  # unmatched names
  names(comp)[1] <- "dd"
  expect_error(compensate(gs, comp), regexp = "compensation not found", class = "error")
  
  #unmatched length
  comp <- comp[1:3]
  expect_error(compensate(gs, comp), regexp = "compensation not found ", class = "error")
  
  #modify comp[5]
  comp <- sapply(sampleNames(gs), function(sn)comp.mat, simplify = FALSE)
  comp[[5]][2] <- 0.001
  gs3 <- compensate(gs_clone(gs.raw), comp)
  expect_failure(expect_equal(fsApply(gs_pop_get_data(gs3), colMeans, use.exprs = TRUE)
                              , expectRes), regexp = "8.399298e-06")
  
  #extra comp element
  comp <- sapply(sampleNames(fs), function(sn)comp.mat, simplify = FALSE)
  comp[["dd"]] <- 1:10
  expect_error(gs4 <- compensate(gs, comp), "should be a compensation object")
  comp[["dd"]] <- comp[[1]]
  gs4 <- compensate(gs_clone(gs.raw), comp)
  comp <- gs_get_compensations(gs4)
  expect_equal(names(comp), sampleNames(gs))
  expect_equal(fsApply(gs_pop_get_data(gs4), colMeans, use.exprs = TRUE), expectRes)
  
  #trans
  chnls <- c("FL1-H", "FL2-H")
  #can't use fs to estimate since range is inconsistent between fs and gs
  #due to the undetermined behavior of setting range from flowCore_Rmax during parsing FCS
  #which will cause the logical parameter to be different
  translist <- estimateLogicle(gs_pop_get_data(gs)[[1]], chnls)
  fs.trans <- transform(fs, translist)  
  expectRes <- fsApply(fs.trans, colMeans, use.exprs = TRUE)
  
  transObj <- estimateLogicle(gs[[1]], chnls)
  
  expect_error(gs.trans <- transform(gs), "Missing the second argument")
  expect_error(gs.trans <- transform(gs, translist), regexp = "transformerList")
  
  suppressMessages(gs.trans <- transform(gs, transObj))
  fs.trans.gs <- gs_pop_get_data(gs.trans)
  expect_equal(fsApply(fs.trans.gs, colMeans, use.exprs = TRUE), expectRes)
  #check range in params and keywords
  expect_equal(range(fs.trans.gs[[1]])[, chnls[1]], c(0, 4.5))
  expect_equal(keyword(fs.trans.gs[[1]])[["flowCore_$P3Rmax"]], "4.5")
  
  transObj.list <- sapply(sampleNames(gs), function(obj)transObj, simplify = FALSE)
  
  # res <- lapply(gs.trans, function(gh){
  #   tl <- getTransformations(gh, only = FALSE)
  #   transformerList(names(tl), tl)
  #   })
  params <- c("m", "t", "a", "w")
  for(sn in sampleNames(gs))
    for(chnl in chnls)
    {
      
      res <- gh_get_transformations(gs[[sn]])[[chnl]]
      expect_equal(attr(res, "type"), "logicle")
      res <- mget(params, environment(res))
      expectRes.trans <- mget(params, environment(transObj.list[[sn]][[chnl]][[2]]))
      expect_equal(res, expectRes.trans)
      
    }
  
  #add trans not supported by c++
  ################################
  gs <- gs_clone(gs.raw)
  cs <- gs_cyto_data(gs)
  translist <- list(flowjo_fasinh_trans(), logicle_trans(), flowjo_biexp_trans(), asinhtGml2_trans(), logicleGml2_trans())
  trans <- transformerList(colnames(cs), translist)      
  gs <- transform(gs, trans)
  # Now all above transformations are supported at c++ level
  expect_equal(gs@transformation, list())
  #trans the cs directly
  gs1 <- gs_clone(gs.raw)
  cs1 <- gs_cyto_data(gs1)
  translist <- lapply(translist, function(obj)obj[["transform"]])
  trans <- transformList(colnames(cs), translist)   
  transform(cs1, trans)
  
  expect_equal(range(cs[[1]], "data"), range(cs1[[1]], "data"))
  expect_false(cs_get_uri(cs)==cs_get_uri(cs1))
  
})
