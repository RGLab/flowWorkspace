test_that("compensate & transform a GatingSet", {
      fs <- read.flowSet(path = system.file("extdata","compdata","data",package="flowCore"))
      gs <- GatingSet(fs)
      
      comp.mat <- as.matrix(read.table(system.file("extdata","compdata","compmatrix",package="flowCore"),header=TRUE,skip=2,check.names=FALSE))
      
      
      # Compensate with single comp
      row.names(comp.mat) <- colnames(comp.mat)
      expectRes <- fsApply(compensate(fs, comp.mat), colMeans, use.exprs = TRUE)
      
      gs1 <- compensate(gs, comp.mat)
      fs1 <- gs_pop_get_data(gs1)
      #the flowset has been cloned
      expect_failure(expect_equal(gs1@data@frames, gs@data@frames))
      
      #cdf version
      suppressMessages(fs <- ncdfFlowSet(fs))
      gs <- GatingSet(fs)
      gs1 <- compensate(gs, comp.mat)
      fs1 <- gs_pop_get_data(gs1)
      #ncdfFlowSet is not cloned
      expect_equal(gs1@data@frames, gs@data@frames)
      
      expect_is(gs1@compensation, "list")
      expect_equal(names(gs1@compensation), sampleNames(gs))
      expect_equal(fsApply(fs1, colMeans, use.exprs = TRUE), expectRes)
      
      # list
      comp <- sapply(sampleNames(fs), function(sn)comp.mat, simplify = FALSE)
      gs2 <- compensate(gs, comp)
      
      expect_is(gs2@compensation, "list")
      expect_equal(names(gs2@compensation), sampleNames(gs))
      expect_equal(fsApply(gs_pop_get_data(gs2), colMeans, use.exprs = TRUE), expectRes)
      
      # unmatched names
      names(comp)[1] <- "dd"
      expect_error(compensate(gs, comp), regexp = "must match")
      
      #unmatched length
      comp <- comp[1:3]
      expect_error(compensate(gs, comp), regexp = "must match")
      
      #modify comp[5]
      comp <- sapply(sampleNames(gs), function(sn)comp.mat, simplify = FALSE)
      comp[[5]][2] <- 0.001
      gs3 <- compensate(gs, comp)
      expect_failure(expect_equal(fsApply(gs_pop_get_data(gs3), colMeans, use.exprs = TRUE)
              , expectRes), regexp = "8.399298e-06")
      
      #extra comp element
      comp <- sapply(sampleNames(fs), function(sn)comp.mat, simplify = FALSE)
      comp[["dd"]] <- 1:10
      gs4 <- compensate(gs, comp)
      expect_is(gs4@compensation, "list")
      expect_equal(names(gs4@compensation), sampleNames(gs))
      expect_equal(fsApply(gs_pop_get_data(gs4), colMeans, use.exprs = TRUE), expectRes)
    
      #trans
      translist <- estimateLogicle(fs[[1]], c("FL1-H", "FL2-H"))
      fs.trans <- transform(fs, translist)  
      expectRes <- fsApply(fs.trans, colMeans, use.exprs = TRUE)
      
      transObj <- estimateLogicle(gs[[1]], c("FL1-H", "FL2-H"))
      
      expect_error(gs.trans <- transform(gs), "Missing the second argument")
      expect_error(gs.trans <- transform(gs, translist), regexp = "transformerList")
      
      suppressMessages(gs.trans <- transform(gs, transObj))
      transObj.list <- sapply(sampleNames(gs), function(obj)transObj, simplify = FALSE)
      
      expect_equal(gs.trans@transformation, transObj.list)
      fs.trans.gs <- gs_pop_get_data(gs.trans)
      expect_equal(fsApply(fs.trans.gs, colMeans, use.exprs = TRUE), expectRes)
      # res <- lapply(gs.trans, function(gh){
      #   tl <- getTransformations(gh, only = FALSE)
      #   transformerList(names(tl), tl)
      #   })
      params <- c("m", "t", "a", "w")
      for(sn in sampleNames(gs))
        for(chnl in chnls)
        {
              
          res <- gh_get_transformations(fs.trans.gs[[sn]])[[chnl]]
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
      expect_equal(gs@transformation[[1]], trans)
      #trans the cs directly
      gs1 <- gs_clone(gs.raw)
      cs1 <- gs_cyto_data(gs1)
      translist <- lapply(translist, function(obj)obj[["transform"]])
      trans <- transformList(colnames(cs), translist)      
      transform(cs1, trans)
      
      expect_equal(range(cs[[1]], "data"), range(cs1[[1]], "data"))
      
      #customerize cdf path
      tmp <- tempfile(fileext = ".cdf")
      suppressMessages(gs.trans <- transform(gs, transObj, ncdfFile = tmp))
      fs.trans.gs <- gs_pop_get_data(gs.trans)
      expect_equal(fsApply(fs.trans.gs, colMeans, use.exprs = TRUE), expectRes)
      expect_equal(fs.trans.gs@file, tmp)
    })
