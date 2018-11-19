test_that("compensate & transform a GatingSet", {
      fs <- read.flowSet(path = system.file("extdata","compdata","data",package="flowCore"))
      gs <- GatingSet(fs)
      comp.mat <- as.matrix(read.table(system.file("extdata","compdata","compmatrix",package="flowCore"),header=TRUE,skip=2,check.names=FALSE))
      
      
      # Compensate with single comp
      row.names(comp.mat) <- colnames(comp.mat)
      expectRes <- fsApply(compensate(fs, comp.mat), colMeans, use.exprs = TRUE)
      
      gs1 <- compensate(gs, comp.mat)
      #the flowset has been cloned
      expect_failure(expect_equal(
                                  fr_get_h5_file_path(get_cytoFrame_from_cs(getData(gs),1))
                                  , fr_get_h5_file_path(get_cytoFrame_from_cs(getData(gs1) ,1))
                                  ))
      
      comp <- getCompensationMatrices(gs1)
      expect_is(comp, "list")
      expect_equal(names(comp), sampleNames(gs))
      expect_equal(fsApply(getData(gs1), colMeans, use.exprs = TRUE), expectRes)
      
      
      # unmatched names
      names(comp)[1] <- "dd"
      expect_error(compensate(gs, comp), regexp = "compensation not found")
      
      #unmatched length
      comp <- comp[1:3]
      expect_error(compensate(gs, comp), regexp = "compensation not found ")
      
      #modify comp[5]
      comp <- sapply(sampleNames(gs), function(sn)comp.mat, simplify = FALSE)
      comp[[5]][2] <- 0.001
      gs3 <- compensate(gs, comp)
      expect_failure(expect_equal(fsApply(getData(gs3), colMeans, use.exprs = TRUE)
              , expectRes), regexp = "8.399298e-06")
      
      #extra comp element
      comp <- sapply(sampleNames(fs), function(sn)comp.mat, simplify = FALSE)
      comp[["dd"]] <- 1:10
      expect_error(gs4 <- compensate(gs, comp), "should be a compensation object")
      comp[["dd"]] <- comp[[1]]
      gs4 <- compensate(gs, comp)
      comp <- getCompensationMatrices(gs4)
      expect_equal(names(comp), sampleNames(gs))
      expect_equal(fsApply(getData(gs4), colMeans, use.exprs = TRUE), expectRes)
    
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
      fs.trans.gs <- getData(gs.trans)
      expect_equal(fsApply(fs.trans.gs, colMeans, use.exprs = TRUE), expectRes)
      
      #customerize cdf path
      tmp <- tempfile(fileext = ".cdf")
      suppressMessages(gs.trans <- transform(gs, transObj, ncdfFile = tmp))
      fs.trans.gs <- getData(gs.trans)
      expect_equal(fsApply(fs.trans.gs, colMeans, use.exprs = TRUE), expectRes)
      expect_equal(fs.trans.gs@file, tmp)
    })
