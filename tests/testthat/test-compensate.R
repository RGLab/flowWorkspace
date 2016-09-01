test_that("compensate a GatingSet", {
      fs <- read.flowSet(path = system.file("extdata","compdata","data",package="flowCore"))
      gs <- GatingSet(fs)
      
      comp.mat <- as.matrix(read.table(system.file("extdata","compdata","compmatrix",package="flowCore"),header=TRUE,skip=2,check.names=FALSE))
      
      
      # Compensate with single comp
      row.names(comp.mat) <- colnames(comp.mat)
      expectRes <- fsApply(compensate(fs, comp.mat), colMeans, use.exprs = TRUE)
      
      gs1 <- compensate(gs, comp.mat)
      expect_failure(expect_equal(gs1@data@frames, gs@data@frames))
      expect_is(gs1@compensation, "list")
      expect_equal(names(gs1@compensation), sampleNames(gs))
      expect_equal(fsApply(getData(gs1), colMeans, use.exprs = TRUE), expectRes)
      
      # list
      comp <- sapply(sampleNames(fs), function(sn)comp.mat, simplify = FALSE)
      gs2 <- compensate(gs, comp)
      
      expect_is(gs2@compensation, "list")
      expect_equal(names(gs2@compensation), sampleNames(gs))
      expect_equal(fsApply(getData(gs2), colMeans, use.exprs = TRUE), expectRes)
      
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
      expect_failure(expect_equal(fsApply(getData(gs3), colMeans, use.exprs = TRUE)
              , expectRes), regexp = "8.399298e-06")
      
      #extra comp element
      comp <- sapply(sampleNames(fs), function(sn)comp.mat, simplify = FALSE)
      comp[["dd"]] <- 1:10
      gs4 <- compensate(gs, comp)
      expect_is(gs4@compensation, "list")
      expect_equal(names(gs4@compensation), sampleNames(gs))
      expect_equal(fsApply(getData(gs4), colMeans, use.exprs = TRUE), expectRes)
      
    })