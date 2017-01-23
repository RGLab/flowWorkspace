context("GatingSetList Accessors")


gslist <- NULL
test_that("GatingSetList constructor", {

      suppressMessages(gs_clone <- clone(gs))
      
      #duplicated sample names
      expect_error(GatingSetList(list(gs, gs_clone)), "There are overlapping samples across GatingSets")
      
      sampleNames(gs_clone) <- "CytoTrol_CytoTrol_2.fcs"
      expect_error(GatingSetList(list(gs, gs_clone), samples = c("a", "b")), "'samples' slot is not consisitent with sample names from GatingSets!")
      
      #different colnames
      chnnls <- colnames(flowData(gs_clone))
      chnnls[1] <- "FSC-H" 
      colnames(flowData(gs_clone)) <- chnnls
      expect_error(GatingSetList(list(gs, gs_clone)), "colnames of flowSets don't match")
            
      #different tree structure
      invisible(Rm("CD8", gs_clone))
      expect_error(GatingSetList(list(gs, gs_clone)), "gating structure doesn't match: CytoTrol_CytoTrol_2.fcs CytoTrol_CytoTrol_1.fcs")
      
      suppressMessages(gs_clone <- clone(gs))
      sampleNames(gs_clone) <- "CytoTrol_CytoTrol_2.fcs"
      #reorder by samples argument
      samp <- c("CytoTrol_CytoTrol_2.fcs", "CytoTrol_CytoTrol_1.fcs")
      gslist <- GatingSetList(list(gs, gs_clone), samples = samp)
      expect_equal(sampleNames(gslist), samp)
      
      gslist <<- GatingSetList(list(gs, gs_clone))
      
    })


test_that("[", {
      thisRes <- gslist[1]
      expect_is(thisRes, "GatingSetList")
      expect_equal(sampleNames(thisRes), "CytoTrol_CytoTrol_1.fcs")
      
    })

test_that("rbind2", {
      suppressMessages(thisRes <- rbind2(gslist))
      expect_is(thisRes, "GatingSet")
      expect_equal(sampleNames(thisRes), sampleNames(gslist))
      expect_equal(pData(thisRes), pData(gslist))
    })

test_that("pData<-", {
      pd <- pData(gslist)
      pd$group <- letters[1:2]
      pData(gslist) <- pd
      expect_equal(pData(gslist), pd, check.attributes = FALSE)
      
    })

test_that("getData", {
      thisRes <- getData(gslist)
      expect_is(thisRes, "ncdfFlowList")
      expect_equal(sampleNames(thisRes), sampleNames(gslist))
      expect_equal(nrow(thisRes[[1]]),  119531)
      
      thisRes <- getData(gslist, "CD8")
      expect_is(thisRes, "ncdfFlowList")
      expect_equal(nrow(thisRes[[1]]),  14564)
      
    })

test_that("getGate", {
      thisRes <- getGate(gslist, "CD4")
      expect_is(thisRes, "list")
      expect_equal(names(thisRes), sampleNames(gslist))
      
      expect_true(all(sapply(thisRes, class) == "ellipsoidGate"))
      
    })

test_that("getPopStats", {
      thisRes <- getPopStats(gslist, format = "wide")
      expect_is(thisRes, "matrix")
      expect_equal(colnames(thisRes), sampleNames(gslist))
      expect_equal(thisRes[,1, drop = F], getPopStats(gs, format = "wide"))
      
    })

test_that("keyword", {
      thisRes <- keyword(gslist)
      
      expect_equal(thisRes[[1]], keyword(gs)[[1]])
      
    })

test_that("getSingleCellExpression for COMPASS",{
      
      thisRes <- getSingleCellExpression(gslist, c('CD8/38- DR+', 'CD8/CCR7- 45RA+') , map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")) 
      expectRes <- readRDS(file.path(resultDir, "getData_COMPASS_gs.rds"))
      expect_equivalent(thisRes,expectRes)

      #perserve the intensity that belows the gate threhold
      thisRes <- getSingleCellExpression(gslist, c('CD8/38- DR+', 'CD8/CCR7- 45RA+') , map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE"), threshold = F) 
      
      #return more markers
      thisRes1 <- getSingleCellExpression(gslist, c('CD8/38- DR+', 'CD8/CCR7- 45RA+')
                                         , map = list("CD8/38- DR+" = "CD38 APC"
                                                      , "CD8/CCR7- 45RA+" = "CCR7 PE")
                                         , threshold = F
                                         , other.markers = c("CD4", "CD8") 
                                        )
      #expect extra two columns
      all.equal(colnames(thisRes1[[1]]), c(colnames(expectRes[[1]]), "CD4 PcpCy55", "CD8 APCH7"))
      thisRes1 <- lapply(thisRes1, function(mat)mat[, 1:2])
      expect_equivalent(thisRes,thisRes1)
    })

test_that("markernames", {
  expect_true(setequal(markernames(gslist), sort(markernames(gs))))
  
  #create discrepancy
  chnls <- c("<B710-A>", "<R780-A>")
  markers <- c("CD4", "CD8")
  names(markers) <- chnls
  markernames(gslist@data[[1]]) <- markers
  expect_warning(res <- markernames(gslist), "not unique")
  tmp <- lapply(gslist@data, function(gs)sort(markernames(gs)))
  expect_equal(res, tmp)
  

  #setter
  markernames(gslist) <- markers
  expect_equivalent(markernames(gslist)[c(1,3)], markers)
  #restore original markers
  markers <- c("CD4 PcpCy55", "CD8 APCH7")
  names(markers) <- chnls
  markernames(gslist) <- markers
  expect_equivalent(markernames(gslist)[c(1,3)], markers)
  
})

test_that("colnames", {
  chnls <- c('FSC-A','FSC-H','FSC-W','SSC-A','<B710-A>','<R660-A>','<R780-A>','<V450-A>','<V545-A>','<G560-A>','<G780-A>','Time')
  expect_equal(colnames(gslist), chnls)
  
  chnls.new <- chnls
  chnls.new[c(1,4)] <- c("fsc", "ssc")
  
  #create discrepancy
  colnames(gslist@data[[1]]) <- chnls.new
  expect_error(colnames(gslist), "not unique")
  
  expect_error(colnames(gslist) <- chnls.new, "not safe")
  
  #test the different order
  colnames(gslist@data[[1]]) <- sort(chnls)
  expect_warning(colnames(gslist), "different orders")
  
  #restore original chnls
  colnames(gslist@data[[1]]) <- chnls
  expect_equal(colnames(gslist), chnls)
  
})

test_that("save_gslist /load_gslist", {
      tmp <- tempfile()
      expect_message(save_gslist(gslist, tmp), regexp = "Done")
      expectRes <- unlist(lapply(gslist, slot, name = "guid", level = 1))
      expectRes <- c("samples.rds", expectRes)
      expect_true(setequal(list.files(tmp), expectRes))
      
      ncfiles <- list.files(tmp, recursive = TRUE, pattern = ".nc")
      
      expect_error(save_gslist(gslist, tmp), regexp = "already exists")
      
      expect_message(save_gslist(gslist, tmp, overwrite = TRUE), regexp = "Done")
      expect_true(setequal(list.files(tmp), expectRes))
      
      expect_error(save_gslist(gslist[1], tmp), regexp = "does not seem to match")
      expect_error(save_gslist(gslist[1:2], tmp), regexp = "does not seem to match")
      
      expect_message(gslist1 <- load_gslist(tmp), regexp = "Done")
      
    })
 

