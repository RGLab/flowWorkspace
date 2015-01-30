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
      expect_equal(pData(gslist), pd)
      
    })

test_that("getData", {
      thisRes <- getData(gslist)
      expect_is(thisRes, "ncdfFlowList")
      expect_equal(sampleNames(thisRes), sampleNames(gslist))
      expect_equal(nrow(thisRes[[1]]),  119531)
      
      thisRes <- getData(gslist, "CD8")
      expect_is(thisRes, "ncdfFlowList")
      expect_equal(nrow(thisRes[[1]]),  14570)
      
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
      
    })

#TODO:write test cases for save_gslist /load_gslist 

