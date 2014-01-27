context("GatingSet Accessors")

test_that("isNcdf ",{
      
        
	expect_equal(isNcdf(gs), TRUE);
    
})

test_that("getData ",{
      
      ncfs <- getData(gs)
      expect_is(ncfs, "ncdfFlowSet");
      expect_equal(nrow(ncfs[[1]]), 119531)
      ncfs <- getData(gs, 0)
      expect_equal(nrow(ncfs[[1]]), 119531)
      
      ncfs <- getData(gs, "CD8")
      expect_equal(nrow(ncfs[[1]]), 14623)
      ncfs <- getData(gs, 14)
      expect_equal(nrow(ncfs[[1]]), 14623)
      expect_is(gs[[1]], "GatingHierarchy");
      
      
    })


test_that("flowData ",{
      
      ncfs <- flowData(gs)
      expect_is(ncfs, "ncdfFlowSet");
      expect_equal(nrow(ncfs[[1]]), 119531)
      
      fs <- as(ncfs, "flowSet")
      flowData(gs) <- fs 
      expect_is(flowData(gs), "flowSet");
      
      #restore data
      flowData(gs) <- ncfs
      expect_is(flowData(gs), "ncdfFlowSet");
    })

test_that("sampleNames",{
      
      sn <- sampleNames(gs)
      expect_equal(sn, "CytoTrol_CytoTrol_1.fcs")
      
      #update name
      sampleNames(gs) <- "newSample"
      new_sn <- sampleNames(gs)
      expect_equal(new_sn, "newSample")
      #check if sample name is also updated in cpp data structure
      name_stored_in_cpp <- .Call("R_getSamples", gs@pointer)
      expect_equal(new_sn, name_stored_in_cpp)
      #restore the original name
      sampleNames(gs) <- sn
    })

test_that("clone & rbind2",{
      
      gs_clone <- clone(gs)
      expect_is(gs_clone, "GatingSet");
      
      #check data consistency
      fs1 <- getData(gs)
      fs2 <- getData(gs_clone)
      expect_identical(fs1[[1]], fs2[[1]])
      
      orig_sn <- sampleNames(gs)
      clone_sn <- sampleNames(gs_clone)
      expect_identical(orig_sn, clone_sn)
      
      #check tree consistentcy
      expect_identical(getNodes(gs[[1]]), getNodes(gs_clone[[1]]))
      
      #construct gslist to rbind2
      sampleNames(gs_clone) <- "CytoTrol_CytoTrol_2.fcs"
      clone_sn <- sampleNames(gs_clone)
      gslist <- GatingSetList(list(gs, gs_clone))
      expect_is(gslist, "GatingSetList");
      gs <<- rbind2(gslist)
      
      new_samples <- sampleNames(gs) 
      expect_identical(new_samples, c(orig_sn, clone_sn))
      
    })


test_that("lapply ",{
      
      gh_list <- lapply(gs, function(x)x)
      expect_is(gh_list, "list");
      expect_equal(length(gh_list), 2);
      
      res <- lapply(gh_list,function(gh){
        expect_is(gh, "GatingHierarchy")
      })
      
    })



test_that("pData ",{
      
      pd <- pData(gs)
      expect_is(pd, "data.frame");
      
      new_pd <- pd
      new_pd[, "PTID"] <- c("001", "002")
      pData(gs) <- new_pd 
      expect_identical(pData(gs), new_pd);
      
      #restore pdata
      pData(gs) <- pd
      expect_identical(pData(gs), pd);
    })

test_that("[ subsetting",{
      
      gs_sub <- gs[2]
      expect_is(gs_sub, "GatingSet");
      gs_sub <- gs["CytoTrol_CytoTrol_2.fcs"]
      expect_is(gs_sub, "GatingSet");
      #c data structure does not change     
      expect_true(identical(gs@pointer,gs_sub@pointer));
      
      expect_equal(length(gs_sub), 1)
    })


