#context("GatingSetList Accessors")
#
#test_that("isNcdf ",{
#      
#        
#	expect_equal(isNcdf(gs), TRUE);
#    
#})
#test_that("length ",{
#      expect_equal(length(gs), 2)
#      
#    })
#test_that("show ",{
#      expect_output(show(gs), "A GatingSet with 2 samples")
#      
#    })
#test_that("getData ",{
#      
#      ncfs <- getData(gs)
#      expect_is(ncfs, "ncdfFlowSet");
#      expect_equal(nrow(ncfs[[1]]), 119531)
#      ncfs <- getData(gs, 0)
#      expect_equal(nrow(ncfs[[1]]), 119531)
#      
#      ncfs <- getData(gs, "CD8")
#      expect_equal(nrow(ncfs[[1]]), 14623)
#      ncfs <- getData(gs, 14)
#      expect_equal(nrow(ncfs[[1]]), 14623)
#      expect_is(gs[[1]], "GatingHierarchy");
#      
#      
#    })
#
#
#test_that("flowData ",{
#      
#      ncfs <- flowData(gs)
#      expect_is(ncfs, "ncdfFlowSet");
#      expect_equal(nrow(ncfs[[1]]), 119531)
#      
#      fs <- as(ncfs, "flowSet")
#      flowData(gs) <- fs 
#      expect_is(flowData(gs), "flowSet");
#      
#      #restore data
#      flowData(gs) <- ncfs
#      expect_is(flowData(gs), "ncdfFlowSet");
#    })
#
#test_that("sampleNames",{
#      
#      sn <- sampleNames(gs)
#      expect_equal(sn, "CytoTrol_CytoTrol_1.fcs")
#      
#      #update name
#      sampleNames(gs) <- "newSample"
#      new_sn <- sampleNames(gs)
#      expect_equal(new_sn, "newSample")
#      #check if sample name is also updated in cpp data structure
#      name_stored_in_cpp <- .Call("R_getSamples", gs@pointer)
#      expect_equal(new_sn, name_stored_in_cpp)
#      #restore the original name
#      sampleNames(gs) <- sn
#    })
#
#test_that("clone & rbind2",{
#      
#      gs_clone <- clone(gs)
#      expect_is(gs_clone, "GatingSet");
#      
#      #check data consistency
#      fs1 <- getData(gs)
#      fs2 <- getData(gs_clone)
#      expect_identical(fs1[[1]], fs2[[1]])
#      
#      orig_sn <- sampleNames(gs)
#      clone_sn <- sampleNames(gs_clone)
#      expect_identical(orig_sn, clone_sn)
#      
#      #check tree consistentcy
#      expect_identical(getNodes(gs[[1]]), getNodes(gs_clone[[1]]))
#      
#      #construct gslist to rbind2
#      sampleNames(gs_clone) <- "CytoTrol_CytoTrol_2.fcs"
#      clone_sn <- sampleNames(gs_clone)
#      gslist <- GatingSetList(list(gs, gs_clone))
#      expect_is(gslist, "GatingSetList");
#      gs <<- rbind2(gslist)
#      
#      new_samples <- sampleNames(gs) 
#      expect_identical(new_samples, c(orig_sn, clone_sn))
#      
#    })
#
#
#test_that("lapply ",{
#      
#      gh_list <- lapply(gs, function(x)x)
#      expect_is(gh_list, "list");
#      expect_equal(length(gh_list), 2);
#      
#      res <- lapply(gh_list,function(gh){
#        expect_is(gh, "GatingHierarchy")
#      })
#      
#    })
#
#
#
#test_that("pData ",{
#      
#      pd <- pData(gs)
#      expect_is(pd, "data.frame");
#      
#      new_pd <- pd
#      new_pd[, "PTID"] <- c("001", "002")
#      pData(gs) <- new_pd 
#      expect_identical(pData(gs), new_pd);
#      
#      #restore pdata
#      pData(gs) <- pd
#      expect_identical(pData(gs), pd);
#    })
#
#test_that("[ subsetting",{
#      
#      gs_sub <- gs[2]
#      expect_is(gs_sub, "GatingSet");
#      gs_sub <- gs["CytoTrol_CytoTrol_2.fcs"]
#      expect_is(gs_sub, "GatingSet");
#      #c data structure does not change     
#      expect_true(identical(gs@pointer,gs_sub@pointer));
#      expect_false(identical(gs_sub@guid, gs@guid))
#      expect_equal(length(gs_sub), 1)
#    })
#
#test_that("preporcess the gating tree to prepare for the plotGate",{
#      
#      f1 <- `FSC-A` ~ `SSC-A` | PTID + VISITNO + STIM
#      
#      stats <- 0.99
#      xParam <- "<B710-A>"
#      names(xParam) <- "<B710-A>"
#      yParam <- "<R780-A>"
#      names(yParam) <- "<R780-A>"
#      expect_value <- list(gates = getGate(gs, "CD4")
#                            , xParam = xParam
#                            , yParam = yParam
#                            , stats = stats
#                            , isBool = FALSE
#                          )
#                        
#      myValue <- .preplot(gs, 5, "xyplot", stats = stats, formula = f1, default.y = "SSC-A")
#      expect_equal(myValue, expect_value)
#      
#      samples <- sampleNames(gs)
#      
#      #miss stats argument
#      expect_value[["stats"]] <- sapply(samples, function(sn)getProp(gs[[sn]], getNodes(gs[[sn]])[5]), simplify = FALSE)
#      myValue <- .preplot(x = gs, y = 5, type = "xyplot", formula = f1, default.y = "SSC-A")
#      expect_equal(myValue, expect_value)
#
#      #y is a list
#      expect_value[["stats"]] <- sapply(samples,function(thisSample){
#                                          lapply(7:8,function(thisY){
#                                                curGh <- gs[[thisSample]]
#                                                getProp(curGh,getNodes(curGh,showHidden=TRUE)[thisY],flowJo = F)
#                                              })
#                                        },simplify = FALSE)
#      curGates<-sapply(samples,function(curSample){
#            
#            filters(lapply(7:8,function(y)getGate(gs[[curSample]],y)))
#          },simplify=F)
#      xParam <- "<R660-A>"
#      names(xParam) <- "<R660-A>"
#      yParam <- "<V545-A>"
#      names(yParam) <- "<V545-A>"
#      expect_value[["xParam"]] <- xParam
#      expect_value[["yParam"]] <- yParam
#      
#      myValue <- .preplot(x = gs, y = list(popIds=7:8), type = "xyplot", formula = f1, default.y = "SSC-A")
#      expect_equal(myValue, expect_value)
#      
#    })
#    
#test_that("setNode",{
#    
#    nodeName <- getNodes(gs[[1]])[3]
#    setNode(gs, "singlets", "S")
#    lapply(gs, function(gh){
#          expect_equal(getNodes(gh)[3], "/not debris/S")
#        }) 
#    setNode(gs, "S", "singlets")
#    invisible()
#    
#    
#  })
#
#test_that("getPopStats",{
#  
#      thisRes <- getPopStats(gs)
#      expect_is(thisRes, "matrix")
#      
#      expectRes <- fread(file.path(resultDir, "getPopStats_gs.csv"))
#      expect_equal(rownames(thisRes),expectRes[["V1"]])#check rownames
#      
#      expect_equal(as.data.table(thisRes), expectRes[,-1, with = F])
#      
#})
#
#test_that("compute CV from gs",{
#      
#      thisRes <- .computeCV(gs)
#      expect_is(thisRes, "matrix")
#      
#      expectRes <- fread(file.path(resultDir, "cv_gs.csv"))
#      expect_equal(rownames(thisRes),expectRes[["V1"]])#check rownames
#      
#      expect_equal(as.data.table(thisRes), expectRes[,-1, with = F])
#      
#    })
#
#test_that("keyword",{
#      
#      thisRes <- keyword(gs)
#      expect_is(thisRes, "list")
#      expectRes <- readRDS(file.path(resultDir, "kw_gs.rds"))
#      expect_equal(thisRes,expectRes)
#      
#      thisRes <- keyword(gs, "$P1N")
#      
#      expect_identical(thisRes, data.frame(`$P1N` = c("FSC-A", "FSC-A"), check.names = FALSE))
#    })
#
#test_that("getIndices for COMPASS",{
#      
#      thisRes <- getIndices(gs,quote(`CD8/38- DR+|CD8/CCR7- 45RA+`)) 
#      expectRes <- readRDS(file.path(resultDir, "getIndices_gs.rds"))
#      expect_identical(thisRes,expectRes)
#      
#    })
#
#test_that("getData for COMPASS",{
#      
#      thisRes <- getData(gs,quote(`CD8/38- DR+|CD8/CCR7- 45RA+`) , list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")) 
#      expectRes <- readRDS(file.path(resultDir, "getData_COMPASS_gs.rds"))
#      expect_identical(thisRes,expectRes)
#      
#    })
#
