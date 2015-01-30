context("GatingSet Accessors")

test_that("isNcdf ",{
      
        
	expect_equal(isNcdf(gs), TRUE);
    
})
test_that("length ",{
      expect_equal(length(gs), 1)
      
    })
test_that("show ",{
      expect_output(show(gs), "A GatingSet with 1 samples")
      
    })
test_that("getData ",{
      
      ncfs <- getData(gs)
      expect_is(ncfs, "ncdfFlowSet");
      expect_equal(nrow(ncfs[[1]]), 119531)
      ncfs <- getData(gs, "root")
      expect_equal(nrow(ncfs[[1]]), 119531)
      
      ncfs <- getData(gs, "singlets")
      expect_equal(nrow(ncfs[[1]]), 87022)
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


test_that("clone & rbind2",{
      
      suppressMessages(invisible(capture.output(gs_clone <- clone(gs))))
      expect_is(gs_clone, "GatingSet");
      
      #check data consistency
      fs1 <- getData(gs)
      fs2 <- getData(gs_clone)
      expect_equal(fs1[[1]], fs2[[1]], tol = 1e-05)
      
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
      suppressMessages(gs <<- rbind2(gslist))
      
      new_samples <- sampleNames(gs) 
      expect_identical(new_samples, c(orig_sn, clone_sn))
      
    })
test_that("sampleNames",{
      
      sn <- sampleNames(gs)
      expect_equal(sn, c("CytoTrol_CytoTrol_1.fcs", "CytoTrol_CytoTrol_2.fcs"))
      
      #update name
      sampleNames(gs)[1] <- "newSample"
      new_sn <- sampleNames(gs)
      expect_equal(new_sn, c("newSample", "CytoTrol_CytoTrol_2.fcs"))
      #check if sample name is also updated in cpp data structure
      name_stored_in_cpp <- .Call("R_getSamples", gs@pointer)
      expect_equal(new_sn, rev(name_stored_in_cpp))
      #restore the original name
      sampleNames(gs) <- sn
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
      expect_false(identical(gs_sub@guid, gs@guid))
      expect_equal(length(gs_sub), 1)
    })

test_that("getGate for gs",{
      
      thisRes <- getGate(gs, "CD3+")
      expectRes <- readRDS(file.path(resultDir, "getGate_gs_ellipse.rds"))
      expect_equal(thisRes, expectRes)
      
      thisRes <- getGate(gs, "singlets")
      expectRes <- readRDS(file.path(resultDir, "getGate_gs_polygon.rds"))
      expect_equal(thisRes, expectRes)
    })

test_that("preporcess the gating tree to prepare for the plotGate",{
      
      #no formula
      stats <- 0.99
      xParam <- "<B710-A>"
      yParam <- "<R780-A>"
      expect_value <- list(gates = getGate(gs, "CD4")
                            , xParam = xParam
                            , yParam = yParam
                            , stats = stats
                            , isBool = FALSE
                        )
      
      myValue <- .preplot(gs, "CD4", "xyplot", stats = stats, formula = NULL, default.y = "SSC-A")
      expect_equal(myValue, expect_value)
      
            
      #with formula override the gate x,y
      f1 <- `FSC-A` ~ `SSC-A` | PTID + VISITNO + STIM
      
      expect_value["xParam"] <- "SSC-A" 
      expect_value["yParam"] <- "FSC-A"
      myValue <- .preplot(gs, "CD4", "xyplot", stats = stats, formula = f1, default.y = "SSC-A")
      expect_equal(myValue, expect_value)
      
      samples <- sampleNames(gs)
      
      #miss stats argument
      expect_value[["stats"]] <- sapply(samples, function(sn)getProp(gs[[sn]], getNodes(gs[[sn]])[5]), simplify = FALSE)
      myValue <- .preplot(x = gs, y = "CD4", type = "xyplot", formula = f1, default.y = "SSC-A")
      expect_equal(myValue, expect_value)

      #y is a list
      expect_value[["stats"]] <- sapply(samples,function(thisSample){
                                          lapply(7:8,function(thisY){
                                                curGh <- gs[[thisSample]]
                                                getProp(curGh,getNodes(curGh,showHidden=TRUE)[thisY],flowJo = F)
                                              })
                                        },simplify = FALSE)
      curGates <- sapply(samples,function(curSample){
            
            filters(lapply(getNodes(gs)[7:8],function(y)getGate(gs[[curSample]],y)))
          },simplify=F)
      xParam <- "<R660-A>"
      yParam <- "<V545-A>"
      expect_value[["gates"]] <- as(curGates,"filtersList")
      expect_value[["xParam"]] <- xParam
      expect_value[["yParam"]] <- yParam
      
      myValue <- .preplot(x = gs, y = list(popIds=getNodes(gs)[7:8]), type = "xyplot", formula = NULL, default.y = "SSC-A")

      expect_identical(myValue, expect_value)
      

    })

    
#test_that("getOverlay",{
#      
#      nodeInd <- "CD8/38- DR+"
#      samples <- sampleNames(gs)
#      chnls <- c("SSC-A", "FSC-A")
#      #by one gate index
#      thisRes <- .getOverlay(gs, overlay = nodeInd, params = chnls)
#      expect_is(thisRes[[1]], "ncdfFlowSet")
#      expect_equal(names(thisRes), nodeInd)
#      expect_equal(sampleNames(thisRes[[1]]), samples)
#      expect_equal(colnames(thisRes), chnls)
#      expect_equivalent(nrow(thisRes[[1]]), 1309)
#      
#      #by one event indice
#      eInd <- getIndices(gs[[1]], nodeInd)
#      thisRes <- .getOverlay(gs, overlay = eInd, params = chnls)
#      expect_is(thisRes, "flowSet")
#      expect_equal(sampleNames(thisRes), samples)
#      expect_equivalent(as.vector(fsApply(thisRes,nrow)), c(1309, 1309))
#      
#      #by a list of event indices
#      eInd <- lapply(gs, getIndices, y = nodeInd)
#      thisRes <- .getOverlay(gs, overlay = eInd, params = chnls)
#      expect_is(thisRes, "list")
#      expect_equal(names(thisRes), samples)
#      expect_equivalent(sapply(thisRes,class), c("flowFrame", "flowFrame"))
#      expect_equivalent(sapply(thisRes,nrow), c(1309, 1309))
#      
#      #by a list of gate/event indices
#      nodeInd <- list("CD8/38- DR+", "CD8/38- DR-")
#      expect_error(.getOverlay(gs, overlay = nodeInd, params = chnls), "names of overlay list does not agree with sampleNames in GatingSet")
#      names(nodeInd) <- samples
#      thisRes <- .getOverlay(gs, overlay = nodeInd, params = chnls)
#      expect_is(thisRes, "list")
#      expect_equal(names(thisRes), samples)
#      expect_equivalent(sapply(thisRes,class), c("flowFrame", "flowFrame"))
#      expect_equivalent(as.vector(sapply(thisRes,nrow)), c(1309, 7473))
#      
#    })
    
test_that("setNode",{
    
    nodeName <- getNodes(gs[[1]])[3]
    setNode(gs, "singlets", "S")
    lapply(gs, function(gh){
          expect_equal(getNodes(gh)[3], "/not debris/S")
        }) 
    setNode(gs, "S", "singlets")
    invisible()
    
    
  })

test_that("getPopStats",{
  
      thisRes <- getPopStats(gs, path = "full", format = "wide")
      expect_is(thisRes, "matrix")
      
      expectRes <- fread(file.path(resultDir, "getPopStats_gs.csv"))
      expect_equal(rownames(thisRes),expectRes[["V1"]])#check rownames
      
      expect_equal(as.data.table(thisRes), expectRes[,-1, with = F])
      
      #use auto path
      stats_wide <- getPopStats(gs, format = "wide", path = "auto")
      stats_wide <- stats_wide[-match("root", rownames(stats_wide)), ] #remove root
      stats_wide <- as.data.frame(stats_wide)
      #get long format
      stats_long <- getPopStats(gs, format = "long")
      
      #convert it to wide to do the comparsion
      stats_long[, value := Count/ParentCount]
      stats_long <- dcast.data.table(stats_long[, list(Population,name, value)],  Population~name)
      rn <- stats_long[, Population]
      stats_long[, Population := NULL]
      stats_long <- as.data.frame(stats_long)
      rownames(stats_long) <- rn
      stats_long <- stats_long[rownames(stats_wide), colnames(stats_wide)]
      
      expect_equal(stats_long, stats_wide)
      
      
})

test_that("compute CV from gs",{
      
      thisRes <- .computeCV(gs)
      expect_is(thisRes, "matrix")
      
      expectRes <- fread(file.path(resultDir, "cv_gs.csv"))
      expect_equal(rownames(thisRes),expectRes[["V1"]])#check rownames
      
      expect_equivalent(as.data.table(thisRes), expectRes[,-1, with = F])
      
    })

test_that("keyword",{
      
      thisRes <- keyword(gs)
      expect_is(thisRes, "list")
      expectRes <- readRDS(file.path(resultDir, "kw_gs.rds"))
      #eliminate the local path difference
      expectRes <- lapply(expectRes, function(thisResult){
            thisResult[["FILENAME"]] <- basename(thisResult[["FILENAME"]])
            thisResult
          })
      thisRes <- lapply(thisRes, function(thisResult){
            thisResult[["FILENAME"]] <- basename(thisResult[["FILENAME"]])
            thisResult
          })
       
      
      expect_equal(thisRes,expectRes)
      
      thisRes <- keyword(gs, "$P1N")
      
      expect_equal(thisRes, data.frame(`$P1N` = c("FSC-A", "FSC-A"), check.names = FALSE))
    })

test_that("getIndices for COMPASS",{
      
      thisRes <- getIndices(gs,quote(`CD8/38- DR+|CD8/CCR7- 45RA+`)) 
      expectRes <- readRDS(file.path(resultDir, "getIndices_gs.rds"))
      expect_equal(thisRes,expectRes)
      
    })

test_that("add", {
      filterslist1 <- lapply(gs, function(gh){
            cd4_gate  <- getGate(gh, "CD4")
            cd4_gate@filterId <- "CD4_test"
            cd8_gate  <- getGate(gh, "CD8")
            cd8_gate@filterId <- "CD8_test"
            filters(list(cd4_gate,cd8_gate))
          })
      
      #add without name
      Ids <- add(gs, filterslist1)
      expect_equal(Ids, c(25,26))
      cd4_gate  <- getGate(gs, "CD4")
      cd4_gate <- lapply(cd4_gate, function(g){
            g@filterId <- "CD4_test"
            g
          })
      expect_equal(getGate(gs, "CD4_test"), cd4_gate) 
      Rm("CD4_test", gs)
      Rm("CD8_test", gs)
      
      #customize names
      expect_error(add(gs, filterslist1, names = c("CD4_demo")), "number of population names ")
      expect_error(add(gs, filterslist1, names = c("CD4_demo", "CD4_demo")), "not unqiue")
      
      Ids <- add(gs, filterslist1, names = c("CD4_demo", "CD8_demo"))
      expect_equal(Ids, c(25,26))
      cd4_gate  <- getGate(gs, "CD4")
      cd4_gate <- lapply(cd4_gate, function(g){
            g@filterId <- "CD4_demo"
            g
          })
      expect_equal(getGate(gs, "CD4_demo"), cd4_gate) 
      Rm("CD4_demo", gs)
      Rm("CD8_demo", gs)
    })
#TODO:write test cases for save_gs /load_gs 

test_that("getSingleCellExpression for COMPASS",{
      
      thisRes <- getSingleCellExpression(gs, c('CD8/38- DR+', 'CD8/CCR7- 45RA+') , map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")) 
      expectRes <- readRDS(file.path(resultDir, "getData_COMPASS_gs.rds"))
      expect_equivalent(thisRes,expectRes)
      
    })

