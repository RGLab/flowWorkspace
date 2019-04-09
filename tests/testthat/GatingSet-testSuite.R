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
test_that("gs_get_data ",{
      
      ncfs <- gs_get_data(gs)
      expect_is(ncfs, "ncdfFlowSet");
      expect_equal(nrow(ncfs[[1]]), 119531)
      ncfs <- gs_get_data(gs, "root")
      expect_equal(nrow(ncfs[[1]]), 119531)
      
      ncfs <- gs_get_data(gs, "singlets")
      expect_equal(nrow(ncfs[[1]]), 87022)
      expect_is(gs[[1]], "GatingHierarchy");
      
      
    })


test_that("gs_cyto_data ",{
      
      ncfs <- gs_cyto_data(gs)
      expect_is(ncfs, "ncdfFlowSet");
      expect_equal(nrow(ncfs[[1]]), 119531)
      
      fs <- as(ncfs, "flowSet")
      gs_cyto_data(gs) <- fs 
      expect_is(gs_cyto_data(gs), "flowSet");
      
      #restore data
      gs_cyto_data(gs) <- ncfs
      expect_is(gs_cyto_data(gs), "ncdfFlowSet");
    })


test_that("gs_clone & gslist_to_gs",{
      
      expect_message(gs_cloned <- gs_clone(gs), "copying data")
      expect_is(gs_cloned, "GatingSet");
      
      #check data consistency
      fs1 <- gs_get_data(gs)
      fs2 <- gs_get_data(gs_cloned)
      expect_equal(fs1[[1]], fs2[[1]], tol = 1e-05)
      
      orig_sn <- sampleNames(gs)
      clone_sn <- sampleNames(gs_cloned)
      expect_identical(orig_sn, clone_sn)
      
      #check tree consistentcy
      expect_identical(gs_get_pop_paths(gs[[1]]), gs_get_pop_paths(gs_cloned[[1]]))
      
      #check if trans is preserved
      expect_equal(gh_get_transformations(gs[[1]]), gh_get_transformations(gs_cloned[[1]]))
      
      expect_equal(gs_get_pop_stats(gs), gs_get_pop_stats(gs_cloned))
      expect_equal(gs_get_pop_stats(gs[[1]]), gs_get_pop_stats(gs_cloned[[1]]))
      
      #clone without copying hdf data
      expect_message(gs_clone1 <- gs_clone(gs, isNew = FALSE), "cloned")
      expect_equal(gs_get_data(gs_clone1)@file, fs1@file)
      
      #construct gslist to gslist_to_gs
      sampleNames(gs_cloned) <- "CytoTrol_CytoTrol_2.fcs"
      clone_sn <- sampleNames(gs_cloned)
      gslist <- GatingSetList(list(gs, gs_cloned))
      expect_is(gslist, "GatingSetList");
      suppressMessages(gs <<- gslist_to_gs(gslist))
      
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
      name_stored_in_cpp <- flowWorkspace:::.cpp_getSamples(gs@pointer)
      expect_equal(sort(new_sn), sort(name_stored_in_cpp))
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

test_that("subset.GatingSet",{
  # GvHD has more samples for this test
  gs_full <- GatingSet(GvHD)
  gs_sub <- subset(gs_full, Visit == 2)
  expect_equal(length(gs_sub), 5)
})

test_that("gs_get_gate for gs",{
      
      thisRes <- gs_get_gate(gs, "CD3+")
      expectRes <- readRDS(file.path(resultDir, "getGate_gs_ellipse.rds"))
      expect_equal(thisRes, expectRes, tol = 5e-04)
      
      thisRes <- gs_get_gate(gs, "singlets")
      expectRes <- readRDS(file.path(resultDir, "getGate_gs_polygon.rds"))
      expect_equal(thisRes, expectRes, tol = 2e-08)
    })

test_that("preporcess the gating tree to prepare for the plotGate",{
      
      #no formula
      stats <- 0.99
      xParam <- "<B710-A>"
      yParam <- "<R780-A>"
      expect_value <- list(gates = gs_get_gate(gs, "CD4")
                            , xParam = xParam
                            , yParam = yParam
                            , stats = stats
                            , isBool = FALSE
                        )
      
      myValue <- flowWorkspace:::.preplot(gs, "CD4", "xyplot", stats = stats, formula = NULL, default.y = "SSC-A")
      expect_equal(myValue, expect_value)
      
            
      #with formula override the gate x,y
      f1 <- `FSC-A` ~ `SSC-A` | PTID + VISITNO + STIM
      
      expect_value["xParam"] <- "SSC-A" 
      expect_value["yParam"] <- "FSC-A"
      myValue <- flowWorkspace:::.preplot(gs, "CD4", "xyplot", stats = stats, formula = f1, default.y = "SSC-A")
      expect_equal(myValue, expect_value)
      
      samples <- sampleNames(gs)
      
      #miss stats argument
      expect_value[["stats"]] <- sapply(samples, function(sn)gh_get_proportion(gs[[sn]], gs_get_pop_paths(gs[[sn]])[5]), simplify = FALSE)
      myValue <- flowWorkspace:::.preplot(x = gs, y = "CD4", type = "xyplot", formula = f1, default.y = "SSC-A")
      expect_equal(myValue, expect_value)

      #y is a list
      expect_value[["stats"]] <- sapply(samples,function(thisSample){
                                          lapply(7:8,function(thisY){
                                                curGh <- gs[[thisSample]]
                                                gh_get_proportion(curGh,gs_get_pop_paths(curGh,showHidden=TRUE)[thisY],xml = F)
                                              })
                                        },simplify = FALSE)
      curGates <- sapply(samples,function(curSample){
            
            filters(lapply(gs_get_pop_paths(gs)[7:8],function(y)gh_get_gate(gs[[curSample]],y)))
          },simplify=F)
      xParam <- "<R660-A>"
      yParam <- "<V545-A>"
      expect_value[["gates"]] <- as(curGates,"filtersList")
      expect_value[["xParam"]] <- xParam
      expect_value[["yParam"]] <- yParam
      
      myValue <- flowWorkspace:::.preplot(x = gs, y = list(popIds=gs_get_pop_paths(gs)[7:8]), type = "xyplot", formula = NULL, default.y = "SSC-A")

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
#      eInd <- gh_get_indices(gs[[1]], nodeInd)
#      thisRes <- .getOverlay(gs, overlay = eInd, params = chnls)
#      expect_is(thisRes, "flowSet")
#      expect_equal(sampleNames(thisRes), samples)
#      expect_equivalent(as.vector(fsApply(thisRes,nrow)), c(1309, 1309))
#      
#      #by a list of event indices
#      eInd <- lapply(gs, gh_get_indices, y = nodeInd)
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
    
test_that("gs_set_node_name",{
    
    nodeName <- gs_get_pop_paths(gs[[1]])[3]
	gs_set_node_name(gs, "singlets", "S")
    lapply(gs, function(gh){
          expect_equal(gs_get_pop_paths(gh)[3], "/not debris/S")
        }) 
	gs_set_node_name(gs, "S", "singlets")
    invisible()
    
    
  })

test_that("gs_get_pop_stats",{
  
      thisRes <- gs_get_pop_stats(gs, path = "full", format = "wide")
      expect_is(thisRes, "matrix")
      
      expectRes <- fread(file.path(resultDir, "getPopStats_gs.csv"))
      expect_equal(rownames(thisRes),expectRes[["V1"]])#check rownames
      
      expect_equal(as.data.table(thisRes), expectRes[,-1, with = F], tol = 2e-3)
      
      #use auto path
      stats_wide <- gs_get_pop_stats(gs, format = "wide", path = "auto")
      stats_wide <- stats_wide[-match("root", rownames(stats_wide)), ] #remove root
      stats_wide <- as.data.frame(stats_wide)
      #get long format
      stats_long <- gs_get_pop_stats(gs, format = "long", path = "auto")
      
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
      
      thisRes <- flowWorkspace:::.computeCV(gs)
      expect_is(thisRes, "matrix")
      
      expectRes <- fread(file.path(resultDir, "cv_gs.csv"))
      expect_equal(rownames(thisRes),expectRes[["V1"]])#check rownames
      tol <- ifelse(isCpStaticGate, 2e-2, 1.5e-8)
      expect_equal(as.data.table(thisRes), expectRes[,-1, with = F], tol = tol)
      
    })

test_that("keyword",{
      
      thisRes <- keyword(gs)
      expect_is(thisRes, "list")
      expectRes <- readRDS(file.path(resultDir, "kw_gs.rds"))
      #eliminate the local path difference
      expectRes <- lapply(expectRes, function(thisResult){
            thisResult[["FILENAME"]] <- basename(thisResult[["FILENAME"]])
            #fix this due to the difference generated from the constructor GatingSet(gh, files)
            thisResult[["ORIGINALGUID"]] <-thisResult[["GUID"]] <- basename(thisResult[["FILENAME"]])
            #skip flowCore_R keys due to the historical archived results do not have this info up to date
            ind <- !grepl("(flowCore_\\$P)|(transformation)",names(thisResult))
            thisResult <- thisResult[ind]
            
            #fix legacy result
            if(!islegacyArchivedGS)
              thisResult[paste0("$P",5:11, "N")] <- paste0("<", thisResult[paste0("$P",5:11, "N")], ">")
            thisResult
          })
      thisRes <- lapply(thisRes, function(thisResult){
            thisResult[["FILENAME"]] <- basename(thisResult[["FILENAME"]])
            thisResult[["ORIGINALGUID"]] <-thisResult[["GUID"]] <- basename(thisResult[["FILENAME"]])
            ind <- !grepl("(flowCore_\\$P)|(transformation)",names(thisResult))
            thisResult[ind]
          })
      expect_equal(thisRes,expectRes)
      
      thisRes <- keyword(gs, "$P1N")
      
      expect_equal(thisRes, data.frame(`$P1N` = c("FSC-A", "FSC-A"), check.names = FALSE))
    })

#test_that("getIndices for COMPASS",{
#      
#      thisRes <- getIndices(gs,quote(`CD8/38- DR+|CD8/CCR7- 45RA+`)) 
#      expectRes <- readRDS(file.path(resultDir, "getIndices_gs.rds"))
#      tol <- ifelse(isCpStaticGate, 1e-2, 1.5e-8)
#      expect_equal(sum(thisRes[[1]]), sum(expectRes[[1]]), tol = tol)
#      expect_equal(sum(thisRes[[2]]), sum(expectRes[[2]]), tol = tol)
#    })

test_that("add", {
      filterslist1 <- lapply(gs, function(gh){
            cd4_gate  <- gh_get_gate(gh, "CD4")
            cd4_gate@filterId <- "CD4_test"
            cd8_gate  <- gh_get_gate(gh, "CD8")
            cd8_gate@filterId <- "CD8_test"
            filters(list(cd4_gate,cd8_gate))
          })
      
      #add without name
      Ids <- gs_add_gate(gs, filterslist1)
      expect_equal(Ids, c(25,26))
      cd4_gate  <- gs_get_gate(gs, "CD4")
      cd4_gate <- lapply(cd4_gate, function(g){
            g@filterId <- "CD4_test"
            g
          })
      expect_equal(gs_get_gate(gs, "CD4_test"), cd4_gate) 
      gs_remove_gate("CD4_test", gs)
      gs_remove_gate("CD8_test", gs)
      
      #customize names
      expect_error(gs_add_gate(gs, filterslist1, names = c("CD4_demo")), "number of population names ")
      expect_error(gs_add_gate(gs, filterslist1, names = c("CD4_demo", "CD4_demo")), "not unqiue")
      
      Ids <- gs_add_gate(gs, filterslist1, names = c("CD4_demo", "CD8_demo"))
      expect_equal(Ids, c(25,26))
      cd4_gate  <- gs_get_gate(gs, "CD4")
      cd4_gate <- lapply(cd4_gate, function(g){
            g@filterId <- "CD4_demo"
            g
          })
      expect_equal(gs_get_gate(gs, "CD4_demo"), cd4_gate) 
      gs_remove_gate("CD4_demo", gs)
      gs_remove_gate("CD8_demo", gs)
    })
#TODO:write test cases for save_gs /load_gs 
if(!isCpStaticGate)
{
  test_that("gs_get_singlecell_expression for COMPASS",{
    nodes <- c('CD8/38- DR+', 'CD8/CCR7- 45RA+')
    
    thisRes <- gs_get_singlecell_expression(gs,  nodes, map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")) 
    expectRes <- readRDS(file.path(resultDir, "getData_COMPASS_gs.rds"))
    #      browser()
    
    expect_equal(thisRes,expectRes,tol = 1e-07)
    
    #test other.markers (redundant marker should be merged automatically)
    thisRes <- gs_get_singlecell_expression(gs, nodes
                                       , map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")
                                       , other.markers = c("CD4", "CD38 APC")
                                       , threshold = FALSE
    )
    expect_equal(colnames(thisRes[[1]]), c("CD38 APC", "CCR7 PE", "CD4 PcpCy55"))
    
    #swap
    expect_warning(thisRes2 <- gs_get_singlecell_expression(gs, nodes
                                                       , map = list("CD8/38- DR+" = "R660", "CD8/CCR7- 45RA+" = "G560")
                                                       , other.markers = c("B710", "R660")
                                                       , threshold = FALSE
                                                       , swap = T
    ), "partially matched")
    expect_equivalent(thisRes2,thisRes)
    expect_equal(colnames(thisRes2[[1]]), c("<R660-A>", "<G560-A>", "<B710-A>"))
    
    #error
    expect_error(gs_get_singlecell_expression(gs, nodes
                                         , map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")
                                         , other.markers = c("CD4", "CD38 APC")
    ), "number of markers")
    #marginal = FALSE
    thisRes <- gs_get_singlecell_expression(gs, nodes, marginal = FALSE)
    expect_equal(thisRes[[1]][,c("CD38 APC", "CCR7 PE")],expectRes[[1]],tol = 1e-07)
    expect_equal(thisRes[[1]][,1] == 0, thisRes[[1]][,2] == 0)
    
    #gates share the same marker
    nodes <- c('CD8/38- DR+', "CD8/38+ DR-", 'CD8/CCR7- 45RA+')
    thisRes <- gs_get_singlecell_expression(gs, nodes, marginal = FALSE)
    #verify the results by calling R routines
    nodes.expr <- quote(`CD8/38- DR+|CD8/38+ DR-|CD8/CCR7- 45RA+`)
    ind.total <- getIndices(gs[1], nodes.expr)[[1]]
    ind.mat <- getIndiceMat(gs[[1]], nodes.expr)
    #Or the ind for the same marker from nodes
    ind.DR <- ind.38 <- ind.mat[,1] | ind.mat[,2]
    ind.CCR <- ind.45 <- ind.mat[,3]
    #masking
    mat <- exprs(gh_get_data(gs[[1]]))[,c(6, 9, 10, 11)]
    mat <- mat * c(ind.38, ind.DR, ind.CCR, ind.45)
    mat <- mat[ind.total, ]
    expect_equal(thisRes[[1]], mat, check.attributes = FALSE)
    
  })
  
}


test_that("markernames", {
  markers <- c('CD4 PcpCy55','CD38 APC','CD8 APCH7','CD3 V450','HLA-DR V500','CCR7 PE','CD45RA PECy7')
  expect_equal(markernames(gs), markers)
  
  markers.new <- c("CD4", "CD8")
  chnls <- c("<B710-A>", "<R780-A>")
  names(markers.new) <- chnls
  gh <- gs[[1]]
  markernames(gh) <- markers.new
  expect_equivalent(markernames(gh)[c(1,3)], markers.new)
  
  expect_warning(res <- markernames(gs), "not consistent")
  expect_equal(unique(lapply(gs, markernames)), res)

  
  markernames(gs) <-  markers.new
  expect_equivalent(markernames(gs)[c(1,3)], markers.new)
  
  #restore original markers
  markers.orig <- markers[c(1,3)]
  names(markers.orig) <- chnls
  markernames(gs) <-  markers.orig
  expect_equal(markernames(gs), markers)
  
})

test_that("colnames", {
  chnls <- c('FSC-A','FSC-H','FSC-W','SSC-A','<B710-A>','<R660-A>','<R780-A>','<V450-A>','<V545-A>','<G560-A>','<G780-A>','Time')
  expect_equal(colnames(gs), chnls)
  
  chnls.new <- chnls
  chnls.new[c(1,4)] <- c("fsc", "ssc")
  
  gh <- gs[[1]]
  expect_error(colnames(gh) <- chnls.new, "Can't change")
  
  colnames(gs) <-  chnls.new
  expect_equal(colnames(gs), chnls.new)
  expect_equal(colnames(gs[[1]]), chnls.new)
  expect_equal(colnames(gs[[2]]), chnls.new)
  
  #restore original chnls
  colnames(gs) <-  chnls
  expect_equal(colnames(gs), chnls)
  expect_equal(colnames(gs[[1]]), chnls)
  expect_equal(colnames(gs[[2]]), chnls)
  
})
