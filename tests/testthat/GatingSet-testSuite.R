context("GatingSet Accessors")

test_that("length ",{
      expect_equal(length(gs), 1)
      
    })
test_that("show ",{
      expect_output(show(gs), "A GatingSet with 1 samples")
      
    })
test_that("getData ",{
      
      ncfs <- getData(gs)
      expect_is(ncfs, "cytoset");
      expect_equal(nrow(ncfs[[1]]), 119531)
      ncfs <- getData(gs, "root")
      expect_equal(nrow(ncfs[[1]]), 119531)
      
      ncfs <- getData(gs, "singlets")
      expect_equal(nrow(ncfs[[1]]), 87022)
      expect_is(gs[[1]], "GatingHierarchy");
      
      
    })


test_that("flowData ",{
      
      ncfs <- flowData(gs)
      expect_is(ncfs, "cytoset");
      expect_equal(nrow(ncfs[[1]]), 119531)
      
      # fs <- as(ncfs, "flowSet")
      # flowData(gs) <- fs 
      # expect_is(flowData(gs), "flowSet");
      
      #restore data
      flowData(gs) <- ncfs
      expect_is(flowData(gs), "cytoset");
    })


test_that("clone & rbind2",{
      
      gs1 <- gs_clone(gs)
      
      expect_is(gs1, "GatingSet");
      
      #check data consistency
      fs1 <- getData(gs)
      fs2 <- getData(gs1)
      expect_equal(fs1[[1]], fs2[[1]])
      
      expect_false(cs_get_h5_file_path(fs1)
                            == cs_get_h5_file_path(fs2)
                            )
      
      
      orig_sn <- sampleNames(gs)
      clone_sn <- sampleNames(gs1)
      expect_identical(orig_sn, clone_sn)
      
      #check tree consistentcy
      expect_identical(getNodes(gs[[1]]), getNodes(gs1[[1]]))
      
      #check if trans is preserved
      expect_equal(getTransformations(gs[[1]]), getTransformations(gs1[[1]]))
      
      expect_equal(getPopStats(gs), getPopStats(gs1))
      expect_equal(getPopStats(gs[[1]]), getPopStats(gs1[[1]]))
      
      #clone without copying hdf data
      gs2 <- gs_copy_tree_only(gs)
      #check data consistency
      fs2 <- getData(gs2)
      expect_equal(fs1[[1]], fs2[[1]])
      
      expect_true(cs_get_h5_file_path(fs1)
                   == cs_get_h5_file_path(fs2)
                  )
      #check tree consistentcy
      expect_identical(getNodes(gs[[1]]), getNodes(gs2[[1]]))
      
      #check if trans is preserved
      expect_equal(getTransformations(gs[[1]]), getTransformations(gs2[[1]]))
      
      expect_equal(getPopStats(gs), getPopStats(gs2))
      expect_equal(getPopStats(gs[[1]]), getPopStats(gs2[[1]]))
})
test_that("rbind2",{
      orig_sn <- sampleNames(gs)
  
      gs1 <- gs_clone(gs)
  
      #construct gslist to rbind2
      sampleNames(gs1) <- "CytoTrol_CytoTrol_2.fcs"
      clone_sn <- sampleNames(gs1)
      gslist <- GatingSetList(list(gs, gs1))
      expect_is(gslist, "GatingSetList");
      suppressMessages(gs <<- rbind2(gslist))
      
      new_samples <- sampleNames(gs) 
      expect_setequal(new_samples, c(orig_sn, clone_sn))
      
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
      #ensure pdata is flushed to disk during gs_clone
      expect_identical(pData(gs_clone(gs))[, colnames(new_pd)], new_pd)
      expect_identical(pData(gs_copy_tree_only(gs)), new_pd)
      #restore pdata
      pData(gs) <- pd
      expect_identical(pData(gs), pd);
    })

test_that("[ subsetting",{
      
      gs_sub1 <- gs[1]
      expect_is(gs_sub1, "GatingSet");
      gs_sub2 <- gs["CytoTrol_CytoTrol_1.fcs"]
      expect_is(gs_sub2, "GatingSet");
      expect_equal(pData(gs_sub1), pData(gs_sub2))
      expect_equal(length(gs_sub1), 1)
      
      expect_equal(cs_get_h5_file_path(getData(gs_sub1))
                   ,cs_get_h5_file_path(getData(gs_sub2))
      )
      
      
    })

test_that("getGate for gs",{
      
      thisRes <- getGate(gs, "CD3+")
      expectRes <- readRDS(file.path(resultDir, "getGate_gs_ellipse.rds"))
      expect_equal(thisRes, expectRes, tol = 5e-04)
      
      thisRes <- getGate(gs, "singlets")
      expectRes <- readRDS(file.path(resultDir, "getGate_gs_polygon.rds"))
      expect_equal(thisRes, expectRes, tol = 2e-08)
    })


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
      
      expect_equal(as.data.table(thisRes[,1:2]), expectRes[,-1, with = F], tol = 2e-3)
      
      #use auto path
      stats_wide <- getPopStats(gs, format = "wide", path = "auto")
      stats_wide <- stats_wide[-match("root", rownames(stats_wide)), ] #remove root
      stats_wide <- as.data.frame(stats_wide)
      #get long format
      stats_long <- getPopStats(gs, format = "long", path = "auto")
      
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
      expect_setequal(rownames(thisRes),expectRes[["V1"]])#check rownames
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
            thisResult[paste0("$P",5:11, "N")] <- paste0("<", thisResult[paste0("$P",5:11, "N")], ">")
            
            thisResult[["$BEGINDATA"]] <- NULL
            thisResult[["$ENDDATA"]] <- NULL
            thisResult <- sapply(thisResult, function(i)
              {
                if(is(i, "character"))
                   i <- trimws(i)
                 i
              }, simplify = FALSE)
            thisResult
          })
      thisRes <- lapply(thisRes, function(thisResult){
            thisResult[["FILENAME"]] <- basename(thisResult[["FILENAME"]])
            thisResult[["ORIGINALGUID"]] <-thisResult[["GUID"]] <- basename(thisResult[["FILENAME"]])
            thisResult[["$BEGINDATA"]] <- NULL
            thisResult[["$ENDDATA"]] <- NULL
            ind <- !grepl("(flowCore_\\$P)|(transformation)",names(thisResult))
            thisResult[ind]
          })
      
      #reorder thisRes since now keywords were stored in hash map
      expect_setequal(names(thisRes[[1]]), names(expectRes[[1]]))
      for(i in seq_along(thisRes))
        thisRes[[i]] <- thisRes[[i]][names(expectRes[[i]])]
      
      expect_equal(thisRes,expectRes)
      
      thisRes <- keyword(gs, "$P1N")
      
      expect_equal(thisRes, data.frame(`$P1N` = c("FSC-A", "FSC-A"), check.names = FALSE))
    })

test_that("getIndices for COMPASS",{
      
      thisRes <- getIndices(gs,quote(`CD8/38- DR+|CD8/CCR7- 45RA+`)) 
      expectRes <- readRDS(file.path(resultDir, "getIndices_gs.rds"))
      tol <- ifelse(isCpStaticGate, 1e-2, 1.5e-8)
      expect_equal(sum(thisRes[[1]]), sum(expectRes[[1]]), tol = tol)
      expect_equal(sum(thisRes[[2]]), sum(expectRes[[2]]), tol = tol)
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

if(!isCpStaticGate)
{
  test_that("getSingleCellExpression for COMPASS",{
    nodes <- c('CD8/38- DR+', 'CD8/CCR7- 45RA+')
    
    thisRes <- getSingleCellExpression(gs,  nodes, map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")) 
    expectRes <- readRDS(file.path(resultDir, "getData_COMPASS_gs.rds"))
    #      browser()
    
    expect_equal(thisRes,rev(expectRes),tol = 1e-07)
    
    #test other.markers (redundant marker should be merged automatically)
    thisRes <- getSingleCellExpression(gs, nodes
                                       , map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")
                                       , other.markers = c("CD4", "CD38 APC")
                                       , threshold = FALSE
    )
    expect_equal(colnames(thisRes[[1]]), c("CD38 APC", "CCR7 PE", "CD4 PcpCy55"))
    
    #swap
    expect_warning(thisRes2 <- getSingleCellExpression(gs, nodes
                                                       , map = list("CD8/38- DR+" = "R660", "CD8/CCR7- 45RA+" = "G560")
                                                       , other.markers = c("B710", "R660")
                                                       , threshold = FALSE
                                                       , swap = T
    ), "partially matched")
    expect_equivalent(thisRes2,thisRes)
    expect_equal(colnames(thisRes2[[1]]), c("<R660-A>", "<G560-A>", "<B710-A>"))
    
    #error
    expect_error(getSingleCellExpression(gs, nodes
                                         , map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")
                                         , other.markers = c("CD4", "CD38 APC")
    ), "number of markers")
    #marginal = FALSE
    thisRes <- getSingleCellExpression(gs, nodes, marginal = FALSE)
    expect_equal(thisRes[[1]][,c("CD38 APC", "CCR7 PE")],expectRes[[1]],tol = 1e-07)
    expect_equal(thisRes[[1]][,1] == 0, thisRes[[1]][,2] == 0)
    
    #gates share the same marker
    nodes <- c('CD8/38- DR+', "CD8/38+ DR-", 'CD8/CCR7- 45RA+')
    thisRes <- getSingleCellExpression(gs, nodes, marginal = FALSE)
    #verify the results by calling R routines
    nodes.expr <- quote(`CD8/38- DR+|CD8/38+ DR-|CD8/CCR7- 45RA+`)
    ind.total <- getIndices(gs[1], nodes.expr)[[1]]
    ind.mat <- getIndiceMat(gs[[1]], nodes.expr)
    #Or the ind for the same marker from nodes
    ind.DR <- ind.38 <- ind.mat[,1] | ind.mat[,2]
    ind.CCR <- ind.45 <- ind.mat[,3]
    #masking
    mat <- exprs(getData(gs[[1]]))[,c(6, 9, 10, 11)]
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

