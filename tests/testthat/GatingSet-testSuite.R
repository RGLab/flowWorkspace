context("GatingSet Accessors")
test_that("<<- ",{
  gs1 <- gs_clone(gs)
  gh <- gs1[[1]]
  # expect_error(gs[[1]] <- gh)
  gs1[[1]] <- gh
  markernames(gs1[[1]]) <- c("<B710-A>" = "cd4")
  expect_equivalent(markernames(gs1)[1], "cd4");
  
})
test_that("gs_is_h5 ",{
      
        
	expect_equal(gs_is_h5(gs), TRUE);
    
})
test_that("length ",{
      expect_equal(length(gs), 1)
      
    })
test_that("show ",{
      expect_output(show(gs), "A GatingSet with 1 samples")
      
    })
test_that("gs_pop_get_data ",{
      
      ncfs <- gs_pop_get_data(gs)
      expect_is(ncfs, "cytoset");
      expect_equal(nrow(ncfs[[1]]), 119531)
      ncfs <- gs_pop_get_data(gs, "root")
      expect_equal(nrow(ncfs[[1]]), 119531)
      
      ncfs <- gs_pop_get_data(gs, "singlets")
      expect_equal(nrow(ncfs[[1]]), 87022)
      expect_is(gs[[1]], "GatingHierarchy");
      
      
    })


test_that("gs_cyto_data ",{
      
      ncfs <- gs_cyto_data(gs)
      expect_is(ncfs, "cytoset");
      expect_equal(nrow(ncfs[[1]]), 119531)
      
     
      #restore data
      gs_cyto_data(gs) <- ncfs
      expect_is(gs_cyto_data(gs), "cytoset");
    })


test_that("gs_clone",{
      
      gs1 <- gs_clone(gs)
      expect_is(gs1, "GatingSet");
      
      #check data consistency
      fs1 <- gs_pop_get_data(gs)
      fs2 <- gs_pop_get_data(gs1)
      expect_equal(fs1[[1]], fs2[[1]])
      
      expect_false(cs_get_h5_file_path(fs1)
                            == cs_get_h5_file_path(fs2)
                            )
      
      
      orig_sn <- sampleNames(gs)
      clone_sn <- sampleNames(gs1)
      expect_identical(orig_sn, clone_sn)
      
      #check tree consistentcy
      expect_identical(gs_get_pop_paths(gs[[1]]), gs_get_pop_paths(gs1[[1]]))
      
      #check if trans is preserved
      expect_equal(gh_get_transformations(gs[[1]]), gh_get_transformations(gs1[[1]]))
      
      expect_equal(gs_pop_get_count_fast(gs), gs_pop_get_count_fast(gs1))
      expect_equal(gh_pop_compare_stats(gs[[1]]), gh_pop_compare_stats(gs1[[1]]))
      
      #clone without copying hdf data
      gs2 <- gs_copy_tree_only(gs)
      #check data consistency
      fs2 <- gs_pop_get_data(gs2)
      expect_equal(fs1[[1]], fs2[[1]])
      
      expect_true(cs_get_h5_file_path(fs1)
                   == cs_get_h5_file_path(fs2)
                  )
      #check tree consistentcy
      expect_identical(gs_get_pop_paths(gs[[1]]), gs_get_pop_paths(gs2[[1]]))
      
      #check if trans is preserved
      expect_equal(gh_get_transformations(gs[[1]]), gh_get_transformations(gs2[[1]]))
      
      expect_equal(gs_pop_get_count_fast(gs), gs_pop_get_count_fast(gs2))
      expect_equal(gh_pop_compare_stats(gs[[1]]), gh_pop_compare_stats(gs2[[1]]))
})

test_that("merge_list_to_gs", {
    sn1 <- sampleNames(gs)
    suppressMessages(gs_clone <- gs_clone(gs))
    #duplicated sample names
    expect_error(merge_list_to_gs(list(gs, gs_clone)), "There are overlapping samples across GatingSets")
    sn2 <- "CytoTrol_CytoTrol_2.fcs"
    sampleNames(gs_clone) <- sn2
    
    #different colnames
    chnnls <- colnames(gs_cyto_data(gs_clone))
    chnnls[1] <- "FSC-H" 
    expect_error(colnames(gs_cyto_data(gs_clone)) <- chnnls, "duplicates", class = "std::domain_error")
    chnnls[1] <- "F" 
    colnames(gs_cyto_data(gs_clone)) <- chnnls
    expect_error(merge_list_to_gs(list(gs, gs_clone)), "colnames of cyto data don't match")
    
    #different tree structure
    invisible(gs_pop_remove("CD8", gs = gs_clone))
    expect_error(merge_list_to_gs(list(gs, gs_clone)), "gating structure doesn't match: CytoTrol_CytoTrol_2.fcs CytoTrol_CytoTrol_1.fcs")
    
    #succeed
    suppressMessages(gs_clone <- gs_clone(gs))
    sampleNames(gs_clone) <- sn2
    gs <<- merge_list_to_gs(list(gs, gs_clone))
    
    expect_is(gs, "GatingSet")
    
    samp <- c(sn1, sn2)
    expect_equal(sampleNames(gs), samp)
    
    #share the same h5 with original gs
    cs <- gs_pop_get_data(gs)
    cf <- get_cytoframe_from_cs(cs, sn2)
    expect_equal(cf_get_h5_file_path(cf), cf_get_h5_file_path(get_cytoframe_from_cs(gs_cyto_data(gs_clone), sn2)))
    expect_equal(nrow(cs[[sn2]]),  119531)
    cs <- gs_pop_get_data(gs, "CD8")
    expect_equal(nrow(cs[[sn2]]),  14564)
    
    #keyword
    thisRes <- keyword(gs)
    expect_equal(thisRes[[1]], keyword(gs_clone)[[1]])
    
    
    expect_true(setequal(markernames(gs), sort(markernames(gs_clone))))
    
    chnls <- c('FSC-A','FSC-H','FSC-W','SSC-A','<B710-A>','<R660-A>','<R780-A>','<V450-A>','<V545-A>','<G560-A>','<G780-A>','Time')
    expect_equal(colnames(gs), chnls)
    
    gs2 <- gs_clone(gs)
    sampleNames(gs2) <- c("A", "B")
    gs3 <- merge_list_to_gs(list(gs, gs2))
    
    #create discrepancy
    chnls <- c("<B710-A>", "<R780-A>")
    markers <- c("CD4", "CD8")
    names(markers) <- chnls
    markernames(gs2) <- markers
    expect_warning(res <- markernames(gs3), "marker names are not consistent across")
    #preserved the original sample order
    expect_equal(sampleNames(gs3), c(sampleNames(gs), sampleNames(gs2)))

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
      expect_identical(pData(gs)[, colnames(new_pd)], new_pd);
      #ensure pdata is flushed to disk during gs_clone
      expect_identical(pData(gs_clone(gs))[, colnames(new_pd)], new_pd)
      expect_identical(pData(gs_copy_tree_only(gs))[, colnames(new_pd)], new_pd)
      #restore pdata
      pData(gs) <- pd
      expect_identical(pData(gs)[, colnames(pd), drop = FALSE], pd);
    })

test_that("[ subsetting",{
      
      gs_sub1 <- gs[1]
      expect_is(gs_sub1, "GatingSet");
      gs_sub2 <- gs["CytoTrol_CytoTrol_1.fcs"]
      expect_is(gs_sub2, "GatingSet");
      expect_equal(pData(gs_sub1), pData(gs_sub2))
      expect_equal(length(gs_sub1), 1)
      
      expect_equal(cs_get_h5_file_path(gs_pop_get_data(gs_sub1))
                   ,cs_get_h5_file_path(gs_pop_get_data(gs_sub2))
      )
      
      
    })

test_that("subset.GatingSet",{
  # GvHD has more samples for this test
  gs_full <- GatingSet(GvHD)
  gs_sub <- subset(gs_full, Visit == 2)
  expect_equal(length(gs_sub), 5)
})

test_that("Adding subpopulation off empty population", {
	gs_scratch <- GatingSet(GvHD)
	empty_gate <- rectangleGate(filterId="emptyGate", list("FL1-H"=c(2500, 5000), "FL2-H"=c(5000, 7500)))
	gs_pop_add(gs_scratch, empty_gate, parent= "root")
	extra_gate <- rectangleGate(filterId="extraGate", list("FL3-H"=c(0, 300), "FL2-A"=c(0, 250)))
	gs_pop_add(gs_scratch, extra_gate, parent="emptyGate")
	recompute(gs_scratch)
	expectRes <- readRDS(file.path(resultDir, "empty_pop.rds"))
	expect_equal(gs_pop_get_stats(gs_scratch), expectRes)
})

test_that("gs_pop_get_gate for gs",{
      
      thisRes <- gs_pop_get_gate(gs, "CD3+")
      expectRes <- readRDS(file.path(resultDir, "getGate_gs_ellipse.rds"))
      expect_equal(thisRes, expectRes, tol = 5e-04)
      
      thisRes <- gs_pop_get_gate(gs, "singlets")
      expectRes <- readRDS(file.path(resultDir, "getGate_gs_polygon.rds"))
      expect_equal(thisRes, expectRes, tol = 2e-08)
    })


test_that("gs_pop_set_name",{
    
    nodeName <- gs_get_pop_paths(gs[[1]])[3]
	gs_pop_set_name(gs, "singlets", "S")
    lapply(gs, function(gh){
          expect_equal(gs_get_pop_paths(gh)[3], "/not debris/S")
        }) 
	gs_pop_set_name(gs, "S", "singlets")
    invisible()
    
    
  })

test_that("gs_pop_get_count_fast",{
  
      thisRes <- gs_pop_get_count_fast(gs, , statistic = "freq", path = "full", format = "wide")
      expect_is(thisRes, "matrix")
      
      expectRes <- fread(file.path(resultDir, "getPopStats_gs.csv"))
      expect_equal(rownames(thisRes),expectRes[["V1"]])#check rownames
      
      expect_equal(as.data.table(thisRes[,1:2]), expectRes[,-1, with = F], tol = 2e-3)
      
      #use auto path
      stats_wide <- gs_pop_get_count_fast(gs, format = "wide", path = "auto")
      stats_wide <- stats_wide[-match("root", rownames(stats_wide)), ] #remove root
      stats_wide <- as.data.frame(stats_wide)
      #get long format
      stats_long <- gs_pop_get_count_fast(gs, format = "long", path = "auto")
      
      #convert it to wide to do the comparsion
      stats_long[, value := Count]
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
            thisResult[["$CYTOLIB_VERSION"]] <- NULL
            colnames(thisResult[["SPILL"]]) <- gsub("<|>", "", colnames(thisResult[["SPILL"]]))
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
            cd4_gate  <- gh_pop_get_gate(gh, "CD4")
            cd4_gate@filterId <- "CD4_test"
            cd8_gate  <- gh_pop_get_gate(gh, "CD8")
            cd8_gate@filterId <- "CD8_test"
            filters(list(cd4_gate,cd8_gate))
          })
      
      #add without name
      Ids <- gs_pop_add(gs, filterslist1)
      expect_equal(Ids, c(25,26))
      cd4_gate  <- gs_pop_get_gate(gs, "CD4")
      cd4_gate <- lapply(cd4_gate, function(g){
            g@filterId <- "CD4_test"
            g
          })
      expect_equal(gs_pop_get_gate(gs, "CD4_test"), cd4_gate) 
      gs_pop_remove("CD4_test", gs = gs)
      gs_pop_remove("CD8_test", gs = gs)
      
      #customize names
      expect_error(gs_pop_add(gs, filterslist1, names = c("CD4_demo")), "number of population names ")
      expect_error(gs_pop_add(gs, filterslist1, names = c("CD4_demo", "CD4_demo")), "not unqiue")
      
      Ids <- gs_pop_add(gs, filterslist1, names = c("CD4_demo", "CD8_demo"))
      expect_equal(Ids, c(25,26))
      cd4_gate  <- gs_pop_get_gate(gs, "CD4")
      cd4_gate <- lapply(cd4_gate, function(g){
            g@filterId <- "CD4_demo"
            g
          })
      expect_equal(gs_pop_get_gate(gs, "CD4_demo"), cd4_gate) 
      gs_pop_remove("CD4_demo", gs = gs)
      gs_pop_remove("CD8_demo", gs = gs)
    })

if(!isCpStaticGate)
{
  test_that("gs_get_singlecell_expression for COMPASS",{
    nodes <- c('CD8/38- DR+', 'CD8/CCR7- 45RA+')
    
    thisRes <- gs_get_singlecell_expression(gs,  nodes, map = list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")) 
    expectRes <- readRDS(file.path(resultDir, "getData_COMPASS_gs.rds"))
    #      browser()
    
    expect_equal(thisRes,rev(expectRes),tol = 1e-07)
    
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
    ind.mat <- gh_pop_get_indices_mat(gs[[1]], nodes.expr)
    #Or the ind for the same marker from nodes
    ind.DR <- ind.38 <- ind.mat[,1] | ind.mat[,2]
    ind.CCR <- ind.45 <- ind.mat[,3]
    #masking
    mat <- exprs(gh_pop_get_data(gs[[1]]))[,c(6, 9, 10, 11)]
    mat <- mat * c(ind.38, ind.DR, ind.CCR, ind.45)
    mat <- mat[ind.total, ]
    expect_equal(thisRes[[1]], mat, check.attributes = FALSE)
    
    #gs_get_singlecell_expression_by_gate associated with channel that does not have markers
    mat <- gs_get_singlecell_expression_by_gate(gs, "CD3+")
    expect_equal(colnames(mat[[1]]), c("CD3 V450", "SSC-A"))
    expect_equal(nrow(mat[[1]]), gh_pop_get_count(gs[[1]], "CD3+"))
  })
  
}


test_that("markernames", {
  markers <- c('CD4 PcpCy55','CD38 APC','CD8 APCH7','CD3 V450','HLA-DR V500','CCR7 PE','CD45RA PECy7')
  expect_equivalent(markernames(gs), markers)
  
  markers.new <- c("CD4", "CD8")
  chnls <- c("<B710-A>", "<R780-A>")
  names(markers.new) <- chnls
  gh <- gs[[1]]
  markernames(gh) <- markers.new
  expect_equivalent(markernames(gh)[c(1,3)], markers.new)
  
  expect_warning(res <- markernames(gs), "not consistent")
  expect_equivalent(unique(lapply(gs, markernames)), res)

  
  markernames(gs) <-  markers.new
  expect_equivalent(markernames(gs)[c(1,3)], markers.new)
  
  #restore original markers
  markers.orig <- markers[c(1,3)]
  names(markers.orig) <- chnls
  markernames(gs) <-  markers.orig
  expect_equivalent(markernames(gs), markers)
  
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

test_that("node path accessors for GatingSet", {
  expect_equal(gs_get_pop_paths(gs)[c(4,7,9)], c("/not debris/singlets/CD3+",
                                                 "/not debris/singlets/CD3+/CD4/38+ DR+",
                                                 "/not debris/singlets/CD3+/CD4/38- DR-"))
  expect_equal(gs_pop_get_parent(gs, "DNT"), "/not debris/singlets/CD3+")
  expect_equal(gs_pop_get_children(gs, "CD3+"), c("/not debris/singlets/CD3+/CD4", 
                                                  "/not debris/singlets/CD3+/CD8", 
                                                  "/not debris/singlets/CD3+/DNT", 
                                                  "/not debris/singlets/CD3+/DPT"))
  
})
