context("GatingHierarchy Accessors")

test_that(".isCompensated",
    {
      expect_true(.isCompensated(gh))
    })

test_that("getNodeInd ",{
      
      #invalid numeric indexing
      expect_error(.getNodeInd(gh, 2), "expecting a string")
      
      #invalid node name
      expect_error(.getNodeInd(gh, "singlet"), "singlet not found")
      
      #valid unique node name
      expect_equal(.getNodeInd(gh, "singlets"), 3)
      
      expect_equal(.getNodeInd(gh, "root"), 1)
      
      expect_equal(.getNodeInd(gh, "CD3+"), 4)
      
      #full path indexing
      expect_equal(.getNodeInd(gh, "/not debris/singlets/CD3+/CD4/38- DR+"), 6)
      expect_equal(.getNodeInd(gh, "/not debris/singlets/CD3+/CD8/38- DR+"), 15)
      
      #partial path 
      expect_equal(.getNodeInd(gh, "CD4/38- DR+"), 6)
      expect_equal(.getNodeInd(gh, "CD8/38- DR+"), 15)
      
      #non-unqiue partial path
      expect_error(.getNodeInd(gh, "/38- DR+"), "not found")
      
      #non-unique node name indexing
      expect_error(.getNodeInd(gh, "38- DR+"), "is ambiguous within the gating tree")
      
      #dealing with root
      expect_equal(.getNodeInd(gh, "/not debris"), 2)
      expect_equal(.getNodeInd(gh, "not debris"), 2)
      expect_equal(.getNodeInd(gh, "/root/not debris"), 2)
      expect_equal(.getNodeInd(gh, "root/not debris"), 2)
      
    })


test_that("getIndices ",{
      
      thisRes <- getIndices(gh, "singlets")
      expectRes <- readRDS(file.path(resultDir, "getIndice_singlet_gh.rds"))
      expect_equal(thisRes,expectRes)
      
    })

test_that("getAxisLabels ",{
      
      thisRes <- getAxisLabels(gh)
      expectRes <- readRDS(file.path(resultDir, "getAxisLabels_gh.rds"))
      expect_equal(thisRes,expectRes)
      
    })

test_that("getCompensationMatrices ",{
      
      thisRes <- getCompensationMatrices(gh)
      expectRes <- readRDS(file.path(resultDir, "getCompensationMatrices_gh.rds"))
      expect_equal(thisRes,expectRes)
      
    })

test_that(".isBoolGate ",{
      expect_false(.isBoolGate(gh, "singlets"))
      
      bf <- booleanFilter("CD4/38- DR+|CD4/CCR7- 45RA+", filterId = "myBoolFilter")
      suppressWarnings(id <- add(gh, bf))
      expect_true(.isBoolGate(gh, "myBoolFilter"))
      invisible(Rm("myBoolFilter", gh))
    })


test_that("getData ",{
      
      fr <- getData(gh)
      expect_is(fr, "flowFrame");
      expect_equal(nrow(fr), 119531)
      
      fr <- getData(gh, "CD8")
      expect_equal(nrow(fr), 14570)
      
      fr <- getData(gh, use.exprs = FALSE)
      expect_equal(nrow(fr), 0)
    })



test_that("sampleNames",{
      
      sn <- sampleNames(gh)
      expect_equal(sn, "CytoTrol_CytoTrol_1.fcs")
      
    })

test_that("getNodes & setNode",{

      expectRes <- readRDS(file.path(resultDir, "getNodes_gh.rds"))

      #full path
      expect_equal(getNodes(gh), expectRes[["full_path"]])
      expect_equal(getNodes(gh, path = "full"), expectRes[["full_path"]])
      
            
      expect_equal(getNodes(gh, path = 1), expectRes[["terminal"]])
      
      #fixed partial path
      expect_equal(getNodes(gh, path = 2), expectRes[["path_two"]])
      expect_equal(getNodes(gh, path = 3), expectRes[["path_three"]])
      
      #auto partial path
      expect_equal(getNodes(gh, path = "auto"), expectRes[["path_auto"]])
      
      #full path (bfs)
      expect_equal(getNodes(gh, order = "bfs"), expectRes[["full_path_bfs"]])
      expect_equal(getNodes(gh, order = "tsort"), expectRes[["full_path_tsort"]])
      
      #change node name
      invisible(setNode(gh, "singlets", "S"))
      expect_equal(getNodes(gh), gsub("singlets", "S", expectRes[["full_path"]]))
      invisible(setNode(gh, "S", "singlets"))
      expect_equal(getNodes(gh), expectRes[["full_path"]])
      
      #hide node(terminal)
      invisible(setNode(gh, "DNT", FALSE))
      expect_equal(getNodes(gh), expectRes[["full_path"]][-23])
      invisible(setNode(gh, "DNT", TRUE))
      expect_equal(getNodes(gh), expectRes[["full_path"]])
      
      #hide node(non-terminal)
      invisible(setNode(gh, "singlets", FALSE))
      expect_equal(getNodes(gh), expectRes[["full_path"]][-3])
      invisible(setNode(gh, "singlets", TRUE))
      expect_equal(getNodes(gh), expectRes[["full_path"]])
    })

test_that("setGate", {
      
      gate_cd4 <- getGate(gh, "CD4")
      gate_cd8 <- getGate(gh, "CD8")
      invisible(setGate(gh, "CD4", gate_cd8))
      expect_equal(getGate(gh, "CD4")@cov, gate_cd8@cov)
      suppressMessages(recompute(gh, "CD4"))
      expect_equal(getTotal(gh, "CD4"), getTotal(gh, "CD8"))
      
      #restore the gate
      invisible(setGate(gh, "CD4", gate_cd4))
      suppressMessages(recompute(gh, "CD4"))
    })    

test_that(".getGraph",{
      
      #extract graph from gh
      thisRes <- .getGraph(gh)
      expectRes <- readRDS(file.path(resultDir, "getGraph_gh.rds"))
      expect_equal(thisRes,expectRes)
      
      #annotate the graph
      thisRes <- .layoutGraph(expectRes)
      expectRes <- readRDS(file.path(resultDir, "layoutGraph_gh.rds"))
      expect_equal(thisRes,expectRes)
    })   

test_that(".getAllDescendants",{
      nodelist <- new.env(parent=emptyenv())
      allNodes <- getNodes(gh, showHidden = TRUE)
      nodelist$v <-integer()
      .getAllDescendants(gh, "CD3+", nodelist)
      thisRes <- nodelist$v
      expectRes <- readRDS(file.path(resultDir, "getAllDescendants_cd3_gh.rds"))
      expect_equal(allNodes[thisRes], expectRes)
      
      nodelist$v <-integer()
      .getAllDescendants(gh, "CD4", nodelist)
      thisRes <- nodelist$v
      expectRes <- readRDS(file.path(resultDir, "getAllDescendants_cd4_gh.rds"))
      expect_equal(allNodes[thisRes], expectRes)
      
      nodelist$v <-integer()
      .getAllDescendants(gh, "CD8", nodelist)
      thisRes <- nodelist$v
      expectRes <- readRDS(file.path(resultDir, "getAllDescendants_cd8_gh.rds"))
      expect_equal(allNodes[thisRes], expectRes)
      
    })

test_that("show ",{
      
      thisRes <- paste(capture.output(show(gh)), collapse = "")
      expectRes <- "Sample:  CytoTrol_CytoTrol_1.fcs GatingHierarchy with  24  gates"
      expect_output(thisRes, expectRes)
            
    })

test_that("keyword",{
      thisRes <- keyword(gh)
      expectRes <- readRDS(file.path(resultDir, "kw_gh.rds"))
      kw_fn <- "flowWorkspaceData/extdata/CytoTrol_CytoTrol_1.fcs"
      expect_output(thisRes$FILENAME, kw_fn)
      expect_output(expectRes$FILENAME, kw_fn)
      expectRes$FILENAME <- NULL
      thisRes$FILENAME <- NULL
      expect_equal(thisRes, expectRes)
      
      expect_equal(keyword(gh, 'P11DISPLAY'), "LOG")
      
      expect_equal(keyword(gh, '$P8N'), "V450-A")
    })

test_that("getParent",{
      
      
      expect_equal(getParent(gh, "singlets"), "/not debris")
      
      expect_equal(getParent(gh, "not debris/singlets"), "/not debris")
      
      expect_error(getParent(gh, "singlet"), "singlet not found")
      
      expect_error(getParent(gh, "root"), "0 :parent not found!")
      
    })

test_that("getChildren",{
      
      
      expect_equal(getChildren(gh, "not debris/singlets"), "/not debris/singlets/CD3+")
      
      expect_equal(getChildren(gh, "singlets"), "/not debris/singlets/CD3+")
      
      
      expect_error(getChildren(gh, "singlet"), "singlet not found")
      
      expect_error(getChildren(gh, "38- DR+"), "ambiguous")
      
      expect_equal(getChildren(gh, "CD4/38- DR+"), character(0))
      
      expect_equal(getChildren(gh, "CD3+"), c("/not debris/singlets/CD3+/CD4"
                                              , "/not debris/singlets/CD3+/CD8"
                                              , "/not debris/singlets/CD3+/DNT"
                                              , "/not debris/singlets/CD3+/DPT")
                                      )
      
    })

test_that(".getPopStat",{
      
      expect_error(.getPopStat(gh, 3), "Error : expecting a string")
      
      expect_error(.getPopStat(gh, "singlet"), "not found")
      
      expect_equal(.getPopStat(gh, "singlets"), list(flowCore = c(proportion = 9.487789e-01, count = 8.702200e+04)
                                            , flowJo = c(proportion = 9.488988e-01, count = 8.703300e+04)
                                            )
                      , tol = 1e-7 )
      
      
      
      expect_equal(.getPopStat(gh, "root"), list(flowCore = c(proportion = 1, count = 119531)
                                            , flowJo = c(proportion = 1, count = 119531)
                                        )
                                )
    })      


test_that("getProp",{
      
      expect_equal(getProp(gh, "singlets"), 0.9487789)
      
      expect_equal(getProp(gh, "singlets", flowJo = FALSE), 0.9487789)
      
      expect_equal(getProp(gh, "singlets", flowJo = TRUE), 0.9488988, tol = 1e-7)
      
      expect_error(getProp(gh, "singlet"), "singlet not found")
    })      

test_that("getTotal",{
      
      expect_equal(getTotal(gh, "singlets"), 87022)
      
      expect_equal(getTotal(gh, "singlets", flowJo = FALSE), 87022)
      
      expect_equal(getTotal(gh, "singlets", flowJo = TRUE), 87033)
      
      expect_error(getTotal(gh, "singlet"), "singlet not found")
    })      


test_that("getPopStats",{
      
      
      thisRes <- getPopStats(gh, path = "full")
      expect_is(thisRes, "data.table")
     
      expectRes <- fread(file.path(resultDir, "getPopStats_gh.csv"))
      expectRes[, node := V1]
      expectRes[, V1 := NULL]
      expect_equal(rownames(thisRes),expectRes[["node"]])#check rownames 
      expect_equal(thisRes[, 1:4, with = F], expectRes[, colnames(thisRes)[1:4], with = F]) 
    })

test_that("compute CV from gh",{
      
      thisRes <- .computeCV_gh(gh)
      expect_is(thisRes, "matrix")
      
      expectRes <- readRDS(file.path(resultDir, "cv_gh.rds"))
      rownames(thisRes) <- basename(rownames(thisRes))
      expect_equal(thisRes, expectRes)
      
    })

test_that("getGate",{
      
      thisRes <- getGate(gh, "singlets")
      expect_is(thisRes, "polygonGate")
      
      #add a rectangleGate to test 
      rg <- rectangleGate(filterId="myRectGate", list("FSC-A"=c(200, 600),"SSC-A"=c(0, 400)))      
      id <- add(gh, rg)
      thisRes <- getGate(gh, "myRectGate")
      expect_is(thisRes, "rectangleGate")
      expect_equal(thisRes@min, rg@min)
      expect_equal(thisRes@max, rg@max)
      expect_equivalent(parameters(thisRes), parameters(rg))
      invisible(Rm("myRectGate", gh))
      
      #add a bool gate to test
      bf <- booleanFilter("CD4/38- DR+|CD4/CCR7- 45RA+", filterId = "myBoolFilter")
      suppressWarnings(id <- add(gh, bf))
      thisRes <- getGate(gh, "myBoolFilter")
      expect_is(thisRes, "booleanFilter")
      
      expect_equal(gsub("[`\"]", "", as.character(thisRes@expr)), as.character(bf@expr))
      invisible(Rm("myBoolFilter", gh))
     
      ##TODO: test rangeGate (no R API to add it, have to parse it from xml )
      
    })

test_that(".mergeGates",{
      nodes <- getNodes(gh, path = "auto")
      #with customized x,y (incorrect prj)
      projections <- list("singlets" = c("FSC-H", "FSC-A")
                        , "CD3+" = c("CD3", "FSC-A")
                        , "CD4" = c("CD4", "CD3")
                        , "CD4/38- DR-" = c("SSC-A", "<V545-A>")
                    )
                          
      expect_error(.mergeGates(gh, i = nodes[6:9], bool = FALSE, merge = TRUE, projections = projections)
                  , "Given projection")

      #swapped x,y (we don't really allow to change the dimensions for 2D gate yet, only the order can be changed)
      projections[["CD4/38- DR-"]] <- c("<R660-A>", "<V545-A>")                     
      thisRes <- .mergeGates(gh, i = nodes[6:9], bool = FALSE, merge = TRUE, projections = projections)
      expectRes <- list(`CD4/38- DR+` = list(popIds = nodes[6:9]
                                  , parentId = nodes[5])
                          )      
      expect_equal(thisRes, expectRes)
      
      
      #merge 4 quadrants
      thisRes <- .mergeGates(gh, i = nodes[6:9], bool = FALSE, merge = TRUE)
      
      expect_equal(thisRes, expectRes)
      
            
      # 4 quadrants + 1 pop
      thisRes <- .mergeGates(gh, i = nodes[5:9], bool = FALSE, merge = TRUE)
      expectRes <- c(`CD4` = "CD4", expectRes) 
      expect_equal(thisRes, expectRes)
      
      thisRes <- .mergeGates(gh, i = nodes[6:12], bool = FALSE, merge = TRUE)
      expectRes <- list(`CD4/38- DR+` = list(popIds = nodes[6:9]
                                  , parentId = "CD4")
                        ,`CD4/CCR7- 45RA+` = list(popIds = nodes[10:12]
                              , parentId = "CD4")
                          )
      
      expect_equal(thisRes, expectRes)
      
      #4 quadrants without merge
      thisRes <- .mergeGates(gh, i = nodes[6:9], bool = FALSE, merge = FALSE)
      expectRes <- list(`CD4/38- DR+` = "CD4/38- DR+"
                        , `CD4/38+ DR+` = "CD4/38+ DR+"
                        , `CD4/38+ DR-` = "CD4/38+ DR-"
                        , `CD4/38- DR-` = "CD4/38- DR-")
      expect_equal(thisRes, expectRes)
      
    })

test_that("pretty10exp",{
      
      thisRes <- pretty10exp(c(1, 10, 100, 1000), drop.1 = TRUE)
      expectRes <- expression(10^0, 10^1, 10^2, 10^3)
      expect_equal(thisRes, expectRes)
      
      thisRes <- pretty10exp(c(1, 10, 100, 1000), drop.1 = FALSE)
      expectRes <- expression(1 %*% 10^0, 1 %*% 10^1, 1 %*% 10^2, 1 %*% 10^3)
      expect_equal(thisRes, expectRes)      
    })



test_that("getIndiceMat for COMPASS",{
      
      thisRes <- getIndiceMat(gh, "CD8/38- DR+|CD8/CCR7- 45RA+")
      expectRes <- readRDS(file.path(resultDir, "getIndiceMat_gh.rds"))
      expect_equal(thisRes,expectRes)
      
      
    })

test_that("getPopChnlMapping",{
      
      fr <- getData(gh, use.exprs = FALSE) 
      this_pd <- pData(parameters(fr))
      this_pd
      #'make up ICS markers
      this_pd[8:12, "desc"] <- c("IL2", "IL4", "IL22", "TNFa", "CD154")
      
      #'no mapping provided 
      
      #we don't parse + signs since they could be part of pop names
      nodes <- c("CD4/IL2+","CD4/IL22+")
      expect_error(.getPopChnlMapping(this_pd, nodes), "Marker not found: IL2+")
      
      #remove + from pop names
      nodes <- c("CD4/IL2","CD4/IL22")
      expectRes <- data.frame(pop = nodes
          , name = c("<V450-A>", "<G560-A>")
          , desc = c("IL2", "IL22")
          , stringsAsFactors = F
      )
      expect_equivalent(.getPopChnlMapping(this_pd, nodes), expectRes)
      
      #partial match for IL22
      nodes <- c("CD4/IL2","CD4/22")
      expectRes[["pop"]] <- nodes 
      expect_equivalent(.getPopChnlMapping(this_pd, nodes), expectRes)
      
      #'mapping provided
      
      # but without the proper names
      nodes <- c("CD4/IL2+","CD4/IL22+")
      mapping <- list("IL2", "IL22")
      expect_error(.getPopChnlMapping(this_pd, nodes, mapping), "Marker not found: IL2+")
      # add names
      names(mapping) <- nodes
      expectRes[["pop"]] <- nodes
      expect_equivalent(.getPopChnlMapping(this_pd, nodes, mapping), expectRes)
      
      
      #incorrect mapping
      mapping <- list("IL2", "IL3")
      names(mapping) <- nodes
      expect_error(.getPopChnlMapping(this_pd, nodes, mapping), "Marker not found: IL3")
      
      #ambiguous mapping
      mapping <- list("IL2", "2")
      names(mapping) <- nodes
      expect_error(.getPopChnlMapping(this_pd, nodes, mapping), "2  paritally matched to multiple markers but failed to exactly matched to any of them")
      
      
      #correct mappping with extra items (ignored during the matching)
      mapping <- list("IL2", "IL22")
      names(mapping) <- nodes
      mapping[["extra"]] <- "marker1"
      expect_equivalent(.getPopChnlMapping(this_pd, nodes, mapping), expectRes)                     
      
      #correct partial mapping
      mapping <- list("IL2", "154")
      names(mapping) <- nodes                          
      expectRes[2,"name"] <- "Time"
      expectRes[2,"desc"] <- "CD154"
      expect_equivalent(.getPopChnlMapping(this_pd, nodes, mapping), expectRes)
      
      #case sensitive at the moment
      mapping <- list("IL2", "cd154")
      names(mapping) <- nodes                          
      expect_error(.getPopChnlMapping(this_pd, nodes, mapping), "Marker not found: cd154")
      
      
    })


