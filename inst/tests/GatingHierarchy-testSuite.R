context("GatingHierarchy Accessors")

test_that("getNodeInd ",{
      
      expect_error(.getNodeInd(gh, 2), "Node:2 not found!")
      
      expect_error(.getNodeInd(gh, "singlet"), "Node:singlet not found!")
      
      expect_equal(.getNodeInd(gh, "singlets"), 3)
      
      expect_equal(.getNodeInd(gh, "root"), 1)
      
      expect_equal(.getNodeInd(gh, "CD3+"), 4)
      
      expect_equal(.getNodeInd(gh, "38- DR+"), 6)
      
      expect_equal(.getNodeInd(gh, "/not debris/singlets/CD3+/CD4/38- DR+"), 6)
      
      expect_equal(.getNodeInd(gh, "CD4/38- DR+"), 6)
      
      expect_equal(.getNodeInd(gh, "/not debris/singlets/CD3+/CD8/38- DR+"), 15)
      
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
test_that("flowParamMatch ",{
      pd <- pData(parameters(getData(gh, use.exprs = FALSE)))
      #complete word fixed match 
      #by channel name
      expect_equivalent(.flowParamMatch(pd, "<B710-A>", fix = TRUE, partial = FALSE), 5)
      expect_equivalent(.flowParamMatch(pd, "<b710-a>", fix = TRUE, partial = FALSE), 5)
      expect_equivalent(.flowParamMatch(pd, "B710-A", fix = TRUE, partial = FALSE), integer(0))
      #by marker name
      expect_equivalent(.flowParamMatch(pd, "CD4 PcpCy55", fix = TRUE, partial = FALSE), integer(0))#TOFIX
      expect_equivalent(.flowParamMatch(pd, "CD4", fix = TRUE, partial = FALSE), 5)#TOFIX
      expect_equivalent(.flowParamMatch(pd, "cd4", fix = TRUE, partial = FALSE), 5)#TOFIX
      expect_equivalent(.flowParamMatch(pd, "CD4 Pcp", fix = TRUE, partial = FALSE), integer(0))
      
      #complete word regexp match 
      #by channel name
      expect_equivalent(.flowParamMatch(pd, "<B710-A>", fix = FALSE, partial = FALSE), 5)
      expect_equivalent(.flowParamMatch(pd, "<b710-a>", fix = FALSE, partial = FALSE), 5)
      expect_equivalent(.flowParamMatch(pd, "B710-A", fix = FALSE, partial = FALSE), integer(0))
      #by marker name
      expect_equivalent(.flowParamMatch(pd, "CD4 PcpCy55", fix = FALSE, partial = FALSE), 5)
      expect_equivalent(.flowParamMatch(pd, "CD4", fix = FALSE, partial = FALSE), 5)
      expect_equivalent(.flowParamMatch(pd, "cd4", fix = FALSE, partial = FALSE), 5)
      expect_equivalent(.flowParamMatch(pd, "cd", fix = FALSE, partial = FALSE), integer(0))
      
      #partial regexp match 
      #by channel name
      expect_equivalent(.flowParamMatch(pd, "<B710-A>", fix = FALSE, partial = TRUE), 5)
      expect_equivalent(.flowParamMatch(pd, "<b710-a>", fix = FALSE, partial = TRUE), 5)
      expect_equivalent(.flowParamMatch(pd, "B710-A", fix = FALSE, partial = TRUE), 5)
      expect_equivalent(.flowParamMatch(pd, "b710", fix = FALSE, partial = TRUE), 5)
      expect_equivalent(.flowParamMatch(pd, "T710", fix = FALSE, partial = TRUE), integer(0))
      #by channel name
      expect_equivalent(.flowParamMatch(pd, "CD4 PcpCy55", fix = FALSE, partial = TRUE), 5)
      expect_equivalent(.flowParamMatch(pd, "CD4", fix = FALSE, partial = TRUE), c(5,11))
      expect_equivalent(.flowParamMatch(pd, "cd4", fix = FALSE, partial = TRUE), c(5,11))
      expect_equivalent(.flowParamMatch(pd, "cd45", fix = FALSE, partial = TRUE), 11)
      expect_equivalent(.flowParamMatch(pd, "cd46", fix = FALSE, partial = TRUE), integer(0))
      
    })

test_that("getChannelMarker ",{
      fr <- getData(gh, use.exprs = FALSE)
      
      expectRes <- data.frame(name = "<B710-A>", desc = "CD4 PcpCy55", row.names = "$P5", stringsAsFactors = FALSE)
      # match by partial/full marker name     
      expect_equivalent(getChannelMarker(fr, "CD4"), expectRes)
      expect_equal(getChannelMarker(fr, "CD4 PcpCy55"), expectRes)
      expect_equal(getChannelMarker(fr, "cd4"), expectRes)
      expect_error(getChannelMarker(fr, "cd41"), "can't find cd41")
      expect_error(getChannelMarker(fr, "cd"), "multiple markers matched")
      
      # match by channel name
      expect_equal(getChannelMarker(fr, "<B710-A>"), expectRes)
      suppressWarnings(expect_equal(getChannelMarker(fr, "B710-A"), expectRes))
      expect_warning(getChannelMarker(fr, "B710-A"), "partially matched")
      
      expectRes <- data.frame(name = "<G780-A>", desc = "CD45RA PECy7", row.names = "$P11", stringsAsFactors = FALSE)
      suppressWarnings(expect_equivalent(getChannelMarker(fr, "CD45"), expectRes))
      expect_warning(getChannelMarker(fr, "CD45"), "partially matched")
      expect_equivalent(getChannelMarker(fr, "CD45RA"), expectRes)
      
            
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
      fr <- getData(gh, 0)
      expect_equal(nrow(fr), 119531)
      
      fr <- getData(gh, "CD8")
      expect_equal(nrow(fr), 14623)
      fr <- getData(gh, 14)
      expect_equal(nrow(fr), 14623)
      
      fr <- getData(gh, use.exprs = FALSE)
      expect_equal(nrow(fr), 0)
    })



test_that("sampleNames",{
      
      sn <- sampleNames(gh)
      expect_equal(sn, "CytoTrol_CytoTrol_1.fcs")
      
    })

test_that("getNodes",{

      #full path
      expect_nodes <- scan(file = file.path(resultDir,"nodes_full_path.txt"), what = character(), sep = "\n", quiet = TRUE)
      expect_equal(getNodes(gh), expect_nodes)
      expect_equal(getNodes(gh, path = "full"), expect_nodes)
      
      #terminal node
      expect_nodes <- scan(file = file.path(resultDir,"nodes_terminal_prefix_all.txt"), what = character(), sep = "\n", quiet = TRUE) 
      expect_equal(getNodes(gh, path = 1, prefix = "all"), expect_nodes)
      
      expect_nodes <- scan(file = file.path(resultDir,"nodes_terminal_prefix_auto.txt"), what = character(), sep = "\n", quiet = TRUE) 
      expect_equal(getNodes(gh, path = 1, prefix = "auto"), expect_nodes)
            
      expect_nodes <- scan(file = file.path(resultDir,"nodes_terminal.txt"), what = character(), sep = "\n", quiet = TRUE)
      expect_equal(getNodes(gh, path = 1), expect_nodes)
      expect_equal(getNodes(gh, path = 1, prefix = "none"), expect_nodes)
      
      #fixed partial path
      expect_nodes <- scan(file = file.path(resultDir,"nodes_path_two.txt"), what = character(), sep = "\n", quiet = TRUE)      
      expect_equal(getNodes(gh, path = 2), expect_nodes)
      
      expect_nodes <- scan(file = file.path(resultDir,"nodes_path_three.txt"), what = character(), sep = "\n", quiet = TRUE)      
      expect_equal(getNodes(gh, path = 3), expect_nodes)
      
      #auto partial path
      expect_nodes <- scan(file = file.path(resultDir,"nodes_path_auto.txt"), what = character(), sep = "\n", quiet = TRUE)      
      expect_equal(getNodes(gh, path = "auto"), expect_nodes)
      
      #full path (bfs)
      expect_nodes <- scan(file = file.path(resultDir,"nodes_full_path_bfs.txt"), what = character(), sep = "\n", quiet = TRUE)
      expect_equal(getNodes(gh, order = "bfs"), expect_nodes)
      expect_nodes <- scan(file = file.path(resultDir,"nodes_full_path_tsort.txt"), what = character(), sep = "\n", quiet = TRUE)
      expect_equal(getNodes(gh, order = "tsort"), expect_nodes)
      
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
      
      nodelist$v <-integer()
      .getAllDescendants(gh, "CD3+", nodelist)
      thisRes <- nodelist$v
      expectRes <- readRDS(file.path(resultDir, "getAllDescendants_cd3_gh.rds"))
      expect_equal(thisRes, expectRes)
      
      nodelist$v <-integer()
      .getAllDescendants(gh, "CD4", nodelist)
      thisRes <- nodelist$v
      expectRes <- readRDS(file.path(resultDir, "getAllDescendants_cd4_gh.rds"))
      expect_equal(thisRes, expectRes)
      
      nodelist$v <-integer()
      .getAllDescendants(gh, "CD8", nodelist)
      thisRes <- nodelist$v
      expectRes <- readRDS(file.path(resultDir, "getAllDescendants_cd8_gh.rds"))
      expect_equal(thisRes, expectRes)
      
    })

test_that("show ",{
      expect_output(show(gh), "Sample:  CytoTrol_CytoTrol_1.fcs GatingHierarchy with  24  gates")
      
    })

test_that("keyword",{
      thisRes <- keyword(gh)
      expectRes <- readRDS(file.path(resultDir, "kw_gh.rds"))
      expect_equal(thisRes, expectRes)
      
      expect_equal(keyword(gh, 'P11DISPLAY'), "LOG")
      
      expect_equal(keyword(gh, '$P8N'), "V450-A")
    })

test_that("getParent",{
      
      
      expect_equal(getParent(gh, "singlets"), "/not debris")
      
      expect_equal(getParent(gh, "not debris/singlets"), "/not debris")
      
      expect_equal(getParent(gh, 3), 2)
      
      expect_error(getParent(gh, "singlet"), "Node:singlet not found!")
      
      expect_error(getParent(gh, "root"), "0 :parent not found!")
      
    })

test_that("getChildren",{
      
      
      expect_equal(getChildren(gh, "not debris/singlets"), "/not debris/singlets/CD3+")
      
      expect_equal(getChildren(gh, "singlets"), "/not debris/singlets/CD3+")
      
      expect_equal(getChildren(gh, 3), 4)
      
      expect_error(getChildren(gh, "singlet"), "Node:singlet not found!")
      
      expect_equal(getChildren(gh, "38- DR+"), character(0))
      
      expect_equal(getChildren(gh, "CD3+"), c("/not debris/singlets/CD3+/CD4"
                                              , "/not debris/singlets/CD3+/CD8"
                                              , "/not debris/singlets/CD3+/DNT"
                                              , "/not debris/singlets/CD3+/DPT")
                                      )
      
    })

test_that(".getPopStat",{
      
      expect_error(.getPopStat(gh, "singlets"), "y has to be numeric!")
      
      expect_error(.getPopStat(gh, 30), "invalid vertexID")
      
      expect_equal(.getPopStat(gh, 3), list(flowCore = c(proportion = 9.487789e-01, count = 8.702200e+04)
                                            , flowJo = c(proportion = 9.488988e-01, count = 8.703300e+04)
                                            )
                      , tol = 1e-7 )
      
      
      
      expect_equal(.getPopStat(gh, 1), list(flowCore = c(proportion = 1, count = 119531)
                                            , flowJo = c(proportion = 1, count = 119531)
                                        )
                                )
    })      


test_that("getProp",{
      
      expect_equal(getProp(gh, "singlets"), 0.9487789)
      
      expect_equal(getProp(gh, "singlets", flowJo = FALSE), 0.9487789)
      
      expect_equal(getProp(gh, "singlets", flowJo = TRUE), 0.9488988, tol = 1e-7)
      
      expect_error(getProp(gh, "singlet"), "Node:singlet not found!")
    })      

test_that("getTotal",{
      
      expect_equal(getTotal(gh, "singlets"), 87022)
      
      expect_equal(getTotal(gh, "singlets", flowJo = FALSE), 87022)
      
      expect_equal(getTotal(gh, "singlets", flowJo = TRUE), 87033)
      
      expect_error(getTotal(gh, "singlet"), "Node:singlet not found!")
    })      


test_that("getPopStats",{
      
      
      thisRes <- getPopStats(gh)
      expect_is(thisRes, "data.table")
     
      expectRes <- fread(file.path(resultDir, "getPopStats_gh.csv"))
      expect_equal(rownames(thisRes),expectRes[["V1"]])#check rownames 
      expect_equal(thisRes[,1:5, with = F], expectRes[,2:6, with = F]) 
    })

test_that("compute CV from gh",{
      
      thisRes <- .computeCV_gh(gh)
      expect_is(thisRes, "matrix")
      
      expectRes <- readRDS(file.path(resultDir, "cv_gh.rds"))
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


test_that("getIndiceMat for COMPASS",{
      
      thisRes <- getIndiceMat(gh, "CD8/38- DR+|CD8/CCR7- 45RA+")
      expectRes <- readRDS(file.path(resultDir, "getIndiceMat_gh.rds"))
      expect_equal(thisRes,expectRes)
      
      
    })

test_that("getPopChnlMapping for COMPASS",{
      
      thisRes <- try(flowWorkspace:::.getPopChnlMapping(gh, "CD8/38- DR+|CD8/CCR7- 45RA+", list()), silent = TRUE)
      expect_is(thisRes, "try-error")
      expect_output(thisRes[[1]], "No markers in flow data matches Populations")
      
      thisRes <- try(flowWorkspace:::.getPopChnlMapping(gh, "CD8/38- DR+|CD8/CCR7- 45RA+"
                                        , list("CD8/38- DR+" = "CD38 APC", "CD8/CCR7- 45RA+" = "CCR7 PE")
                                        )
                     )
      expect_is(thisRes, "data.frame")
     
      expectRes <- data.frame(pop = c("CD8/38- DR+", "CD8/CCR7- 45RA+")
                                  , name = c("<R660-A>", "<G560-A>")
                                  , desc = c("CD38 APC", "CCR7 PE")
                                  , row.names = c("$P6", "$P10")
                              )
                                                            
      for(i in 1:ncol(thisRes))
            thisRes[,i] <- as.factor(thisRes[, i])
          
      expect_identical(thisRes,expectRes)
      
      
    })

