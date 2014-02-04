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

test_that("getNodes & setNode",{

      expectRes <- readRDS(file.path(resultDir, "getNodes_gh.rds"))

      #full path
      expect_equal(getNodes(gh), expectRes[["full_path"]])
      expect_equal(getNodes(gh, path = "full"), expectRes[["full_path"]])
      
      #terminal node
      expect_equal(getNodes(gh, path = 1, prefix = "all"), expectRes[["terminal_prefix_all"]])
      expect_equal(getNodes(gh, path = 1, prefix = "auto"), expectRes[["terminal_prefix_auto"]])
            
      expect_equal(getNodes(gh, path = 1), expectRes[["terminal"]])
      expect_equal(getNodes(gh, path = 1, prefix = "none"), expectRes[["terminal"]])
      
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
      invisible(setNode(gh, 3, "singlets"))
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

test_that(".mergeGates",{
      
      thisRes <- .mergeGates(gh, i = 6:9, bool = FALSE, merge = TRUE)
      expectRes <- list(`6` = list(popIds = 6:9
                                , parentId = 5)
                          )
      expect_equal(thisRes, expectRes)
      
      thisRes <- .mergeGates(gh, i = 5:9, bool = FALSE, merge = TRUE)
      expectRes <- c(`5` = 5, expectRes) 
      expect_equal(thisRes, expectRes)
      
      thisRes <- .mergeGates(gh, i = 6:12, bool = FALSE, merge = TRUE)
      expectRes <- list(`6` = list(popIds = 6:9
                                  , parentId = 5)
                        ,`10` = list(popIds = 10:12
                              , parentId = 5)
                          )
      
      expect_equal(thisRes, expectRes)
      
      thisRes <- .mergeGates(gh, i = 6:9, bool = FALSE, merge = FALSE)
      expectRes <- list(`6` = 6, `7` = 7, `8` = 8, `9` = 9)
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

test_that("formatAxis",{
      
      parent <- getData(gh, use.exprs = FALSE)
      thisRes <- .formatAxis(gh, parent, xParam = "SSC-A", yParam = "FSC-A")
      expectRes <- list(scales = list(), xlab = "SSC-A ", ylab = "FSC-A ")
      expect_equal(thisRes, expectRes)
      
      thisRes <- .formatAxis(gh, parent, xParam = "SSC-A", yParam = "<V450-A>")
      expectRes <- list(scales = list(y = list(at = c(227.00,  948.81, 1893.44, 2808.63, 3717.62)
                                                , labels = expression(0, 10^2, 10^3, 10^4, 10^5)
                                              )
                                      )
                        , xlab = "SSC-A ", ylab = "<V450-A> CD3 V450")
      expect_equal(thisRes, expectRes)
      
      thisRes <- .formatAxis(gh, parent, xParam = "SSC-A", yParam = "<V450-A>", marker.only = TRUE)
      expectRes[["xlab"]] <- "SSC-A"
      expectRes[["ylab"]] <- "CD3 V450"
      expect_equal(thisRes, expectRes)
            
    })

test_that("getSample",{
      
  expect_equal(getSample(gh), "CytoTrol_CytoTrol_1.fcs")
  
  expect_true(grepl("/CytoTrol_CytoTrol_1.fcs", getSample(gh, isFullPath = TRUE)))

})




test_that("getIndiceMat for COMPASS",{
      
      thisRes <- getIndiceMat(gh, "CD8/38- DR+|CD8/CCR7- 45RA+")
      expectRes <- readRDS(file.path(resultDir, "getIndiceMat_gh.rds"))
      expect_equal(thisRes,expectRes)
      
      
    })

test_that("getPopChnlMapping for COMPASS",{
      
      #no mapping provided
      thisRes <- try(.getPopChnlMapping(gh, "CD8/38- DR+|CD8/CCR7- 45RA+", list()), silent = TRUE)
      expect_is(thisRes, "try-error")
      expect_output(thisRes[[1]], "No markers in flow data matches Populations")
      
      #correct mapping provided
      thisRes <- try(.getPopChnlMapping(gh, "CD8/38- DR+|CD8/CCR7- 45RA+"
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
      
      
      #incorrect mapping
      thisRes <- try(.getPopChnlMapping(gh, "CD8/38- DR+|CD8/CCR7- 45RA+"
                                        , list("CD8/38- DR+" = "CD38", "CD8/CCR7- 45RA+" = "CCR7 PE")
                                        )
                     , silent = TRUE)
      expect_is(thisRes, "try-error")
      expect_output(thisRes[[1]], "No markers in flow data matches Populations:CD8/38- DR+")                     
      
      #correct mappping with extra items
      thisRes <- try(.getPopChnlMapping(gh, "CD8/38- DR+|CD8/CCR7- 45RA+"
                                      , list("CD8/38- DR+" = "CD38 APC"
                                            , "CD8/CCR7- 45RA+" = "CCR7 PE"
                                            , "CD3+" = "CD3 V450"
                                            )
                                       )
                      )
      expect_is(thisRes, "data.frame")
      for(i in 1:ncol(thisRes))
        thisRes[,i] <- as.factor(thisRes[, i])
      expect_identical(thisRes,expectRes)                     
      

      #mapping with incorrect extra item
      thisRes <- try(.getPopChnlMapping(gh, "CD8/38- DR+|CD8/CCR7- 45RA+"
                                    , list("CD8/38- DR+" = "CD38 APC"
                                        , "CD8/CCR7- 45RA+" = "CCR7 PE"
                                        , "CD3+" = "450"
                                    )
                                  )
                              )
      expect_is(thisRes, "data.frame")
      for(i in 1:ncol(thisRes))
        thisRes[,i] <- as.factor(thisRes[, i])
      expect_identical(thisRes,expectRes)
      
      #incorrect mappping with extra items
      thisRes <- try(.getPopChnlMapping(gh, "CD8/38- DR+|CD8/CCR7- 45RA+"
                                      , list("CD8/38- DR+" = "CD38"
                                          , "CD8/CCR7- 45RA+" = "CCR7 PE"
                                          , "CD3+" = "CD3 V450"
                                      )
                                  )
                              ,silent = TRUE)
        expect_is(thisRes, "try-error")
        expect_output(thisRes[[1]], "No markers in flow data matches Populations:CD8/38- DR+")                          
      
    })


