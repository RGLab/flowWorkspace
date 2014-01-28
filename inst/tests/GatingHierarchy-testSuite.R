context("GatingHierarchy Accessors")

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
      expect_nodes <- scan(file = file.path(resultDir,"nodes_full_path.txt"), what = character(), sep = "\n")
      expect_equal(getNodes(gh), expect_nodes)
      expect_equal(getNodes(gh, path = "full"), expect_nodes)
      
      #terminal node
      expect_nodes <- scan(file = file.path(resultDir,"nodes_terminal_prefix_all.txt"), what = character(), sep = "\n") 
      expect_equal(getNodes(gh, path = 1, prefix = "all"), expect_nodes)
      
      expect_nodes <- scan(file = file.path(resultDir,"nodes_terminal_prefix_auto.txt"), what = character(), sep = "\n") 
      expect_equal(getNodes(gh, path = 1, prefix = "auto"), expect_nodes)
            
      expect_nodes <- scan(file = file.path(resultDir,"nodes_terminal.txt"), what = character(), sep = "\n")
      expect_equal(getNodes(gh, path = 1), expect_nodes)
      expect_equal(getNodes(gh, path = 1, prefix = "none"), expect_nodes)
      
      #fixed partial path
      expect_nodes <- scan(file = file.path(resultDir,"nodes_path_two.txt"), what = character(), sep = "\n")      
      expect_equal(getNodes(gh, path = 2), expect_nodes)
      
      expect_nodes <- scan(file = file.path(resultDir,"nodes_path_three.txt"), what = character(), sep = "\n")      
      expect_equal(getNodes(gh, path = 3), expect_nodes)
      
      #auto partial path
      expect_nodes <- scan(file = file.path(resultDir,"nodes_path_auto.txt"), what = character(), sep = "\n")      
      expect_equal(getNodes(gh, path = "auto"), expect_nodes)
      
      
    })
              
test_that("getPopStats",{
      
      
      thisRes <- getPopStats(gh)
      expect_is(thisRes, "data.table")
     
      expect_result <- fread(file.path(resultDir, "getPopStats_gh.csv"))
      expect_equal(rownames(thisRes),expect_result[["V1"]])#check rownames 
      expect_equal(thisRes[,1:5, with = F], expect_result[,2:6, with = F]) 
    })

test_that("getIndiceMat for COMPASS",{
      
      thisRes <- getIndiceMat(gh, "CD8/38- DR+|CD8/CCR7- 45RA+")
      expect_result <- readRDS(file.path(resultDir, "getIndiceMat_gh.rds"))
      expect_identical(thisRes,expect_result)
      
      
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
     
      expect_result <- data.frame(pop = c("CD8/38- DR+", "CD8/CCR7- 45RA+")
                                  , name = c("<R660-A>", "<G560-A>")
                                  , desc = c("CD38 APC", "CCR7 PE")
                                  , row.names = c("$P6", "$P10")
                              )
                                                            
      for(i in 1:ncol(thisRes))
            thisRes[,i] <- as.factor(thisRes[, i])
          
      expect_identical(thisRes,expect_result)
      
      
    })

