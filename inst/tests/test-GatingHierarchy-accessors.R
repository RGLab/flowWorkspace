context("GatingHierarchy Accessors")

gh <- NULL

#resultDir <- "/home/wjiang2/rglab/workspace/flowWorkspace/inst/tests/expect_result"

test_that("load GatingSet from archive",
{
  gs <- load_gs(list.files(dataDir, pattern="gs_manual",full=TRUE))
  gh <<- gs[[1]]
  expect_that(gh, is_a("GatingHierarchy"))
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