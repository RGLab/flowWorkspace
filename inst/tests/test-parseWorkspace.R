context("parseWorkspace")

wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)

ws <- openWorkspace(wsfile);
test_that("can load xml workspace",
{
  
  expect_that(ws, is_a("flowJoWorkspace"))
})

source("flowJoWorkspace-testSuite.R", local = TRUE)


gs <- NULL

test_that("Can parse workspace",{
    suppressWarnings(gs <<- try(parseWorkspace(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", isNcdf = TRUE)))
	expect_that(gs, is_a("GatingSet"));
	
})


gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
      gh <<- gs[[1]]
      expect_that(gh, is_a("GatingHierarchy"));  
    })


test_that("parse without gating",{
      
      suppressWarnings(gs1 <- try(parseWorkspace(ws, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", execute = FALSE)))
      expect_that(gs1, is_a("GatingSet"));
      gh1 <- gs1[[1]]
      
      thisStats <- getPopStats(gh1)[, c(2,4,5), with = FALSE]
      expectStats <- getPopStats(gh)[, c(2,4,5), with = FALSE]
      expect_equal(thisStats, expectStats)
      
      #exclude the gates that require extension since the extend_to are different 
      # based on whether data is loaded
      nodes <- getNodes(gh)[ -c(6:13, 15:22)]
      thisGates <- sapply(nodes, getGate, obj = gh1)
      expectGates <- sapply(nodes, getGate, obj = gh)
      expect_equal(thisGates, expectGates)
      
      
    })

# make sure this test is invoked before GatingSet-testSuite since the trans is gonna be lost
# during clone and rbind2 test
test_that("getTransformations ",{
      
      thisRes <- getTransformations(gh)
      expectRes <- readRDS(file.path(resultDir, "getTransformations_gh.rds"))
      expect_equal(thisRes,expectRes)
      
    })
source("GatingHierarchy-testSuite.R", local = TRUE)
source("GatingSet-testSuite.R", local = TRUE)



test_that("closeWorkspace",
    {
      closeWorkspace(ws)
      thisRes <- paste(capture.output(show(ws))[-2], collapse = "")
      expectRes <- paste(fjRes[["ws_show_close"]][-2], collapse = "")
      expect_output(thisRes, expectRes)
      
    })


