context("parseWorkspace")

wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)

ws <- openWorkspace(wsfile);
test_that("can load xml workspace",
{
  
  expect_that(ws, is_a("flowJoWorkspace"))
})


gs <- NULL

test_that("Can parse workspace",{
    gs <<- try(parseWorkspace(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", isNcdf = TRUE));
	expect_that(gs, is_a("GatingSet"));
	
})

source("GatingSet-testSuite.R", local = TRUE)

gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
      gh <<- gs[[1]] 
      expect_that(gh, is_a("GatingHierarchy"));  
    })


source("GatingHierarchy-testSuite.R", local = TRUE)
