context("GatingSet archive")

gs <- NULL


test_that("load GatingSet from archive",
{
  gs <<- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  expect_that(gs, is_a("GatingSet"))
})

source("GatingSet-testSuite.R", local = TRUE)

gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
    gh <<- gs[[1]] 
    expect_that(gh, is_a("GatingHierarchy"));  
})


source("GatingHierarchy-testSuite.R", local = TRUE)