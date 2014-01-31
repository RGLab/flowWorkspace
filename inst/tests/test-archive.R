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
    gh2 <- gs[["CytoTrol_CytoTrol_1.fcs"]]
    expect_is(gh, "GatingHierarchy")
    expect_false(identical(gh2@guid, gh@guid))
    expect_true(identical(gh@pointer, gh2@pointer))
    expect_equal(gh@data, gh2@data)
    expect_equal(gh@axis, gh2@axis)
    expect_equal(gh@flag, gh2@flag)
    
})


source("GatingHierarchy-testSuite.R", local = TRUE)


###TODO: trans somehow did not get copied over ,
###cpp code needs to be fixed before adding this testsuite 
#test_that("Construct new GatingSet based on the existing gating hierarchy",
#    {
#      gs <<- GatingSet(gh, sampleNames(gh), path = dataDir, isNcdf = TRUE)
#      expect_that(gs, is_a("GatingSet"))
#    })
#
#source("GatingSet-testSuite.R", local = TRUE)
#
#gh <- NULL
#test_that("extract GatingHierarchy from GatingSet",{
#      gh <<- gs[[1]] 
#      expect_that(gh, is_a("GatingHierarchy"));  
#    })
#
#
#source("GatingHierarchy-testSuite.R", local = TRUE)
