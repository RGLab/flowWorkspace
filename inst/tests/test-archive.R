context("GatingSet archive")

gs <- NULL


test_that("load GatingSet from archive",
{
  gs <<- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  #update the gating results
  # since the gating results stored in achive was not accurate due to the ellipse gates
  recompute(gs)
  expect_that(gs, is_a("GatingSet"))
})

## it is placed here because trans may get cleared later on by cloning process
test_that("getTransformations",{    
      
      
      
      flowDataPath <- system.file("extdata", package = "flowWorkspaceData")
      fcsFiles <- list.files(pattern = "CytoTrol", flowDataPath, full = TRUE)
      fr  <- read.FCS(fcsFiles[[1]])
      
      chnl <- "V450-A"
      raw <- exprs(fr)[,chnl]
      gh <- gs[[1]]
      #test getTransformations
      trans <- getTransformations(gh)[[1]]
      inverseTrans <- getTransformations(gh, inverse = TRUE)[[1]]
      
      transformed <- trans(raw)
      raw1 <- inverseTrans(transformed)
      all.equal(raw, raw1, tolerance = 2e-3)
      
      #test flowJoTrans
      trans <- flowJoTrans()
      inverseTrans <- flowJoTrans(inverse = TRUE)
      
      transformed <- trans(raw)
      raw1 <- inverseTrans(transformed)
      all.equal(raw, raw1, tolerance = 2e-3)
      
      
    })


source("GatingSetList-testSuite.R", local = TRUE)

source("GatingSet-testSuite.R", local = TRUE)



gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
    gh <<- gs[[1]]
    gh2 <- gs[["CytoTrol_CytoTrol_1.fcs"]]
    expect_is(gh, "GatingHierarchy")
    expect_true(identical(gh2@guid, gh@guid))
    expect_true(identical(gh@pointer, gh2@pointer))
    expect_equal(gh@data, gh2@data)
    expect_equal(gh@axis, gh2@axis)
    expect_equal(gh@flag, gh2@flag)
    
})


source("GatingHierarchy-testSuite.R", local = TRUE)



test_that("Construct new GatingSet based on the existing gating hierarchy",
    {
      #re-load the gs since the trans get lost during clone 
      gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
      gh <- gs[[1]] 
      gs <<- GatingSet(gh, sampleNames(gh), path = dataDir, isNcdf = TRUE)
      expect_that(gs, is_a("GatingSet"))
    })

source("GatingSet-testSuite.R", local = TRUE)

gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
      gh <<- gs[[1]] 
      expect_that(gh, is_a("GatingHierarchy"));  
    })


source("GatingHierarchy-testSuite.R", local = TRUE)
