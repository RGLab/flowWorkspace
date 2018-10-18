context("GatingSet archive")

gs <- NULL
isCpStaticGate <<- TRUE
test_that("load GatingSet from archive",
{
  suppressWarnings(suppressMessages(gs <<- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))
  expect_that(gs, is_a("GatingSet"))
})

test_that("save GatingSet to archive",
    {
      tmp <- tempfile()
      save_gs(gs, path = tmp)
      
      gs <<- load_gs(tmp)
      expect_that(gs, is_a("GatingSet"))
      
      expect_message(save_gs(gs, path = tmp), "Done")
      expect_error(save_gs(gs[1], path = tmp), "doesn't match")

      cdf <- list.files(tmp, ".h5", full.names = TRUE)
      file.copy(cdf, file.path(tmp, "redundant.nc"))
      expect_error(save_gs(gs, path = tmp), "Not a valid")
    
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
      
      # only return the transfromation associated with given channel
      tran <- getTransformations(gh, channel = "<B710-A>")
      expect_is(tran, "function")
      
      expect_null(getTransformations(gh, channel = "<"))
      
      tran <- getTransformations(gh, channel = "<B710-AA>")
      expect_null(tran)
      
      trans <- getTransformations(gh)
      expect_is(trans, "list")
      
      trans <- trans[[1]]
      inverseTrans <- getTransformations(gh, inverse = TRUE)[[1]]
      
      transformed <- trans(raw)
      raw1 <- inverseTrans(transformed)
      expect_equal(raw, raw1, tolerance = 2e-3)
      
      #test flowJoTrans
      trans <- flowJoTrans()
      inverseTrans <- flowJoTrans(inverse = TRUE)
      
      transformed <- trans(raw)
      raw1 <- inverseTrans(transformed)
      expect_equal(raw, raw1, tolerance = 2e-3)
      
      
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


##TODO: has some issue with the latest change from #203, breaks the test on getSingleCellExpression call
test_that("Construct new GatingSet based on the existing gating hierarchy",
   {
     #re-load the gs since the trans get lost during clone
     suppressWarnings(suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))
     gh <- gs[[1]]
     suppressMessages(gs <<- GatingSet(gh, sampleNames(gh), path = dataDir, isNcdf = TRUE))
     expect_that(gs, is_a("GatingSet"))
   })
isCpStaticGate <<- TRUE
source("GatingSet-testSuite.R", local = TRUE)

gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
     gh <<- gs[[1]]
     expect_that(gh, is_a("GatingHierarchy"));
   })


source("GatingHierarchy-testSuite.R", local = TRUE)
