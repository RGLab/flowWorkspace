context("GatingSet archive")

gs <- NULL


test_that("load GatingSet from archive",
{
  suppressWarnings(suppressMessages(gs <<- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))
  #update the gating results
  # since the gating results stored in achive was not accurate due to the ellipse gates
  suppressMessages(recompute(gs))
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
      
      # only return the transfromation associated with given channel
      tran <- getTransformations(gh, channel = "<B710-A>")
      expect_is(tran, "function")
      
      expect_error(tran <- getTransformations(gh, channel = "<"), "multiple tranformation")
      
      tran <- getTransformations(gh, channel = "<B710-AA>")
      expect_null(tran)
      
      trans <- getTransformations(gh)
      expect_is(trans, "list")
      
      trans <- trans[[1]]
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

test_that("formatAxis",{
      gh <- gs[[1]]
      parent <- getData(gh, use.exprs = FALSE)
      thisRes <- flowWorkspace:::.formatAxis(gh, parent, xParam = "SSC-A", yParam = "FSC-A")
      expectRes <- list(scales = list())
      expect_equal(thisRes, expectRes)
      
      thisRes <- flowWorkspace:::.formatAxis(gh, parent, xParam = "SSC-A", yParam = "<V450-A>")
      expectRes <- list(scales = list(y = list(at = c(227.00,  948.81, 1893.44, 2808.63, 3717.62)
                  , labels = expression(0, 10^2, 10^3, 10^4, 10^5)
              )
          ))
      expect_equal(thisRes, expectRes)
      
      thisRes <- flowWorkspace:::.formatAxis(gh, parent, xParam = "SSC-A", yParam = "<V450-A>", marker.only = TRUE)
      expect_equal(thisRes, expectRes)
      
      #DISABLE RAW SCALE
      thisRes <- flowWorkspace:::.formatAxis(gh, parent, xParam = "SSC-A", yParam = "<V450-A>", raw.scale = FALSE)
      expectRes <- list(scales = list())
      expect_equal(thisRes, expectRes)      
      
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
      suppressWarnings(suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))
      gh <- gs[[1]] 
      suppressMessages(gs <<- GatingSet(gh, sampleNames(gh), path = dataDir, isNcdf = TRUE))
      expect_that(gs, is_a("GatingSet"))
    })

source("GatingSet-testSuite.R", local = TRUE)

gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
      gh <<- gs[[1]] 
      expect_that(gh, is_a("GatingHierarchy"));  
    })


source("GatingHierarchy-testSuite.R", local = TRUE)
