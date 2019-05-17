context("GatingSet archive")

gs <- NULL
isCpStaticGate <<- TRUE
test_that("load GatingSet from archive",
{
  suppressWarnings(suppressMessages(gs <<- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))
  expect_that(gs, is_a("GatingSet"))
  gs <<- gs_clone(gs)#make it writable
})

test_that("save GatingSet to archive",
    {
      pd <- pData(gs)
      pd[["newCol"]] <- "A"
      pData(gs) <- pd
      id <- identifier(gs)
      expect_is(id, "character")
      tmp <- tempfile()
      save_gs(gs, path = tmp)
      
      gs <- load_gs(tmp)
      expect_that(gs, is_a("GatingSet"))
      expect_setequal(colnames(pData(gs)), colnames(pd))
      expect_message(save_gs(gs, path = tmp), "Done")
      expect_error(save_gs(gs[1], path = tmp), "doesn't match")

      cdf <- list.files(tmp, ".h5", full.names = TRUE)
      expect_equal(identifier(gs), id)
      id.new <- "test"
      identifier(gs) <- id.new
      expect_equal(identifier(gs), id.new)
      #restore id
      identifier(gs) <- id
      file.copy(cdf, file.path(tmp, "redundant.nc"))
      expect_error(save_gs(gs, path = tmp), "Not a valid")
      
      expect_error(colnames(gs_cyto_data(gs))[1] <- "dd" , "read-only")
      expect_error(exprs(get_cytoframe_from_cs(gs_cyto_data(gs), 1))[1,1] <- 0, "read-only")
    
    })
## it is placed here because trans may get cleared later on by cloning process
test_that("gh_get_transformations",{    
      
      
      
      flowDataPath <- system.file("extdata", package = "flowWorkspaceData")
      fcsFiles <- list.files(pattern = "CytoTrol", flowDataPath, full = TRUE)
      fr  <- read.FCS(fcsFiles[[1]])
      
      chnl <- "V450-A"
      raw <- exprs(fr)[,chnl]
      gh <- gs[[1]]
      
      #test gh_get_transformations
      
      # only return the transfromation associated with given channel
      tran <- gh_get_transformations(gh, channel = "<B710-A>")
      expect_is(tran, "function")
      
      expect_null(gh_get_transformations(gh, channel = "<"))
      
      tran <- gh_get_transformations(gh, channel = "<B710-AA>")
      expect_null(tran)
      
      trans <- gh_get_transformations(gh)
      expect_is(trans, "list")
      
      trans <- trans[[1]]
      inverseTrans <- gh_get_transformations(gh, inverse = TRUE)[[1]]
      
      transformed <- trans(raw)
      raw1 <- inverseTrans(transformed)
      expect_equal(raw, raw1, tolerance = 2e-3)
      
      #test flowJoTrans
      trans <- flowjo_biexp()
      inverseTrans <- flowjo_biexp(inverse = TRUE)
      
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
    expect_true(!identical(get_gatingset_id(gh2@pointer), get_gatingset_id(gh@pointer)))
    expect_true(!identical(gh@pointer, gh2@pointer))
    expect_equal(getData(gh), getData(gh2))
    
})


source("GatingHierarchy-testSuite.R", local = TRUE)


##TODO: has some issue with the latest change from #203, breaks the test on gs_get_singlecell_expression call
test_that("Construct new GatingSet based on the existing gating hierarchy",
   {
     #re-load the gs since the trans get lost during clone
     suppressWarnings(suppressMessages(gs1 <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))
     gh <- gs1[[1]]
     suppressMessages(gs <<- GatingSet(gh, sampleNames(gh), path = dataDir))
     expect_that(gs, is_a("GatingSet"))
     expect_equal(getNodes(gs), getNodes(gs1))
   })
isCpStaticGate <<- TRUE
source("GatingSet-testSuite.R", local = TRUE)

gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
     gh <<- gs[[1]]
     expect_that(gh, is_a("GatingHierarchy"));
   })


source("GatingHierarchy-testSuite.R", local = TRUE)
