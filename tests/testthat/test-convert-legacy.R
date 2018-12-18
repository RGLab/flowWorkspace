context("legacy_gs")

test_that(".load_legacy", {
  legacy <- file.path(dataDir,"/legacy_gs/gs_manual")
  tmp <- tempfile()
  gs <- .load_legacy(legacy, tmp)
  #ensure the range slot is synced to keyword
  expect_equal(round(range(getData(gs[[1]]))[, 5:10])
              , structure(list("<B710-A>" = c(426, 4097), "<R660-A>" = c(519, 4096), "<R780-A>" = c(877, 4093)
                                     , "<V450-A>" = c(-904, 4098), "<V545-A>" = c(213, 4097), "<G560-A>" = c(110, 4097))
                                , row.names = c("min", "max"), class = "data.frame")
              )
  })
test_that("convert_gs_legacy", {
  legacy <- file.path(dataDir,"/legacy_gs/gs_bcell_auto")
  expect_error(load_gs(legacy), "convert_gs_legacy")
  tmp <- tempfile()
  expect_message(convert_gs_legacy(legacy, tmp), "saved")
  gs <- load_gs(tmp)
  expect_is(gs, "GatingSet")
  })
