context("legacy_gs")
skip_if(win32_flag)

test_that(".load_legacy v1", {
  legacy <- file.path(dataDir,"/legacy_gs/v1/gs_manual")
  tmp <- tempfile()
  gs <- .load_legacy(legacy, tmp)
  #ensure the range slot is synced to keyword
  expect_equal(round(range(gh_pop_get_data(gs[[1]]))[, 5:10])
              , structure(list("<B710-A>" = c(426, 4097), "<R660-A>" = c(519, 4096), "<R780-A>" = c(877, 4093)
                                     , "<V450-A>" = c(-904, 4098), "<V545-A>" = c(213, 4097), "<G560-A>" = c(110, 4097))
                                , row.names = c("min", "max"), class = "data.frame")
              ,tol = 2.5e-4)
  })
test_that("convert_legacy_gs v1", {
  legacy <- file.path(dataDir,"/legacy_gs/v1/gs_bcell_auto")
  expect_error(load_gs(legacy), "convert_legacy_gs")
  tmp <- tempfile()
  expect_message(convert_legacy_gs(legacy, tmp), "saved")
  gs <- load_gs(tmp)
  expect_is(gs, "GatingSet")
  
  #use customized folder to hold temp fcs files generated during the converting
  tmpfcs <- tempfile()
  tmp <- tempfile()
  expect_message(convert_legacy_gs(legacy, tmp, tmp = tmpfcs), "saved")
  list.files(tmpfcs)
  #output/legacy_gs_sn_without_fcs_ext
  legacy <- "~/rglab/workspace/flowWorkspace/output/legacy_gs_sn_without_fcs_ext"
  skip_if_not(dir.exists(legacy))
  tmp <- tempfile()
  expect_message(convert_legacy_gs(legacy, tmp), "saved")
  gs1 <- load_gs(tmp)
  expect_equal(sampleNames(gs1), sub(".fcs", "", sampleNames(gs)))  
  })

test_that("load legacy_gs v2", {
  legacy <- file.path(dataDir,"/legacy_gs/v2/gs_bcell_auto")
  gs <- load_gs(legacy)
  expect_is(gs, "GatingSet")
  
  tmp <- tempfile()
  expect_message(save_gs(gs, tmp), "Done")
  expect_equal(length(list.files(tmp, ".h5")), 2)
  expect_equal(length(list.files(tmp, ".pb")), 2)
  expect_equal(length(list.files(tmp, ".gs")), 1)
})
test_that("convert_legacy_gslist v1", {
  legacy <- "~/remote/fh/fast/gottardo_r/mike_working/lyoplate_out/gated_data/legacy/manual/gslist-tcell"
  skip_if_not(dir.exists(legacy))
  expect_error(load_gslist(legacy), "convert_legacy_gslist")
  tmp <- tempfile()
  expect_message(convert_legacy_gslist(legacy, tmp), "saved")
  expect_error(convert_legacy_gslist(legacy, tmp), "existing")
  gslist <- load_gslist(tmp)
  expect_is(gslist, "GatingSetList")
  expect_equal(length(gslist), 63)
  expect_equal(length(gslist@data), 7)
})
