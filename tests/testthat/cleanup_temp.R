context("cs cleanup")
skip_if(win32_flag)
data("GvHD")
fs <- fsApply(GvHD[1:6], function(fr)fr[1:2, ])
gs_untouched <- GatingSet(fs)
files_untouched <- list.files(cs_get_uri(gs_cyto_data(gs_untouched)), full.names = TRUE)

gs <-GatingSet(fs)
h5_path <- cs_get_uri(gs_cyto_data(gs))

test_that("cf_cleanup_temp", {
	files_pre <- list.files(h5_path)
	# Just a few checks to make sure pathing didn't get screwed up
	expect_equal(length(files_pre), length(sampleNames(gs)))
	expect_equal(length(files_untouched), length(sampleNames(gs)))
	
	target <- sampleNames(gs)[[6]]
	target_fn <- paste0(target, ".", get_default_backend())
	cf_cleanup_temp(gs_cyto_data(gs)[[target, returnType = "cytoframe"]])
	files_post <- list.files(h5_path)
	expect_equal(files_post, files_pre[!(files_pre == target_fn)])
})

test_that("cs_cleanup_temp", {
	expect_true(dir.exists(h5_path))
	cs_cleanup_temp(gs_cyto_data(gs))
	expect_false(dir.exists(h5_path))
})

gs <-GatingSet(fs)
h5_path <- cs_get_uri(gs_cyto_data(gs))

test_that("gh_cleanup_temp", {
	gs <-GatingSet(fs)
	h5_path <- cs_get_uri(gs_cyto_data(gs))
	files_pre <- list.files(h5_path)
	expect_equal(length(files_pre), length(sampleNames(gs)))
	target <- sampleNames(gs)[[4]]
	target_fn <- paste0(target, ".", get_default_backend())
	gh_cleanup_temp(gs[[target]])
	files_post <- list.files(h5_path)
	expect_equal(files_post, files_pre[!(files_pre == target_fn)])
})

test_that("cs_cleanup_temp", {
	expect_true(dir.exists(h5_path))
	gs_cleanup_temp(gs)
	expect_false(dir.exists(h5_path))
	expect_equal(files_untouched, list.files(cs_get_uri(gs_cyto_data(gs_untouched)), full.names = TRUE))
})
