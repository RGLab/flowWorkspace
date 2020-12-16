#only run s3 test on my machine for now 
#until figure out how to create bucket on the fly using put_bucket
skip_if_not(dir.exists("/home/wjiang2"))

bucket <- "mike-h5"
prefix <- flowCore:::guid()
url <- paste0("s3://", bucket, "/", prefix)
reg <- "us-west-1"
test_that("save_gs/load",
{
  cat("save_gs from local to remote \n")
  gs_dir <- list.files(dataDir, pattern = "gs_manual",full = TRUE)
  if(get_default_backend() == "tile")#currently we don't support h5 backend for s3
  {
    tmp <- tempfile()
    convert_backend(gs_dir, tmp)
    
    if(use_on_disk_idx())
    {
      gs <- load_gs(tmp)
      gs_convert_idx_to_ondisk(gs)
      expect_equal(basename(gh_idx_get_uri(gs[[1]])), paste0(sampleNames(gs[[1]]), ".idx"))
      tmp1 <- tempfile()
      save_gs(gs, tmp1)
      unlink(tmp, recursive = TRUE)
      tmp <- tmp1
    }
    gs_dir <- tmp
  }
  #load the local gs archive
  gs <- load_gs(gs_dir)
  #save to remote 
  delete_gs(url)#strange that url already exists in devtools::test() environment(but not separately run)
  save_gs(gs, url)
  
  guid <- identifier(gs)
  sn <- sampleNames(gs)
  gs_key <- paste0(prefix, "/", guid, ".gs")
  gh_key <- paste0(prefix, "/", sn, ".pb")
  cf_key <- paste0(prefix, "/", sn, ".", get_default_backend())
  idx_key <- paste0(prefix, "/", sn, ".idx")
  
  keys <- get_bucket_df(url, prefix = prefix, region = reg)[["Key"]]
  expect_true(is.element(c(gs_key), keys))
  expect_true(is.element(c(gh_key), keys))
  if(use_on_disk_idx())
  {
    expect_equal(sum(grepl(idx_key, keys)), 3)
  }
  expect_equal(sum(grepl(cf_key, keys)), 21)
  
  if(get_default_backend() == "tile")
    delete_gs(tmp)
  
  cat("load_gs from s3 \n")
  gs <- load_gs(url)
  expect_is(gs, "GatingSet")
  expect_true(is_s3_path(cs_get_uri(gs)))
  if(use_on_disk_idx())
    expect_true(is_s3_path(gh_idx_get_uri(gs[[1]])))

  cat("save_gs from s3 to local \n")
  tmp <- tempfile()
  save_gs(gs, tmp)
  guid <- identifier(gs)
  sn <- sampleNames(gs)
  gs_key <- paste0(guid, ".gs")
  gh_key <- paste0(sn, ".pb")
  idx_key <- paste0(sn, ".idx")
  cf_key <- paste0(sn, ".", get_default_backend())
  
  res <- c(gs_key, gh_key, cf_key)
  if(use_on_disk_idx())
    res <- c(res, idx_key)
  expect_true(setequal(list.files(tmp), res))
  delete_gs(tmp)

  cat("save_gs from s3 to s3\n")
  gs <- load_gs(url)
  url1 <- paste0("s3://", bucket, "/", flowCore:::guid())
  save_gs(gs, url1)
  delete_gs(url1)

  delete_gs(url)

})
