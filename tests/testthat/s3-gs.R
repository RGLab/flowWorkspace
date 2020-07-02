context("GatingSet s3")

#only run s3 test on my machine for now 
#until figure out how to create bucket on the fly using put_bucket
skip_if_not(dir.exists("/home/wjiang2"))

bucket <- "mike-h5"
prefix <- flowCore:::guid()
url <- paste0("s3://", bucket, "/", prefix)
reg <- "us-west-1"
test_that("save_gs from local to remote",
{
  gs_dir <- list.files(dataDir, pattern = "gs_manual",full = TRUE)
  if(get_default_backend() == "tile")#currently we don't support h5 backend for s3
  {
    tmp <- tempfile()
    convert_backend(gs_dir, tmp)
    gs_dir <- tmp
  }
  #load the local gs archive
  gs <- load_gs(gs_dir)
  #save to remote 
  save_gs(gs, url)
  
  guid <- identifier(gs)
  sn <- sampleNames(gs)
  gs_key <- paste0(prefix, "/", guid, ".gs")
  gh_key <- paste0(prefix, "/", sn, ".pb")
  cf_key <- paste0(prefix, "/", sn, ".", get_default_backend())
  
  keys <- get_bucket_df(url, region = reg)[["Key"]]
  expect_true(is.element(c(gs_key), keys))
  expect_true(is.element(c(gh_key), keys))
  expect_equal(sum(grepl(cf_key, keys)), 21)
  
  if(get_default_backend() == "tile")
    delete_gs(tmp)
  
  })


test_that("load_gs from s3",
          {
            gs <- load_gs(url)
            expect_is(gs, "GatingSet")
            expect_true(is_s3_path(cs_get_uri(gs)))
            
           })
test_that("save_gs from s3 to local",
          {
            tmp <- tempfile()
            save_gs(gs, tmp)
            guid <- identifier(gs)
            sn <- sampleNames(gs)
            gs_key <- paste0(guid, ".gs")
            gh_key <- paste0(sn, ".pb")
            cf_key <- paste0(sn, ".", get_default_backend())
            expect_true(setequal(list.files(tmp), c(gs_key, gh_key, cf_key)))
            delete_gs(tmp)
            })

test_that("save_gs from s3 to s3",
          {
            gs <- load_gs(url)
            url1 <- paste0("s3://", bucket, "/", flowCore:::guid())
            save_gs(gs, url1)
            delete_gs(url1)
          })

delete_gs(url)

# library(aws.s3)
# bucketlist(bucket = "mike-h5", region = "us-west-1")
