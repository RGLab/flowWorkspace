context("GatingSet s3")

bucket <- "mike-h5"
prefix <- "test"
url <- paste0("s3://", bucket, "/", prefix)
reg <- "us-west-1"
test_that("save_gs from local to remote",
{
  gs_dir <- list.files(dataDir, pattern = "gs_manual",full = TRUE)
  if(backend_mode == "tile")
  {
    tmp <- tempfile()
    convert_backend(gs_dir, tmp)
  }
  #load the local gs archive
  gs <- load_gs(gs_dir)
  #save to remote 
  save_gs(gs, url)
  
  guid <- identifier(gs)
  sn <- sampleNames(gs)
  gs_key <- paste0(prefix, "/", guid, ".gs")
  gh_key <- paste0(prefix, "/", sn, ".pb")
  h5_key <- paste0(prefix, "/", sn, ".h5")
  
  keys <- get_bucket_df(url, region = reg)[["Key"]]
  expect_true(setequal(keys, c(gs_key, gh_key, h5_key)))
  })


test_that("load_gs from s3",
          {
            gs <- load_gs(url)
            expect_is(gs, "GatingSet")
            expect_true(grepl("https", cs_get_uri(gs)))
            
           })
test_that("save_gs from s3 to local",
          {
            tmp <- tempfile()
            save_gs(gs, tmp)
            guid <- identifier(gs)
            sn <- sampleNames(gs)
            gs_key <- paste0(guid, ".gs")
            gh_key <- paste0(sn, ".pb")
            h5_key <- paste0(sn, ".h5")
            expect_true(setequal(list.files(tmp), c(gs_key, gh_key, h5_key)))
            })

test_that("save_gs from s3 to s3",
          {
            
          })

delete_gs(url)

# library(aws.s3)
# bucketlist(bucket = "mike-h5", region = "us-west-1")
