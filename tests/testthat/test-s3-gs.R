context("GatingSet s3")

bucket <- "mike-h5"
prefix <- "test"
url <- paste0("s3://", bucket, "/", prefix)
reg <- "us-west-1"
test_that("save_gs from local to remote",
{
  #load the local gs archive
  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
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
            #load gs from remote without downloading h5
            gs <- load_gs(url)
            expect_is(gs, "GatingSet")
            expect_true(grepl("https", cs_get_h5_file_path(gs)))
           })
test_that("save_gs from s3 to local",
          {
            tmp <- tempfile()
            save_gs(gs, tmp)
          })

test_that("save_gs from s3 to s3",
          {
            
          })

delete_gs(url)

# library(aws.s3)
# bucketlist(bucket = "mike-h5", region = "us-west-1")
