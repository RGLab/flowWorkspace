context("GatingSet s3")

bucket <- "mike-h5"
skip("test")
test_that("save_gs from local to remote",
{
  suppressWarnings(suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))          
  tmp <- "test"
  save_gs(gs, paste0("s3://", bucket, "/", tmp))
  })

test_that("save_gs from s3 to local",
          {
            
          })

test_that("save_gs from s3 to s3",
          {
            
          })


bucketlist(bucket = "mike-h5")
