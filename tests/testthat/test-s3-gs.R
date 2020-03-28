context("GatingSet s3")

bucket <- "mike-h5"
skip("test")
tmp <- "test"
url <- paste0("s3://", bucket, "/", tmp)
test_that("save_gs from local to remote",
{
  suppressWarnings(suppressMessages(gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))          
  
  save_gs(gs, url)
  })

test_that("save_gs from s3 to local",
          {
            
          })

test_that("save_gs from s3 to s3",
          {
            
          })

test_that("load_gs from s3",
          {
            
            gs <- load_gs(url)
          })

delete_gs(url)

library(aws.s3)
# bucketlist(bucket = "mike-h5")
