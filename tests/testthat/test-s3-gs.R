context("GatingSet s3")

bucket <- "mike-h5"
skip("test")
tmp <- "test"
url <- paste0("s3://", bucket, "/", tmp)
test_that("save_gs from local to remote",
{
  #load the local gs archive
  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
  cs <- gs_pop_get_data(gs)
  cf <- get_cytoframe_from_cs(cs, 1)
  cf_get_h5_file_path(cf)
  system.time(colnames(cf))
  system.time(exprs(cf[, 1:2]))
  system.time(exprs(cf))
  #save to remote 
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
            #load gs from remote without downloading h5
            gs <- load_gs(url)
            cs <- gs_pop_get_data(gs)
            cf <- get_cytoframe_from_cs(cs, 1)
            cf_get_h5_file_path(cf)
            system.time(colnames(cf))#meta is already in memory
            system.time(exprs(cf[, 1:2]))#partial IO from remote h5
            system.time(exprs(cf))#load more data from remote
          })

delete_gs(url)

# library(aws.s3)
# bucketlist(bucket = "mike-h5", region = "us-west-1")
