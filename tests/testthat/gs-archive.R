context("-- archived gs")

gs <- NULL
isCpStaticGate <<- TRUE
test_that("load GatingSet from archive",
{
  gs_dir <- list.files(dataDir, pattern = "gs_manual",full = TRUE)
  suppressWarnings(suppressMessages(gs <<- load_gs(gs_dir)))
  expect_that(gs, is_a("GatingSet"))
  gs <<- gs_clone(gs)#make it writable  
  
  if(get_default_backend() == "tile")
  {
    tmp1 <- tempfile()
    convert_backend(gs_dir, tmp1)
    gs <<- load_gs(tmp1)
    if(use_on_disk_idx())
    {
      gs_convert_idx_to_ondisk(gs)
      expect_equal(basename(gh_idx_get_uri(gs[[1]])), paste0(sampleNames(gs[[1]]), ".idx"))
    }
  }
  
  
})

test_that("load archive with specified format",
{
  fmt_orig <- get_default_backend()
  skip_if(fmt_orig=="mem")
  tmp <- tempfile()
  save_gs(gs, tmp)
  #default load cf as it is
  gs1 <- load_gs(tmp)
  expect_equal(cf_backend_type(gs_get_cytoframe(gs1, 1)), fmt_orig)
  #load it as on disk
  gs1 <- load_gs(tmp, load_format = "on-disk")
  expect_equal(cf_backend_type(gs_get_cytoframe(gs1, 1)), fmt_orig)
  #load it as in-mem
  gs1 <- load_gs(tmp, load_format = "in-memory")
  expect_equal(cf_backend_type(gs_get_cytoframe(gs1, 1)), "mem")
  expect_equal(gh_idx_get_uri(gs1[[1]]), "")
  #save it back
  save_gs(gs1, tmp, backend_opt = "skip")
  #now default is in-mem
  gs1 <- load_gs(tmp)
  expect_equal(cf_backend_type(gs_get_cytoframe(gs1, 1)), "mem")
  if(use_on_disk_idx())
    expect_false(gh_idx_get_uri(gs1[[1]]) == "")#not sure if we want to erase the original on-disk idx instead
  
  #load it as on-disk again
  gs1 <- load_gs(tmp, load_format = "on-disk")
  expect_equal(cf_backend_type(gs_get_cytoframe(gs1, 1)), fmt_orig)
  #save it back
  save_gs(gs1, tmp, backend_opt = "skip")
  #now default is on-disk
  gs1 <- load_gs(tmp)
  expect_equal(cf_backend_type(gs_get_cytoframe(gs1, 1)), fmt_orig)
  
})
test_that("load read-only archive",
{
  skip_on_os("windows")
  tmp <- tempfile()
  save_gs(gs, tmp)
  
  #set the archive permission as readonly
  system2("chmod", paste0("-R 555 ", tmp))
  gs1 <- load_gs(tmp)      
  expect_is(gs1, "GatingSet")
  colnames(gs1)[1] <- "d"
  expect_error(cs_flush_meta(gs_cyto_data(gs1)), "read-only", class = "error")
  if(use_on_disk_idx())
  {
    gh <- gs1[[1]]
    node <- gh_get_pop_paths(gh)[2]
    idx <- gh_pop_get_indices(gh, node)
    idx[1] <- FALSE
    gh_pop_set_indices(gh, node, idx)
    expect_error(gh_idx_flush(gh), "readonly", class = "error")
    
  }
  if(get_default_backend()=="h5")
    expect_error(capture.output(gs1 <- load_gs(tmp, backend_readonly = FALSE), type = "message"), "hdf Error", class = "error")
  else
  {
    gs1 <- load_gs(tmp, backend_readonly = FALSE)
    colnames(gs1)[1]<- "d"
    expect_error(cs_flush_meta(gs_cyto_data(gs1)), "Permission", class = "error")
  }
  #restore write permission so that tmp can be cleanedup
  system2("chmod", paste0("-R 755 ", tmp))
  })
test_that("validity checks for new distributed pb format",
{
            
  tmp1 <- tempfile()
  save_gs(gs, tmp1)
  
  #allow save to the existing folder twice
  expect_message(save_gs(gs, tmp1), "Done")
  
  sn <- sampleNames(gs)
  
  # idx file
  if(use_on_disk_idx())
  {
    idxfile <- file.path(tmp1, paste0(sn, ".idx"))
    # extra
    idxfile1 <- file.path(tmp1, paste0(sn, "1.idx"))
    file.copy(idxfile, idxfile1)
    expect_error(load_gs(tmp1), "No cytoframe file matched", class = "error")
    #missing
    unlink(idxfile, recursive = T)
    unlink(idxfile1)
    gs1 <- load_gs(tmp1)
    gh <- gs1[[1]]
    expect_equal(gh_idx_get_uri(gh), "")
    node <- gh_get_pop_paths(gh)[2]
    expect_error(gh_pop_get_indices(gh, node), "not available", class = "error")
    cnt <- gh_pop_get_count(gh, node)
    recompute(gs1)
    expect_equal(cnt, gh_pop_get_count(gh, node))
  }
    
  #extra pb
  gspbfile <- paste0(identifier(gs), ".gs")
  gspbfile1 <- "t.gs"
  file.copy(file.path(tmp1, gspbfile), file.path(tmp1, gspbfile1))
  expect_error(load_gs(tmp1), "Multiple .gs", class = "error")
  expect_error(save_gs(gs, tmp1), "not matched to GatingSet uid", class = "error")
  
  #extra h5
  h5f <- paste0("t.", get_default_backend())
  file.rename(file.path(tmp1, gspbfile1), file.path(tmp1, h5f))
  expect_error(load_gs(tmp1), "No .pb file matched for sample", class = "error")
  expect_error(save_gs(gs, tmp1), "file not matched to any sample", class = "error")
  
  #unrecognized file
  f1 <- "t.txt"
  file.rename(file.path(tmp1, h5f), file.path(tmp1, f1))
  expect_error(load_gs(tmp1), "not recognized", class = "error")
  expect_error(save_gs(gs, tmp1), "not recognized", class = "error")
  file.remove(file.path(tmp1, f1))
  
  #missing gs pb
  gspbtmp <- tempfile()
  file.rename(file.path(tmp1, gspbfile), gspbtmp)
  expect_error(load_gs(tmp1), "No .gs file found", class = "error")
  expect_error(save_gs(gs, tmp1), "gs file missing", class = "error")
  file.rename(gspbtmp, file.path(tmp1, gspbfile))
  
  #missing h5
  h5f <- paste(sn, get_default_backend(), sep = ".")    
  unlink(file.path(tmp1, h5f), recursive = TRUE)
    
  expect_error(load_gs(tmp1), "No cytoframe file matched", class = "error")
  expect_error(save_gs(gs, tmp1), "file missing", class = "error")
})
test_that("save indexed GatingSet",
{
  
  mytest <- function(gs, mem = TRUE){
    cs <- gs_cyto_data(gs)
    gs_cyto_data(gs) <- cs[, 1:2]
    tmp <- tempfile()
    expect_error(save_gs(gs, tmp, backend_opt = "symlink"), "subsetted")
    #cp
    tmp <- tempfile()
    suppressMessages(save_gs(gs, tmp))
    h5 <- list.files(cs_get_uri(gs), full = T)
    expect_equal(length(h5), ifelse(mem, 0 ,2))
    ext <- ifelse(mem, ".h5", get_default_backend())
    h5 <- list.files(tmp, pattern = ext, full = T)
    expect_equal(length(h5), 2)
    #mv
    tmp <- tempfile()
    suppressMessages(save_gs(gs, tmp, backend_opt = "move"))
    h5 <- list.files(cs_get_uri(gs), full = T)
    expect_equal(length(h5), 0)
    h5 <- list.files(tmp, pattern = ext, full = T)
    expect_equal(length(h5), 2)
    gs1 <- load_gs(tmp)
    expect_equal(length(colnames(gs_cyto_data(gs1))), 2)
 }
  
  #mem
  fs <- GvHD[1:2]
  
  gs <- GatingSet(fs, backend = "mem")
  expect_true(cf_get_uri(get_cytoframe_from_cs(gs_cyto_data(gs), 1)) == "")
  mytest(gs)
  
  #h5
  gs <- GatingSet(fs)
  expect_true(cf_get_uri(get_cytoframe_from_cs(gs_cyto_data(gs), 1)) != "")
  mytest(gs, F)
})

test_that("save GatingSet to archive",
    {
      skip_if(get_default_backend() == "mem")
      pd <- pData(gs)
      pd[["newCol"]] <- "A"
      pData(gs) <- pd
      id <- identifier(gs)
      expect_is(id, "character")
      #save to a new dir
      tmp <- tempfile()
      skip_if(win32_flag)#somehow save_gs fail bioc win32bit
      save_gs(gs, path = tmp)
      #load it back
      expect_silent(gs <- load_gs(tmp))
      expect_output(gs <- load_gs(tmp, verbose = TRUE), "archived by")
      expect_that(gs, is_a("GatingSet"))
      #check the pd
      expect_setequal(colnames(pData(gs)), colnames(pd))
      #overwrite the existing dir
      expect_message(save_gs(gs, path = tmp), "Done")
      #fail to save due to mismatch of gs id
      expect_error(save_gs(gs[1], path = tmp), "not match", class = "error")
      # check gs id is preserved
      cdf <- list.files(tmp, get_default_backend(), full.names = TRUE)
      expect_equal(identifier(gs), id)
      #update gs id
      id.new <- "test"
      identifier(gs) <- id.new
      expect_equal(identifier(gs), id.new)
      #restore id
      identifier(gs) <- id
      #fail to save due to the mismatch of h5 files between folder and gs
      redundant <- file.path(tmp, paste0("redundant.", get_default_backend()))
      file.create(redundant)
      expect_error(save_gs(gs, path = tmp), "Not a valid", class = "error")
      
      # protect the readonly data
      cf <- get_cytoframe_from_cs(gs_cyto_data(gs), 1)
      expect_error(exprs(cf)[1,1] <- 0, "read-only", class = "std::domain_error")
      colnames(gs_cyto_data(gs))[1] <- "dd"
      expect_equal(colnames(gs)[1], "dd")
      expect_error(cs_flush_meta(gs_cyto_data(gs)) , "read-only", class = "std::domain_error")
      #but can be saved 
      tmp1 <- tempfile()
      #either by save_gs
      save_gs(gs, tmp1)
      gs1 <- load_gs(tmp1)
      expect_equal(colnames(gs1)[1], "dd")
      #or by clone
      gs1 <- gs_clone(gs)
      expect_equal(colnames(gs1)[1], "dd")
      expect_silent(cs_flush_meta(gs_cyto_data(gs1)))
      
      
      #skip h5 when write to itself
      file.remove(redundant)
      expect_message(save_gs(gs, path = tmp, backend_opt = "skip"), "Done")
      #load it back to ensure the is_h5 flag is preserved
      gs1 <- load_gs(tmp)
      expect_equal(cf_get_uri(gs_get_cytoframe(gs,1)),  cf_get_uri(gs_get_cytoframe(gs1,1)))
      
      #skip h5 when the new dir already has h5 pre-copied
      tmp1 <- tempfile()
      dir.create(tmp1)
      # file.copy(cdf, file.path(tmp1, list.files(tmp, ".h5")))
      expect_message(save_gs(gs, path = tmp1, backend_opt = "skip"), "Done")
      expect_equal(length(list.files(tmp1, get_default_backend())), 0)
      
      #link
      tmp1 <- tempfile()
      dir.create(tmp1)
      expect_error(save_gs(gs, path = tmp1, backend_opt = "link"), "no longer", class = "error")
      
      #symlink
      skip_on_os(c("mac","windows"))
      tmp1 <- tempfile()
      dir.create(tmp1)
      expect_message(save_gs(gs, path = tmp1, backend_opt = "symlink"), "Done")
      h5 <- list.files(tmp1, get_default_backend(), full.names = TRUE)
      expect_equal(length(h5), 1)
      expect_equal(normalizePath(Sys.readlink(h5)), normalizePath(cdf))
     
      #move
      tmp1 <- tempfile()
      dir.create(tmp1)
      expect_message(save_gs(gs, path = tmp1, backend_opt = "move"), "Done")
      h5 <- list.files(tmp1, get_default_backend(), full.names = TRUE)
      expect_equal(length(h5), 1)
      expect_false(file.exists(cdf))
      
      
  })
## it is placed here because trans may get cleared later on by cloning process
test_that("gh_get_transformations",{    
      
      
      
      flowDataPath <- system.file("extdata", package = "flowWorkspaceData")
      fcsFiles <- list.files(pattern = "CytoTrol", flowDataPath, full = TRUE)
      fr  <- read.FCS(fcsFiles[[1]])
      
      chnl <- "V450-A"
      raw <- exprs(fr)[,chnl]
      gh <- gs[[1]]
      
      #test gh_get_transformations
      
      # only return the transfromation associated with given channel
      tran <- gh_get_transformations(gh, channel = "<B710-A>")
      expect_is(tran, "function")
      
      expect_null(gh_get_transformations(gh, channel = "<"))
      
      tran <- gh_get_transformations(gh, channel = "<B710-AA>")
      expect_null(tran)
      
      trans <- gh_get_transformations(gh)
      expect_is(trans, "list")
      
      trans <- trans[[1]]
      inverseTrans <- gh_get_transformations(gh, inverse = TRUE)[[1]]
      
      transformed <- trans(raw)
      raw1 <- inverseTrans(transformed)
      expect_equal(raw, raw1, tolerance = 2e-3)
      
      #test flowJoTrans
      trans <- flowjo_biexp()
      inverseTrans <- flowjo_biexp(inverse = TRUE)
      
      transformed <- trans(raw)
      raw1 <- inverseTrans(transformed)
      expect_equal(raw, raw1, tolerance = 2e-3)
      
      
    })



source("GatingSet-testSuite.R", local = TRUE)



gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
    gh <<- gs[[1]]
    gh2 <- gs[["CytoTrol_CytoTrol_1.fcs"]]
    expect_is(gh, "GatingHierarchy")
    expect_true(!identical(get_gatingset_id(gh2@pointer), get_gatingset_id(gh@pointer)))
    expect_true(!identical(gh@pointer, gh2@pointer))
    expect_equal(gh_pop_get_data(gh), gh_pop_get_data(gh2))
    
})


source("GatingHierarchy-testSuite.R", local = TRUE)


##TODO: has some issue with the latest change from #203, breaks the test on gs_get_singlecell_expression call
test_that("Construct new GatingSet based on the existing gating hierarchy",
   {
     #re-load the gs since the trans get lost during clone
     suppressWarnings(suppressMessages(gs1 <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))))
     gh <- gs1[[1]]
     suppressMessages(gs <<- gh_apply_to_new_fcs(gh, list.files(dataDir, pattern = "CytoTrol_CytoTrol",full = TRUE)[1], compensation_source = "template"))
     expect_that(gs, is_a("GatingSet"))
     expect_equal(gs_get_pop_paths(gs), gs_get_pop_paths(gs1))
     expect_equal(gs_pop_get_stats(gs), gs_pop_get_stats(gs1), tol = 2e-3)
     
   })
isCpStaticGate <<- TRUE
source("GatingSet-testSuite.R", local = TRUE)

gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
     gh <<- gs[[1]]
     expect_that(gh, is_a("GatingHierarchy"));
   })


source("GatingHierarchy-testSuite.R", local = TRUE)
