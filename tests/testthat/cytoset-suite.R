context("-- cytoset")

fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)
fs <- read.flowSet(fcs_files)
suppressMessages(cs <- load_cytoset_from_fcs(fcs_files))
samples <- sampleNames(cs)


test_that("throw on slash symbol in sample names", {
  skip_if(get_default_backend()=="mem")
  
  expect_error(cs <- cytoset(sapply(list.files(cs_get_uri(cs), full.names = TRUE), load_cytoframe)), "invalid", class = "error")
  expect_error(sampleNames(cs)[1] <- "a/b", "invalid", class = "error")
  
})

test_that("nrow", {
  expect_equal(nrow(cs), lapply(cs, nrow))
  
})


test_that("fsApply", {
  fsApply(cs, function(fr){
    expect_is(fr, "flowFrame")
    })
})

test_that("coerce", {
  fr <- as(cs, "flowFrame")
  expect_is(fr, "flowFrame")

})

test_that("empty cs", {
			skip_if(get_default_backend()!="mem")
			
			cs <- cytoset()
			expect_error(cs[[1]], "Empty")
			expect_error(cs[1], "Empty")
			expect_equal(colnames(cs), character())
			# expect_equal(nrow(pData(cs)), 0)
			expect_error(cs_get_uri(cs), "Empty")
			
		})
test_that("col order", {
  fr1 <- GvHD[[1]][1:2, 4:6]
  fr2 <- GvHD[[2]][1:2, 6:4]
  cn1 <- colnames(fr1)
  cn2 <- colnames(fr2)
  
  #constructor from fcs
  tmp1 <- tempfile()
  write.FCS(fr1, tmp1)
  tmp2 <- tempfile()
  write.FCS(fr2, tmp2)
  cs1 <- load_cytoset_from_fcs(c(a=tmp1, b = tmp2))
  expect_equal(length(unique(lapply(cs1, colnames))), 1)
  
  #add/set
  cs1 <- load_cytoset_from_fcs(c(a=tmp1))
  cf <- load_cytoframe_from_fcs(tmp2)
  sn <- "b"
  cs_add_cytoframe(cs1, sn, cf)
  expect_is(markernames(cs1), "character")
  expect_equal(colnames(cf), cn2)
  #the view that is added to cs is reordered
  expect_equal(colnames(get_cytoframe_from_cs(cs1, sn)), cn1)
  
  cs_set_cytoframe(cs1, sn, cf)
  expect_equal(colnames(cf), cn2)
  cf1 <- get_cytoframe_from_cs(cs1, sn)
  expect_equal(colnames(get_cytoframe_from_cs(cs1, sn)), cn1)
  
  #constructor from gs archive
  tmp1 <- tempfile()
  save_cytoset(cs1, tmp1)
  fn <- list.files(tmp1, pattern = ifelse(get_default_backend()=="tile", ".tile", ".h5"), full.names = T)[1]
  #create different ordered h5
  capture_output(cf_write_disk(cf, fn, backend = ifelse(get_default_backend()=="tile", "tile", "h5")))
  expect_equal(colnames(cf), cn2)#verify the order is different
  cs1 <- load_cytoset(tmp1)
  #sample gets reordered after loading into cs
  expect_equal(colnames(get_cytoframe_from_cs(cs1, sn)), cn1)
  
  })
test_that("add/set frames", {
  cs1 <- realize_view(cs)
  cf <- realize_view(get_cytoframe_from_cs(cs1, 1))
  sn <- "a"
  cs_add_cytoframe(cs1, sn, cf)
  expect_equal(sampleNames(cs1), c(sampleNames(cs), sn))
  expect_error(cs_add_cytoframe(cs1, sn, cf), "already", class = "error")
  expect_error(cs_set_cytoframe(cs1, "b", cf), "doesn't exist", class = "error")
  cf <- realize_view(get_cytoframe_from_cs(cs1, 1))
  cf <- cf[, 1:2]
  expect_error(cs_add_cytoframe(cs1, "b", cf), "missing", class = "error")
  expect_error(cs_set_cytoframe(cs1, sn, cf), "missing", class = "error")
  cf <- realize_view(get_cytoframe_from_cs(cs1, 1))
  cs1 <- cs1[,1:2]
  expect_error(cs_add_cytoframe(cs1, "b", cf), "not found", class = "error")
  expect_error(cs_set_cytoframe(cs1, sn, cf), "not found", class = "error")
})
test_that("gs constructor", {
  skip_if(get_default_backend()=="mem")
  cs1 <- realize_view(cs)
  h5 <- cf_get_uri(get_cytoframe_from_cs(cs1,1))
  gs <- GatingSet(cs1)
 #gates preserved 
  gs_pop_add(gs, rectangleGate(d = c(1,10)))
  cs_set_cytoframe(cs1, sampleNames(gs)[1], realize_view(get_cytoframe_from_cs(cs, 1)))
  expect_equal(length(gs_get_pop_paths(gs)), 2)
  #data changed
  expect_false(identical(cf_get_uri(get_cytoframe_from_cs(gs,1)), h5))
  h5 <- cf_get_uri(get_cytoframe_from_cs(cs1,1))
  #cs change in place
  colnames(cs1)[1] <- "d"
  expect_equal(colnames(gs)[1], "d")
  
  expect_true(identical(cf_get_uri(get_cytoframe_from_cs(gs,1)), h5))
  
  })
test_that("cs constructor", {
  fs1 <- read.flowSet(fcs_files)
  colnames(fs1@frames[[samples[[1]]]])[1] <- "FSC"#hack to create discrepancy
  tmp <- tempfile()
  write.flowSet(fs1, tmp)
  expect_error(load_cytoset_from_fcs(path = tmp, pattern = ".fcs"), "inconsistency", class = "error")
  cflist <- lapply(list.files(tmp, ".fcs", full.names = T), load_cytoframe_from_fcs)
  names(cflist) <- letters[1:2]
  expect_error(cytoset(cflist), "missing", class = "error")
  cflist[[1]] <-  cflist[[1]][,-1] 
  expect_error(cytoset(cflist), "not found", class = "error")
  cs <- load_cytoset_from_fcs(fcs_files, which.lines = 1)
  res <- unlist(lapply(cs, nrow))
  names(res) <- NULL
  expect_equal(res, c(1,1))
  })

test_that("save/load", {
  #load h5
  skip_if(get_default_backend() == "mem")
  cf_dir <- cs_get_uri(cs)
  cs <- load_cytoset(cf_dir)
  expect_equal(sampleNames(cs), paste0(samples, ".", get_default_backend()))
  
  pd <- pData(cs)
  pd[["newCol"]] <- "A"
  pData(cs) <- pd
  id <- identifier(cs)
  expect_is(id, "character")
  tmp <- tempfile()
  save_cytoset(cs, path = tmp)
  
  cs <- load_cytoset(tmp)
  expect_that(cs, is_a("cytoset"))
  expect_setequal(colnames(pData(cs)), colnames(pd))
  expect_message(save_cytoset(cs, path = tmp), "Done")
  expect_error(save_cytoset(cs[1], path = tmp), "not matched ")
  
  #idx by col
  tmp1 <- tempfile()
  save_cytoset(cs[, 1:2], tmp1)
  # For later overwrite test, this needs to be writable
  cs1 <- load_cytoset(tmp1, backend_readonly = FALSE)
  expect_equal(colnames(cs1), colnames(cs[,1:2]))#col-idexing result is preserved
  expect_equal(range(cs1[[1]], "data"), range(cs[[1]][,1:2], "data"))#the data is correct
  
  if(get_default_backend() != "mem")
  {
    
    h5f <- cf_get_uri(get_cytoframe_from_cs(cs1, 1))
    f2 <- cf_get_uri(get_cytoframe_from_cs(cs, 1))
    if(get_default_backend()=="tile")
    {
      fsize1 <- sum(file.info(list.files(h5f, all.files = TRUE, recursive = TRUE, full.names = T))$size)
      fsize2 <- sum(file.info(list.files(f2, all.files = TRUE, recursive = TRUE, full.names = T))$size)
    }else
    {
      fsize1 <- file.size(h5f)  
      fsize2 <- file.size(f2)  
    }
    
    expect_lt(fsize1, fsize2)#file size decreased
  }
  expect_error(save_cytoset(cs[, 1:2], tempfile(), backend_opt = "symlink"), "Only 'copy'")
  #overwrite existing h5
  id1 <- identifier(cs1)
  cs2 <- cs1[, 2]
  identifier(cs2) <- id1#force it to be identical in order to be able to overwrite the existing folder
  capture_output(save_cytoset(cs2, tmp1))
  cs1 <- load_cytoset(tmp1)
  expect_equal(colnames(cs1), colnames(cs[,2]))#col-idexing result is preserved
  expect_equal(range(cs1[[1]], "data"), range(cs[[1]][,2], "data"))#the data is correct
  
  if(get_default_backend() != "mem")
  {
    f2 <- cf_get_uri(get_cytoframe_from_cs(cs1, 1))
    if(get_default_backend()=="tile")
    {
     
      fsize2 <- sum(file.info(list.files(f2, all.files = TRUE, recursive = TRUE, full.names = T))$size)
    }else
    {
     
      fsize2 <- file.size(f2)  
    }
    expect_lt(fsize2, fsize1)#file size decreased
  }
  
  expect_equal(identifier(cs), id)
  id.new <- "test"
  identifier(cs) <- id.new
  expect_equal(identifier(cs), id.new)
  #restore id
  identifier(cs) <- id
  # file.copy(cdf, file.path(tmp, "redundant.nc"))
  # expect_error(save_cytoset(cs, path = tmp), "Not a valid", class = "error")
  colnames(cs)[1] <- "dd"
  expect_equal(colnames(cs)[1], "dd")
  if(get_default_backend() != "mem")
  {
    expect_error(cs_flush_meta(cs) , "read-only", class = "error")
    cf <- get_cytoframe_from_cs(cs, 1)
    expect_error(exprs(cf)[1,1] <- 0, "read-only", class = "error")
  
    cs <- load_cytoset(tmp, backend_readonly = FALSE)
    colnames(cs)[1] <- "dd"
    expect_silent(cs_flush_meta(cs))
    cs <- load_cytoset(tmp)
    expect_equal(colnames(cs)[1], "dd")
  }
})

test_that("[[", {
      
      sn <- samples[1]
      fr <- cs[[sn]]
      expect_is(fr, "cytoframe")
      fr1 <- fs[[sn]]
      is_equal_flowFrame(fr, fr1)
      fr <- cs[[1]]
      is_equal_flowFrame(fr, fr1)
      
      #without reading data
      fr <- cs[[sn, use.exprs = FALSE]]
      cn <- colnames(fr1)
      fr1@exprs <- matrix(nrow = 0, ncol = length(cn), dimnames = list(NULL, cn))
      is_equal_flowFrame(fr, fr1)
      
      #subset by channel
      chnls <- cn[c(1,3)]
      fr <- cs[[sn, chnls]]
      fr1 <- fs[[sn, chnls]]
      is_equal_flowFrame(fr, fr1)
      
      #subset by int
      chnls <- c(3,5,1)
      fr <- cs[[sn, chnls]]
      fr1 <- fs[[sn, chnls]]
      is_equal_flowFrame(fr, fr1)
      
      #subset by single channel 
      chnls <- c(3)
      fr <- cs[[sn, chnls]]
      fr1 <- fs[[sn, chnls]]
      is_equal_flowFrame(fr1, fr)
      
      # Test graceful handling of bad subscripts
      expect_error(cs[[-2]], "subscript out of bounds")
      expect_error(cs[[c(2,4)]], "subscript out of bounds")
      
    })

test_that("cytoset_to_flowSet", {
  fs1 <- cytoset_to_flowSet(cs)
  expect_is(fs1, "flowSet")
  expect_equal(colnames(fs1), colnames(cs))
  expect_equal(pData(fs1), pData(cs))

  is_equal_flowSet(cs, fs1)

})


test_that("cs_get_uri", {
      skip_if(get_default_backend() == "mem")
      expect_error(cf_get_uri(cs), "cytoframe")
      h5file <- cs_get_uri(cs)
      expect_true(dir.exists(h5file))
      
    })


test_that("[", {
      #index by samples
      sn <- samples[2:1]
      nc1 <- cs[sn]
      expect_is(nc1, "cytoset")
      expect_equal(length(nc1), 2)
      is_equal_flowSet(fs[sn], nc1)

      #nc1 and nc share the cdf file
      expect_equal(cs_get_uri(nc1), cs_get_uri(cs))
      
      #index by cols
      chnls <- colnames(cs)
      cs1 <- cs[, 1:2]
      expect_equal(colnames(cs1), chnls[1:2])
      expect_equal(colnames(cs), chnls)
      
      skip_if_not(get_default_backend()=="tile")
      #Test negative subscripts
      cs1 <- flowSet_to_cytoset(GvHD)
      expect_equal(sampleNames(cs1[-c(3,7,11)]), sampleNames(cs1)[-c(3,7,11)])
      expect_error(cs1[c(-3, 7, -11)], "Cannot mix positive and negative subscripts")
      
      #edge cases
      ridx <- sapply(sampleNames(fs), function(sn)return(F), simplify = FALSE)
      expect_equivalent(fsApply(Subset(cs, ridx), nrow, simplify = F), list(0,0))
      #Test negative subsetting of columns for cytoset and cytoframe
      expect_equal(colnames(cs1[,-c(3,5)]), colnames(cs1)[-c(3,5)])
      cf1 <- cs1[[1, returnType="cytoframe"]]
      expect_equal(colnames(cf1[,-c(2,4)]), colnames(cf1)[-c(2,4)])
      
    })

test_that("subset", {
# browser()
      nc_sub <- subset(cs, name == samples[2])
      is_equal_flowSet(nc_sub, fs[2])

    })

test_that("copy", {
  skip_if(get_default_backend()=="mem")
  cs1 <- cs[]
  expect_equal(cs_get_uri(cs1), cs_get_uri(cs))
  
  cs1 <- realize_view(cs)
  expect_false(identical(cs_get_uri(cs1), cs_get_uri(cs)))
  is_equal_flowSet(cs1,cs)
})

test_that("[[<-", {
  skip_if(get_default_backend()=="mem")
  cs1 <- realize_view(Subset(cs, sampleFilter(1e3)))#TODO:dowsize the data because somehow [[<- is abnormally slow, which needs to be investigated later
  sn <- samples[1]
  
  cf <- get_cytoframe_from_cs(cs1, sn)
  h5 <- cf_get_uri(cf)
  
  fr <- cytoframe_to_flowFrame(cf)
  exprs(fr)[1:10, 1:10] <- 0
  markernames(fr) <- c("B710-A" = "test")
  
  expect_identical(normalizePath(file.path(cs_get_uri(cs1),paste(sn, get_default_backend(), sep = "."))), normalizePath(h5))
  
  #write flowFrame
  cs1[[sn]] <- fr
  is_equal_flowFrame(cf, fr)
  is_equal_flowFrame(cs1[[sn]], fr)
  
  #test graceful handling of bad subscripts
  expect_error(cs1[[-5]]<-fr, "subscript out of bounds")
  expect_error(cs1[[c(2,3)]]<-fr, "subscript out of bounds")
  
  #write cf
  cf1 <- realize_view(cf)
  h5 <- cf_get_uri(cf1)
  cs1[[sn]] <- cf1
  is_equal_flowFrame(cs1[[sn]], cf1)
  expect_false(identical(normalizePath(file.path(cs_get_uri(cs1),paste(sn, get_default_backend(), sep = "."))), normalizePath(h5)))
  #set cf
  cs_set_cytoframe(cs1, sn, cf1)  
  cf2 <- cs1[[sn, returnType = "cytoframe"]]
  expect_identical(normalizePath(cf_get_uri(cf2)), normalizePath(h5))
  colnames(cf1)[1] <- "d"
  expect_equal(colnames(cf2)[1], "d")
  })

test_that("lapply", {
  cs1 <- realize_view(cs)
  expect_true(all(unlist(lapply(cs1, is, "cytoframe"))))
  expect_equal(lapply(cs1, nrow), sapply(sampleNames(cs1), function(sn)nrow(cs1[[sn]]), simplify = FALSE))
})

fs <- GvHD[pData(GvHD)$Patient %in% 6:7][1:4]
test_that("flowSet_to_cytoset", {
  cs <<- flowSet_to_cytoset(fs)
  pd1 <- pData(fs)
  pd2 <- pData(cs)
  for(i in seq_along(colnames(pd1)))
    pd1[, i] <- as.character(pd1[, i])
  expect_equivalent(pd1, pd2[, colnames(pd1)])

  cs_load_meta(cs)
  pd2 <- pData(cs)
  
  expect_equivalent(pd1, pd2[, colnames(pd1)])
  })
samples <- sampleNames(cs)
# sampleNames(fs) <- samples
rectGate <- rectangleGate(filterId="nonDebris","FSC-H"=c(200,Inf))

test_that("Subset", {
  expect_false(cs_is_subsetted(cs))
  expect_true(cs_is_subsetted(cs[,1:2]))
  expect_true(cs_is_subsetted(Subset(cs, rectGate)))
  #Subset by gate
  expect_equivalent(fsApply(Subset(cs, rectGate), nrow), fsApply(Subset(fs, rectGate), nrow))
  #ensure the original cs is intact
  expect_equivalent(fsApply(cs, nrow), fsApply(fs, nrow))
})
test_that("sampleNames<-", {
      sn <- samples[1:2]
      nc <- cs[sn]
      newNames <- c("s1", "s2")
      sampleNames(nc) <- newNames
      expect_equal(sampleNames(nc), newNames)
      #the original cs is not affected since samples belong to the views thus is changed independently
      expect_equal(sampleNames(cs), samples)
      is_equal_flowFrame(cs[sn][[1]], nc[[1]])

      newNames <- c("s01", "s2")
      sampleNames(nc) <- newNames
      expect_equal(sampleNames(nc), newNames)
      is_equal_flowFrame(cs[sn][[1]], nc[[1]])

      newNames <- c("s2", "s2")
      expect_error(sampleNames(nc) <- newNames, "exists", class = "error")

      #replace the single subsetted fs
      nc <- nc["s2"]
      sampleNames(nc) <- "dd"
      expect_equal(sampleNames(nc), "dd")
      is_equal_flowFrame(cs[sn][[2]], nc[[1]])

      
      })

test_that("colnames<-", {
      sn <- samples[1:2]
      coln <- colnames(cs)

      nc <- realize_view(cs[sn, coln[1:2]])
      newColNames <- c("c1", "c2")
      colnames(nc) <- newColNames
      expect_equal(colnames(nc), newColNames)
      invisible(fsApply(nc, function(fr)expect_equal(colnames(fr), newColNames)))

      expect_equivalent(unlist(keyword(nc[[1]])[c("$P1N", "$P2N")]), newColNames)

      #change the order of colnames
      nc <- realize_view(cs[sn, coln[2:1]])
      colnames(nc) <- newColNames
      expect_equivalent(unlist(keyword(nc[[1]])[c("$P1N", "$P2N")]), newColNames)
      newColNames <- rev(newColNames)
      colnames(nc) <- newColNames
      
      expect_equal(colnames(nc), newColNames)
      
    })
# 
# test_that("split", {
#       
#       #split by factor
#       splitBy <- factor(c("p1","p2","p1","p2"))
#       
#       nclist <- split(cs, splitBy)
#       fslist <- split(fs, splitBy)
#       expect_is(nclist, "list")
#       
#       expect_equal(names(nclist), names(fslist))
#       invisible(lapply(names(nclist), function(thisPop){
#                 is_equal_flowSet(nclist[[thisPop]], fslist[[thisPop]])
#                 expect_equal(getFileName(nclist[[thisPop]]), getFileName(cs))
#               }))
#       
# 
#       #split by filter
#       nclist <- split(cs, rectGate)
#       fslist <- split(fs, rectGate)
#       expect_is(nclist, "list")
#       expect_equal(names(nclist), names(fslist))
#       invisible(lapply(names(nclist), function(thisPop){
#                 is_equal_flowSet(nclist[[thisPop]], fslist[[thisPop]])
#                 expect_equal(getFileName(nclist[[thisPop]]), getFileName(cs))
#               }))
#       
#     })
# 
# 
test_that("transform", {
  nc <- realize_view(cs[1:2])
  sn <- samples[1]
  #return the entire flowFrame
  fr <- cs[[sn, returnType = "flowFrame"]]

  #transform the data
  translist <- transformList(c("FL1-H", "FL2-H"), lgcl)

  #list of transformList
  trans.list <- sapply(sampleNames(nc), function(sn)translist)
  trans.fs1 <- transform(nc, trans.list)
  trans_range <- range(trans.fs1[[sn]], "data")
  expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226), tol = 2e-7)
  expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872), tol = 2e-7)
  expect_equal(cs_get_uri(nc), cs_get_uri(trans.fs1))
  
  trans.list[[1]] <- logicleTransform()
  expect_error(trans.fs1 <- transform(nc, trans.list), "'transformList'")

  trans.list[[1]] <- translist
  names(trans.list)[1] <- "d"
  expect_error(trans.fs1 <- transform(nc, trans.list), "consistent with flow data")

  fr_trans <- transform(fr, translist)

  #update the data
  nc <- realize_view(cs[1:2])
  suppressMessages(nc[[sn]] <- fr_trans)
  trans_range <- apply(exprs(nc[[sn]]), 2, range)
  expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226), tol = 2e-7)
  expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872), tol = 2e-7)

  #subset on channels
  nc <- realize_view(cs[1:2])
  expect_error(nc[[sn]] <- fr_trans[,c("FL1-H")], "colnames")
  nc1 <- nc[,c("FL1-H")]
  #TODO: now it replace the entire cf
  suppressMessages(nc1[[sn]] <- fr_trans[,c("FL1-H")])
  trans_range <- apply(exprs(nc[[sn]]), 2, range)
  #transformed channel
  expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226), tol = 2e-7)
  #untransformed channel
  # expect_equal(trans_range[, c("FL2-H")], c(1.000, 1637.104), tol = 8e-08)

  # #update chanel colnames
  # nc <- realize_view(cs[1:2])
  # colnames(fr_trans)[3:4] <- c("<FL1-H>", "<FL2-H>")
  # #write data without matching up the colnames
  # suppressMessages(nc[[sn, only.exprs = TRUE]] <- fr_trans)
  # trans_range <- apply(exprs(nc[[sn]]), 2, range)
  # expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226))
  # expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))
  # #colnames remain unchanged
  # expect_equal(colnames(nc), colnames(cs))
  # expect_error(nc[[sn]] <- fr_trans, "colnames of the input are not consistent")

})
# 
# test_that("csApply", {
#   cs1 <- realize_view(cs[1:2])  
#   sn <- samples[1]
#   #use csApply when FUN returns a flowFrame
#   translist <- transformList(c("FL1-H", "FL2-H"), lgcl)
#   suppressMessages(nc1 <- csApply(cs1, transform, translist))
#   expect_is(nc1, "cytoset")
#   expect_equal(sampleNames(cs), sampleNames(nc1))
#   expect_equal(colnames(cs), colnames(nc1))
#   #the other channels remain the same
#   # is_equal_flowSet(cs[, -c(3:4)], nc1[, -c(3:4)], description = FALSE)
#   #tow channels are tranformed
#   trans_range <- apply(exprs(nc1[[sn]]), 2, range)
#   expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226))
#   expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))
#   expect_true(cs_get_uri(nc1) == cs_get_uri(cs))
# 
# })

test_that("cytoset_to_list", {
  skip_if(get_default_backend()!="h5")
  cs <- flowSet_to_cytoset(GvHD)
  cfs <- cytoset_to_list(cs)
  expect_true(is.list(cfs))
  expect_equal(sampleNames(cs), names(cfs))
  expect_true(all(sapply(1:length(cfs), function(idx) {all.equal(cfs[[idx]], cs[[idx, returnType="cytoframe"]])})))
  # spot check
  expect_equal(exprs(cfs[[7]]), exprs(cs[[7, returnType="cytoframe"]]))
})


test_that("keyword setters", {
  cs1 <- realize_view(cs)
  #add new
  cs_keyword_insert(cs1, "k1", 2)
  expect_error(cs_keyword_insert(cs1, "k1", 2), "exist")
  #rename
  cs_keyword_rename(cs1, "k1", "k2")
  expect_error(cs_keyword_rename(cs1, "k1", "k2"), "not found")
  expected <- matrix(rep("2", 4), ncol = 1, dimnames = list(sampleNames(cs), "k2"))
  expect_equal(keyword(cs1, "k2"), expected)
  #set (subset)
  cs_keyword_set(cs1, "k2", 5)
  expected <- matrix(rep("5", 4), ncol = 1, dimnames = list(sampleNames(cs), "k2"))
  expect_equal(keyword(cs1, "k2"), expected)
  #delete
  cs_keyword_delete(cs1, "k2")
  expect_error(cs_keyword_delete(cs1, "k2"), "not found")
  
  # Testing vectorized operations
  cs1 <- realize_view(cs)
  #add new
  cs_keyword_insert(cs1, c("k1", "k2", "k3"), c("red", 5, 1.23))
  # If any is already present, the call should fail
  expect_error(cs_keyword_insert(cs1, c("k1", "k2"), c("blue", 6)), "exist")
  #rename
  cs_keyword_rename(cs1, c("k1", "k2"), c("key1", "key2"))
  expect_error(cs_keyword_rename(cs1, c("k1", "k2"), c("key1", "key2")), "not found")
  expected <- matrix(c(rep("red", 4), rep("5", 4)),
                     ncol = 2, byrow = FALSE, 
                     dimnames = list(sampleNames(cs), c("key1", "key2")))
  expect_equal(keyword(cs1, c("key1", "key2")), expected)
  #set (subset) -- overwrite two and add one
  cs_keyword_set(cs1, c("key1", "key2", "key4"), c("green", 7, "newval"))
  expected <- matrix(c(rep("green", 4), rep("7", 4), rep("1.23", 4), rep("newval", 4)),
                     ncol = 4, byrow = FALSE,
                     dimnames = list(sampleNames(cs), c("key1", "key2", "k3", "key4")))
  expect_equal(keyword(cs1, c("key1", "key2", "k3", "key4")), expected)
  #delete
  cs_keyword_delete(cs1, c("key2", "key4"))
  # If any are not longer present, the call should fail
  expect_error(cs_keyword_delete(cs1, c("key2", "k3")), "not found")
  
  # Testing vectorized operations with named vectors
  cs1 <- realize_view(cs)
  #add new
  cs_keyword_insert(cs1, c(k1="red", k2=5, k3=1.23))
  # If any is already present, the call should fail
  expect_error(cs_keyword_insert(cs1, c(k1="blue", k2=6)), "exist")
  #rename
  cs_keyword_rename(cs1, c(k1="key1", k2="key2"))
  expect_error(cs_keyword_rename(cs1, c(k1="key1", k2="key2")), "not found")
  expected <- matrix(c(rep("red", 4), rep("5", 4)),
                     ncol = 2, byrow = FALSE, 
                     dimnames = list(sampleNames(cs), c("key1", "key2")))
  expect_equal(keyword(cs1, c("key1", "key2")), expected)
  #set (subset) -- overwrite two and add one
  cs_keyword_set(cs1, c(key1="green", key2=7, key4="newval"))
  expected <- matrix(c(rep("green", 4), rep("7", 4), rep("1.23", 4), rep("newval", 4)),
                     ncol = 4, byrow = FALSE,
                     dimnames = list(sampleNames(cs), c("key1", "key2", "k3", "key4")))
  expect_equal(keyword(cs1, c("key1", "key2", "k3", "key4")), expected)
  #delete
  cs_keyword_delete(cs1, c("key2", "key4"))
  # If any are not longer present, the call should fail
  expect_error(cs_keyword_delete(cs1, c("key2", "k3")), "not found")
})