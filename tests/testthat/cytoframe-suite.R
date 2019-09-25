test_that("cf_scale_time_channel", {
  cf1 <- realize_view(cf)
  
  rg <- range(cf1, "data")
  cf_scale_time_channel(cf1)
  rg1 <- range(cf1, "data")
  rg[, "Time"] <- rg[, "Time"] * as.numeric(keyword(cf1, "$TIMESTEP")[[1]])
  expect_equal(rg, rg1, tol = 3e-8)
})
test_that("load_meta", {
  cf1 <- realize_view(cf)
  tmp <- cf_get_h5_file_path(cf1)
  skip_if_not(ish5)
  oldvalue <- keyword(cf1)[["TUBE NAME"]]
  keyword(cf1)[["TUBE NAME"]] <- "dd"
  expect_equivalent(keyword(cf1)[["TUBE NAME"]], "dd")
  
  #discard changes
  cf_load_meta(cf1)
  expect_equivalent(keyword(cf1)[["TUBE NAME"]], oldvalue)
  keyword(cf1)[["TUBE NAME"]] <- "dd"
  #flush the change
  cf_flush_meta(cf1)
  cf2 <- load_cytoframe_from_h5(tmp)
  expect_equivalent(keyword(cf2)[["TUBE NAME"]], "dd")
  
  cf_load_meta(cf1)
  expect_equivalent(keyword(cf1)[["TUBE NAME"]], "dd")  
})

test_that("Subset", {
      #Subset by gate
      is_equal_flowFrame(Subset(cf, rectGate), Subset(fr, rectGate))

    })
test_that("cytoset_to_flowframe", {
  fr1 <- cytoframe_to_flowFrame(cf)
  is_equal_flowFrame(cf, fr1)

})


test_that("get_h5_file_path", {
  skip_if_not(ish5)
  
      h5file <- cf_get_h5_file_path(cf)
      expect_true(file.exists(h5file))
      
      cf1 <- load_cytoframe_from_fcs(fcs_file)
      expect_true(cf_get_h5_file_path(cf1)=="")
    })
test_that("write permission", {
  #newly created from fcs: writable
  cf1 <- load_cytoframe_from_fcs(fcs_file, is_h5 = TRUE, which.lines = 1:10)
  exprs(cf1)[1,1] <- 1
  expect_equivalent(exprs(cf1)[1,1], 1)
  
  #loaded from h5: default readonly
  h5file <- cf_get_h5_file_path(cf1)
  rm(cf1)
  invisible(gc())
  cf2 <- load_cytoframe_from_h5(h5file)
  expect_error(exprs(cf2)[1,1] <- 2, "read-only")
  cf_unlock(cf2)
  exprs(cf2)[1,1] <- 2
  expect_equivalent(exprs(cf2)[1,1], 2)
  
  #loaded from h5: explicitly set write mode
  cf2 <- load_cytoframe_from_h5(h5file, readonly = FALSE)
  exprs(cf2)[1,1] <- 2
  expect_equivalent(exprs(cf2)[1,1], 2)
  
  #fresh deep cp: writable
  rm(cf2)
  invisible(gc())
  cf2 <- load_cytoframe_from_h5(h5file)
  cf3 <- realize_view(cf2)
  exprs(cf3)[1,1] <- 3
  expect_equivalent(exprs(cf3)[1,1], 3)
  
})

test_that("lock", {
  cf1 <- realize_view(cf)
  #writable
  exprs(cf1)[1,1] <- 3
  expect_equivalent(exprs(cf1)[1,1], 3)
  #lock it
  cf_lock(cf1)
  expect_error(exprs(cf1)[1,1] <- 4, "read-only")
  expect_error(keyword(cf1)[["TUBE NAME"]] <- "dd", "read-only")
  expect_equivalent(exprs(cf1)[1,1], 3)
  cf_unlock(cf1)
  exprs(cf1)[1,1] <- 4
  expect_equivalent(exprs(cf1)[1,1], 4)
  key.old <- keyword(cf1)[["TUBE NAME"]]
  key.new <- "dd"
  keyword(cf1)[["TUBE NAME"]] <- key.new
  expect_equivalent(keyword(cf1)[["TUBE NAME"]], key.new)
  skip_if_not(ish5)
  #test flush and load
  cf_lock(cf1)
  cf_load_meta(cf1)
  expect_equivalent(keyword(cf1)[["TUBE NAME"]], key.old)
  
  #shallow cp
  cf1 <- shallow_copy(cf)
})


test_that("[", {
      cf1 <- cf[1:100, 2:3]
      is_equal_flowFrame(cf1, fr[1:100, 2:3])
      
      #nc1 and nc share the cdf file
      expect_equal(cf_get_h5_file_path(cf1), cf_get_h5_file_path(cf))

    })

test_that("copy", {
  cf1 <- shallow_copy(cf)
  expect_equal(cf_get_h5_file_path(cf1), cf_get_h5_file_path(cf))  
  
  cf1 <- realize_view(cf)
  skip_if_not(ish5)
  
  expect_false(identical(cf_get_h5_file_path(cf1), cf_get_h5_file_path(cf)))
  is_equal_flowFrame(cf, cf1)
})

test_that("exprs<-", {
  cf1 <- realize_view(cf)
  exprs(cf1)[1:10, 1:10] <- 0
  expect_true(all(exprs(cf1)[1:10, 1:10] == 0))
  expect_false(all(exprs(cf)[1:10, 1:10] == 0))
  
  expect_error(exprs(cf1) <- exprs(cf1)[1:10, ] , "size")
  expect_error(exprs(cf1) <- exprs(cf1)[, 1:2] , "size")
  
})

test_that("colnames<-", {
      cf1 <- realize_view(cf)
      coln <- colnames(cf1)
      expect_equal(coln, colnames(fr))
      cf2 <- cf1[, coln[1:2]]
      newColNames <- c("c1", "c2")
      colnames(cf2) <- newColNames
      expect_equal(colnames(cf2), newColNames)
      
      expect_equivalent(unlist(keyword(cf2)[c("$P1N", "$P2N")]), newColNames)

      #TODO:change the order of colnames
      coln <- colnames(cf1)
      cf2 <- cf1[, coln[2:1]]
      expect_error(colnames(cf2) <- newColNames, "colname already exists")
      cf_swap_colnames(cf2, "c1", "c2")
      expect_equivalent(unlist(keyword(cf2)[c("$P1N", "$P2N")]), rev(newColNames))
    })

test_that("parameters<-", {
  cf1 <- realize_view(cf)
  pd <- pData(parameters(cf1))
  pd[, "desc"][5] <- "cd4"
  pd[, "minRange"][2] <- 1
  pData(parameters(cf1)) <- pd
  expect_equal(pData(parameters(cf1)), pd)
  
})

test_that("keyword<-", {
  cf1 <- realize_view(cf)
  kw <- kw.old <- keyword(cf1)
  kw[["$P5S"]] <- "cd4"#update
  kw[["$P6S"]] <- NULL #delete
  kw[["testkw"]] <- 11 #add new
  keyword(cf1) <- kw
  kw <- collapse_desc(kw, collapse.spill = FALSE)
  expect_equal(keyword(cf1)[names(kw)], kw)
  skip_if_not(ish5)
  
  #now meta won't be flushed to disk automatically after destroy cf1
  tmp <- cf_get_h5_file_path(cf1)
  rm(cf1)
  invisible(gc())
  cf2 <- load_cytoframe_from_h5(tmp, readonly = FALSE)
  expect_equal(keyword(cf2)[names(kw.old)], kw.old)
  #explicit flush
  keyword(cf2) <- kw
  cf_flush_meta(cf2)
  rm(cf2)
  invisible(gc())
  cf2 <- load_cytoframe_from_h5(tmp)
  expect_equal(keyword(cf2)[names(kw)], kw)
})



# test_that("range", {
# cf <- flowFrame_to_cytoframe(GvHD[[1]], is_h5 = TRUE)
#   rng1 <- data.frame("FSC-H" = c(0,1023)
#                      ,"SSC-H" = c(0,1023)
#                      ,"FL1-H" = c(1,10000)
#                      ,"FL2-H" = c(1,10000)
#                      ,"FL3-H" = c(1,10000)
#                      ,"FL2-A" = c(0,1023)
#                      ,"FL4-H" = c(1,10000)
#                      ,"Time" = c(0,1023)
#                      , row.names = c("min", "max")
#                      , check.names = FALSE
#   )
#   expect_equal(range(cf), rng1)
#   
#   expect_equal(range(fr, "instrument"), rng1)
#   
#   expect_equal(range(fr, type = "instrument"), rng1)
#   
#   expect_error(range(fr, "FSC-H"), "only accept two")
#   
#   rng2 <- data.frame("FSC-H" = c(59,1023)
#                      ,"SSC-H" = c(6,1023)
#                      ,"FL1-H" = c(1,10000)
#                      ,"FL2-H" = c(1.000,9221.666)
#                      ,"FL3-H" = c(1.000,1131.784)
#                      ,"FL2-A" = c(0,1023)
#                      ,"FL4-H" = c(1,1162.77)
#                      ,"Time" = c(1, 755)
#                      , row.names = c("min", "max")
#                      , check.names = FALSE
#   )
#   expect_equal(range(fr, type = "data")  ,rng2, tolerance = 4e-7)
#   expect_equal(range(fr, "data")  ,rng2, tolerance = 4e-7)
#   expect_error(range(fr, "FSC-H", type = "data"), "only accept two")
#   
# })
# 
test_that("transform", {
  
  fr <- GvHD[pData(GvHD)$Patient %in% 6:7][[1]]
  cf <- flowFrame_to_cytoframe(fr, is_h5 = TRUE)
  h5 <- cf_get_h5_file_path(cf)
  translist <- transformList(c("FL1-H", "FL2-H"), lgcl)
  
  #in place transform
  transform(cf, translist)
  expect_equal(h5, cf_get_h5_file_path(cf))
  trans_range <- range(cf, "data")
  expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226))
  expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))
})

test_that("load_fcs", {
  fr <- read.FCS(list.files(system.file("extdata","compdata","data",package="flowCore"), full.names = TRUE)[1])
  #write to carry flowCore_Rmax keywords
  tmp <- tempfile()
  write.FCS(fr, tmp)
  fr <- read.FCS(tmp)
  cf <- load_cytoframe_from_fcs(tmp)
  #check if pickup the new keyword for range
  is_equal_flowFrame(fr, cf)
  #random select rows to read
  set.seed(1)
  cf <- load_cytoframe_from_fcs(tmp, which.lines = 10)
  expect_equal(nrow(cf), 10)
  set.seed(1)
  cf2 <- load_cytoframe_from_fcs(tmp, which.lines = 10)
  expect_equal(exprs(cf), exprs(cf2))
  set.seed(2)
  cf2 <- load_cytoframe_from_fcs(tmp, which.lines = 10)
  expect_false(isTRUE(all.equal(exprs(cf), exprs(cf2))))
  #pass an existing row indices explicitly
  select <- sample(seq_len(nrow(fr)), 20)
  cf <- load_cytoframe_from_fcs(tmp, which.lines = select)
  fr <- read.FCS(tmp, which.lines = select)
  is_equal_flowFrame(fr, cf)
  
  #TODO: yet to determine whether the original FCS R parser is correct on
  # setting range from flowCore_Rmax in makeFCSparameters call without checking condition of x[["transformation"]] == "custom"
  #expect_equal(range(fr)[2,], range(cf)[2,] + 1)
})
