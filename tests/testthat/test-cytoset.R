context("cytoset accessors")
# fs <- GvHD[pData(GvHD)$Patient %in% 6:7][1:4]#can't use it due to its malformated FCS TEXT making test difficult
fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
fs <- read.flowSet(fcs_files)
suppressMessages(cs <- load_cytoset_from_fcs(fcs_files, is_h5 = TRUE))
samples <- sampleNames(cs)
lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)

test_that("[[", {
      
      sn <- samples[1]
      fr <- cs[[sn]]
      expect_is(fr, "flowFrame")
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


test_that("cs_get_h5_file_path", {
      h5file <- cs_get_h5_file_path(cs)
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
      expect_equal(cs_get_h5_file_path(nc1), cs_get_h5_file_path(cs))
      
      #index by cols
      chnls <- colnames(cs)
      cs1 <- cs[, 1:2]
      expect_equal(colnames(cs1), chnls[1:2])
      expect_equal(colnames(cs), chnls)
      
      #Test negative subscripts
      cs1 <- flowSet_to_cytoset(GvHD)
      expect_equal(sampleNames(cs1[-c(3,7,11)]), sampleNames(cs1)[-c(3,7,11)])
      expect_error(cs1[c(-3, 7, -11)], "Cannot mix positive and negative subscripts")
      
      #Test negative subsetting of columns for cytoset and cytoframe
      expect_equal(colnames(cs1[,-c(3,5)]), colnames(cs1)[-c(3,5)])
      cf1 <- cs1[[1]]
      expect_equal(colnames(cf1[,-c(2,4)]), colnames(cf1)[-c(2,4)])
      
    })

test_that("subset", {
# browser()
      nc_sub <- subset(cs, name == samples[2])
      is_equal_flowSet(nc_sub, fs[2])

    })

test_that("copy", {
  cs1 <- copy_view(cs)#or cs[]
  expect_equal(cs_get_h5_file_path(cs1), cs_get_h5_file_path(cs))
  
  cs1 <- realize_view(cs)
  expect_false(identical(cs_get_h5_file_path(cs1), cs_get_h5_file_path(cs)))
  is_equal_flowSet(cs1,cs)
})

test_that("[[<-", {
  cs1 <- realize_view(Subset(cs, sampleFilter(1e3)))#TODO:dowsize the data because somehow [[<- is abnormally slow, which needs to be investigated later
  sn <- samples[1]
  
  cf <- get_cytoframe_from_cs(cs1, sn)
  h5 <- cf_get_h5_file_path(cf)
  
  fr <- cytoframe_to_flowFrame(cf)
  exprs(fr)[1:10, 1:10] <- 0
  markernames(fr) <- c("B710-A" = "test")
  
  expect_identical(file.path(cs_get_h5_file_path(cs1),sn), h5)
  
  #write flowFrame
  cs1[[sn]] <- fr
  is_equal_flowFrame(cf, fr)
  is_equal_flowFrame(cs1[[sn]], fr)
  
  #test graceful handling of bad subscripts
  expect_error(cs1[[-5]]<-fr, "subscript out of bounds")
  expect_error(cs1[[c(2,3)]]<-fr, "subscript out of bounds")
  
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
      expect_error(sampleNames(nc) <- newNames, "exists", class = "std::range_error")

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
      newColNames <- rev(newColNames)
      expect_equivalent(unlist(keyword(nc[[1]])[c("$P1N", "$P2N")]), newColNames)
      expect_error(colnames(nc) <- newColNames, "colname already exists", class = "std::domain_error")
      cs_swap_colnames(nc, "c1", "c2")
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
  fr <- cs[[sn]]

  #transform the data
  translist <- transformList(c("FL1-H", "FL2-H"), lgcl)

  #list of transformList
  trans.list <- sapply(sampleNames(nc), function(sn)translist)
  trans.fs1 <- transform(nc, trans.list)
  trans_range <- range(trans.fs1[[sn]], "data")
  expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226))
  expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))
  expect_equal(cs_get_h5_file_path(nc), cs_get_h5_file_path(trans.fs1))
  
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
  expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226))
  expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))

  #subset on channels
  nc <- realize_view(cs[1:2])
  expect_error(nc[[sn]] <- fr_trans[,c("FL1-H")], "colnames")
  nc1 <- nc[,c("FL1-H")]
  #TODO: now it replace the entire cf
  suppressMessages(nc1[[sn]] <- fr_trans[,c("FL1-H")])
  trans_range <- apply(exprs(nc[[sn]]), 2, range)
  #transformed channel
  expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226))
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
#   expect_true(cs_get_h5_file_path(nc1) == cs_get_h5_file_path(cs))
# 
# })
