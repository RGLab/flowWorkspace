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
      
    })
rectGate <- rectangleGate(filterId="nonDebris","FSC-H"=c(200,Inf))

test_that("Subset", {
      #Subset by gate
      is_equal_flowSet(Subset(cs, rectGate), Subset(fs, rectGate))

    })
test_that("cytoSet_to_flowSet", {
  fs1 <- cytoSet_to_flowSet(cs)
  expect_is(fs1, "flowSet")
  expect_equal(colnames(fs1), colnames(cs))
  expect_equal(pData(fs1), pData(cs))

  is_equal_flowSet(cs, fs1)

})


test_that("fr_get_h5_file_path", {
      h5file <- fr_get_h5_file_path(get_cytoFrame_from_cs(cs, 1))
      expect_true(file.exists(h5file))
      
    })


test_that("[", {
      sn <- samples[2:1]
      nc1 <- cs[sn]
      expect_is(nc1, "cytoSet")
      expect_equal(length(nc1), 2)
      is_equal_flowSet(fs[sn], nc1)

      #nc1 and nc share the cdf file
      expect_equal(fr_get_h5_file_path(get_cytoFrame_from_cs(nc1, 2)), fr_get_h5_file_path(get_cytoFrame_from_cs(cs, 1)))

    })

test_that("subset", {

      nc_sub <- subset(cs, name == samples[2])
      is_equal_flowSet(nc_sub, fs[2])

    })



test_that("[[<-", {

      sn <- samples[1]
      suppressMessages(nc <- flowSet_to_cytoSet(fs[sn]))
      
      #return the entire flowFrame
      fr <- nc[[sn]]
      
      #transform the data
      #construct transformList first instead of 
      # trransform(fr, `FL1-H` = lgcl(`FL1-H`), `FL2-H` = lgcl(`FL2-H`))
      # because the latter only works in console mode (global envir)
      translist <- transformList(c("FL1-H", "FL2-H"), lgcl)
      
      #list of transformList
      trans.list <- sapply(sampleNames(nc), function(sn)translist)
      trans.fs1 <- transform(nc, trans.list)
      trans_range <- range(trans.fs1[[sn]], "data")
      expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226)) 
      expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))
      
      trans.list[[1]] <- logicleTransform()
      expect_error(trans.fs1 <- transform(nc, trans.list), "a valid 'transformList'")
      
      trans.list[[1]] <- translist
      names(trans.list)[1] <- "d"
      expect_error(trans.fs1 <- transform(nc, trans.list), "consistent with flow data")
      
      fr_trans <- transform(fr, translist)
      
      #update the data
      suppressMessages(nc[[sn]] <- fr_trans)
      trans_range <- apply(exprs(nc[[sn]]), 2, range)
      expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226)) 
      expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))
      
      #subset on channels
      suppressMessages(nc <- ncdfFlowSet(fs[sn]))
      expect_error(nc[[sn]] <- fr_trans[,c("FL1-H")], "colnames of the input are not consistent")
      nc1 <- nc[,c("FL1-H")]
      #only write the channels of interest (reduce disk IO)
      suppressMessages(nc1[[sn]] <- fr_trans[,c("FL1-H")])
      trans_range <- apply(exprs(nc[[sn]]), 2, range)
      #transformed channel
      expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226)) 
      #untransformed channel
      expect_equal(trans_range[, c("FL2-H")], c(1.000, 1637.104), tol = 8e-08)
      
      #update chanel colnames
      suppressMessages(nc <- ncdfFlowSet(fs[sn]))
      colnames(fr_trans)[3:4] <- c("<FL1-H>", "<FL2-H>")
      #write data without matching up the colnames
      suppressMessages(nc[[sn, only.exprs = TRUE]] <- fr_trans)
      trans_range <- apply(exprs(nc[[sn]]), 2, range)
      expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226)) 
      expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))
      #colnames remain unchanged
      expect_equal(colnames(nc), colnames(cs))
      expect_error(nc[[sn]] <- fr_trans, "colnames of the input are not consistent")
      
    })

test_that("csApply", {
      sn <- samples[1]
      #use csApply when FUN returns a flowFrame
      translist <- transformList(c("FL1-H", "FL2-H"), lgcl)
      suppressMessages(nc1 <- csApply(cs, transform, translist))
      expect_is(nc1, "cytoSet")
      expect_equal(sampleNames(cs), sampleNames(nc1))
      expect_equal(colnames(cs), colnames(nc1))
      #the other channels remain the same
      # is_equal_flowSet(cs[, -c(3:4)], nc1[, -c(3:4)], description = FALSE)
      #tow channels are tranformed
      trans_range <- apply(exprs(nc1[[sn]]), 2, range)
      expect_equal(trans_range[, c("FL1-H")], c(0.6312576, 4.0774226)) 
      expect_equal(trans_range[, c("FL2-H")], c(0.6312576, 3.7131872))
      expect_false(fr_get_h5_file_path(get_cytoFrame_from_cs(nc1,sn)) == fr_get_h5_file_path(get_cytoFrame_from_cs(cs ,sn)))
      
    })    

# test_that("sampleNames<-", {
#       sn <- samples[1:2]
#       nc <- cs[sn]
#       newNames <- c("s1", "s2")
#       sampleNames(nc) <- newNames
#       expect_equal(sampleNames(nc), newNames)
#       expect_equal(nc@origSampleVector, c(newNames,samples[-c(1:2)]))
#       expect_equal(ls(nc@indices), newNames)
#       is_equal_flowFrame(cs[sn][[1]], nc[[1]])
#       
#       newNames <- c("s01", "s2")
#       sampleNames(nc) <- newNames
#       expect_equal(sampleNames(nc), newNames)
#       expect_equal(nc@origSampleVector, c(newNames,samples[-c(1:2)]))
#       expect_equal(ls(nc@indices), newNames)
#       is_equal_flowFrame(cs[sn][[1]], nc[[1]])
#       
#       newNames <- c("s2", "s2")
#       expect_error(sampleNames(nc) <- newNames, "Replacement values are not unique")
#       
#       #replace the single subsetted fs
#       nc <- nc["s2"]
#       sampleNames(nc) <- "dd"
#       expect_equal(sampleNames(nc), "dd")
#       expect_equal(nc@origSampleVector, c("s01","dd",samples[-c(1:2)]))
#       expect_equal(ls(nc@indices), "dd")
#       is_equal_flowFrame(cs[sn][[2]], nc[[1]])
#       
#       #replace with the name that is conflicting with values in origSampleVector
#       sampleNames(nc) <- "s01"
#       expect_equal(nc@origSampleVector[-1], c("s01",samples[-c(1:2)]))
#       is_equal_flowFrame(cs[sn][[2]], nc[[1]])
#       
#       })
# 
# test_that("colnames<-", {
#       sn <- samples[1:2]
#       coln <- colnames(cs)
#       
#       nc <- cs[sn, coln[1:2]]
#       newColNames <- c("c1", "c2")
#       colnames(nc) <- newColNames
#       expect_equal(colnames(nc), newColNames)
#       expect_equal(nc@origColnames, c(newColNames,coln[-c(1:2)]))
#       invisible(fsApply(nc, function(fr)expect_equal(colnames(fr), newColNames)))
#       is_equal_flowSet(cs[sn, coln[1:2]], nc)
#       expect_equivalent(unlist(keyword(nc[[1]])[c("$P1N", "$P2N")]), newColNames)
#       
#       #change the order of colnames
#       nc <- cs[sn, coln[2:1]]
#       colnames(nc) <- newColNames
#       expect_equal(nc@origColnames, c(newColNames[2:1],coln[-c(1:2)]))
#       is_equal_flowSet(cs[sn, coln[2:1]], nc)
#       expect_equivalent(unlist(keyword(nc[[1]])[c("$P1N", "$P2N")]), rev(newColNames))
#     })  
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
# test_that("clone.ncdfFlowSet", {
#   
#   nc1 <- cs[1:2]
#   ##clone the ncdfFlowSet object,by default the actual raw data is not added
#   nc2 <- clone.ncdfFlowSet(nc1,"clone.nc", isEmpty = TRUE)
#   expect_equal(nrow(nc2[[1]]), 0)
#   expect_equal(getFileName(nc2), "clone.nc")
#   
#   #add the actual raw data
#   suppressMessages(nc2[[1]] <- nc1[[1]])
#   is_equal_flowFrame(nc1[[1]], nc2[[1]])
#   
#   suppressMessages(nc2 <- clone.ncdfFlowSet(nc1, "clone.nc"))
#   is_equal_flowSet(nc1, nc2)
#   expect_equal(getFileName(nc2), "clone.nc")
#   expect_false(identical(nc2@frames, nc1@frames))
#   
#   unlink(nc2)
# })
