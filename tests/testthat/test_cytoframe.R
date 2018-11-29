context("cytoframe accessors")
fcs_file <- list.files(dataDir, "Cyto", full.names = TRUE)[1]
fr <- read.FCS(fcs_file)
cf <- load_cytoframe_from_fcs(fcs_file, is_h5 = TRUE)
lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)

rectGate <- rectangleGate(filterId="nonDebris","FSC-H"=c(200,Inf))

test_that("Subset", {
      #Subset by gate
      is_equal_flowFrame(Subset(cf, rectGate), Subset(fr, rectGate))

    })
test_that("cytoSet_to_flowframe", {
  fr1 <- cytoFrame_to_flowFrame(cf)
  is_equal_flowFrame(cf, fr1)

})


test_that("get_h5_file_path", {
      h5file <- cf_get_h5_file_path(cf)
      expect_true(file.exists(h5file))
      
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
      # coln <- colnames(cf1)
      # cf2 <- cf1[, coln[2:1]]
      # colnames(cf2) <- newColNames
      # expect_equivalent(unlist(keyword(cf2)[c("$P1N", "$P2N")]), rev(newColNames))
    })
