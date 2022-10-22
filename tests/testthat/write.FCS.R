context("write.FCS testing for cytoframe")
skip_if(win32_flag)
test_that("test write.FCS", {
  fcsfile <- system.file("extdata/CytoTrol_CytoTrol_1.fcs", package = "flowWorkspaceData")
  fr <- load_cytoframe_from_fcs(fcsfile)
  tmp <- tempfile()
  
  #change desc col
  pData(parameters(fr))[6, "desc"] <- "38"
  # Write to file
  tmp <- tempfile()
  write.FCS(fr,tmp)
  fr1 <- load_cytoframe_from_fcs(tmp)
  expect_equal(markernames(fr1)[2], c("R660-A" = "38"))
  
  # When I read the file back in, the SPILL matrix appears to be malformed.
  fr <- load_cytoframe_from_fcs(fcsfile)
  expect_equal(keyword(fr)[["transformation"]], "applied")
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["NHLBI"]], digest(fr)) #TODO:try to reproduce the check error once upgrade R to 3.5
  
  write.FCS(fr,tmp)
  fr1 <- load_cytoframe_from_fcs(tmp)
  keys <- keyword(fr)
  keys[["FILENAME"]] <- "setToDummy"
  keys[["$TOT"]] <- trimws(keys[["$TOT"]])
  keys[["PATIENT ID"]] <- trimws(keys[["PATIENT ID"]])
  keys[["SAMPLE ID"]] <- trimws(keys[["SAMPLE ID"]])
  keys[c("$BEGINDATA", "$ENDDATA", "GUID", "ORIGINALGUID")] <- NULL
  keys.new <- keyword(fr1)
  keys.new[["FILENAME"]] <- "setToDummy"
  expect_equal(keys.new[names(keys)], keys, tol = 6e-6)
  expect_equivalent(exprs(fr), exprs(fr1))
  
  #disable default linearize trans
  fr_notrans <- load_cytoframe_from_fcs(fcsfile, transformation = FALSE)
  expect_null(keyword(fr_notrans)[["transformation"]])
  #flowCore_$PnR and transformation keywords should be absent now
  #and there are should be no other difference in keywords between the two read
  missing.keys <- names(keys)[which(!names(keys) %in% names(keyword(fr_notrans)))]
  expect_equal(length(missing.keys), 25)
  expect_true(all(grepl("(flowCore_\\$P)|(transformation)", missing.keys)))
  #any the resulted write will produce no trans related keyword r
  suppressWarnings(write.FCS(fr_notrans,tmp))
  fr1 <- load_cytoframe_from_fcs(tmp, transformation = FALSE)
  missing.keys <- names(keys)[which(!names(keys) %in% names(keyword(fr1)))]
  expect_equal(length(missing.keys), 25)
  expect_true(all(grepl("(flowCore_\\$P)|(transformation)", missing.keys)))
  # when default linearize is enabled
  fr1 <- load_cytoframe_from_fcs(tmp)
  missing.keys <- names(keys)[which(!names(keys) %in% names(keyword(fr1)))]
  expect_equal(length(missing.keys), 0)
  
  #transform fr 
  fr.trans <- transform(fr_notrans, estimateLogicle(fr_notrans, markernames(fr_notrans)))
  expect_equal(keyword(fr.trans)[["transformation"]], "custom")
  #new keywords flowCore_$P* has been inserted
  missing.keys <- names(keys)[which(!names(keys) %in% names(keyword(fr.trans)))]
  expect_equal(length(missing.keys), 0)
  suppressWarnings(write.FCS(fr.trans,tmp))
  #these keywords remains even disable trans when read  it back
  fr1 <- read.FCS(tmp, transformation = FALSE)
  expect_equal(keyword(fr1)[["transformation"]], "custom")
  missing.keys <- names(keys)[which(!names(keys) %in% names(keyword(fr1)))]
  expect_equal(length(missing.keys), 0)
  #and transformation flag has no effect on read when it is already custom
  fr1 <- read.FCS(tmp)
  expect_equal(keyword(fr1)[["transformation"]], "custom")
  missing.keys <- names(keys)[which(!names(keys) %in% names(keyword(fr1)))]
  expect_equal(length(missing.keys), 0)
  
    # test delimiter(\) escaping 
  keyword(fr)[["$DATE"]] <- "05\\JUN\\2012"
  suppressWarnings(write.FCS(fr,tmp, delimiter = "\\"))
  fr1 <- load_cytoframe_from_fcs(tmp, emptyValue = F)
  keys.new <- keyword(fr1)
  keys.new[["FILENAME"]] <- "setToDummy"
  expect_equal(keys.new[["$DATE"]], "05\\JUN\\2012")
  keys.new[["$DATE"]] <- keys[["$DATE"]]
  expect_equal(keys.new[names(keys)], keys, tol = 6e-6)
  expect_equivalent(exprs(fr), exprs(fr1))

  # write it again to see if the existing double delimiter is handled properly
  suppressWarnings(write.FCS(fr1,tmp, delimiter = "\\"))
  fr1 <- load_cytoframe_from_fcs(tmp, emptyValue = F)
  keys.new <- keyword(fr1)
  keys.new[["FILENAME"]] <- "setToDummy"
  expect_equal(keys.new[["$DATE"]], "05\\JUN\\2012")
  keys.new[["$DATE"]] <- keys[["$DATE"]]
  expect_equal(keys.new[names(keys)], keys, tol = 6e-6)
  expect_equivalent(exprs(fr), exprs(fr1))

  #test other delimiter
  suppressWarnings(write.FCS(fr,tmp, delimiter = ";"))
  fr1 <- load_cytoframe_from_fcs(tmp, emptyValue = F)
  keys.new <- keyword(fr1)
  keys.new[["FILENAME"]] <- "setToDummy"
  expect_equal(keys.new[["$DATE"]], "05\\JUN\\2012")
  keys.new[["$DATE"]] <- keys[["$DATE"]]
  expect_equal(keys.new[names(keys)], keys, tol = 6e-6)
  expect_equivalent(exprs(fr), exprs(fr1))

  #test quadrual-delimiter string
  keyword(fr)[["$DATE"]] <- "05||JUN||2012"
  suppressWarnings(write.FCS(fr,tmp, delimiter = "|"))
  fr1 <- load_cytoframe_from_fcs(tmp, emptyValue = F)
  keys.new <- keyword(fr1)
  keys.new[["FILENAME"]] <- "setToDummy"
  expect_equal(keys.new[["$DATE"]], "05||JUN||2012")
  keys.new[["$DATE"]] <- keys[["$DATE"]]
  expect_equal(keys.new[names(keys)], keys, tol = 6e-6)
  expect_equivalent(exprs(fr), exprs(fr1))

  #TODO:not supported
  #when colmn.pattern is used to subset channels in read.FCS
  #make sure the id in $Pn is set properly in write.FCS
  # fr_sub <- read.FCS(fcsfile, column.pattern = '-A')
  # tmp <- tempfile()
  # suppressWarnings(write.FCS(fr_sub , filename = tmp))
  # fr1 <- read.FCS(tmp)
  # expect_equal(pData(parameters(fr_sub))[["name"]], pData(parameters(fr1))[["name"]], check.attributes = FALSE)
  # expect_equal(pData(parameters(fr_sub))[["desc"]], pData(parameters(fr1))[["desc"]], check.attributes = FALSE)
  # 
  
})
test_that("write.FCS -- subsetted flowframe", {
  tmpfile <- tempfile()
  f1 = flowFrame_to_cytoframe(GvHD[[2]])[,c(1:6,8)]
  write.FCS(f1, tmpfile)
  f2 <- load_cytoframe_from_fcs(tmpfile)
  expect_equal(nrow(f2),  3405)
  expect_equal(ncol(f2),  7)
  expect_equal(colnames(f2),  colnames(f1))
  expect_equal(markernames(f2),  markernames(f1))
  
  #add test since GvHD's range slot is not consistent with PnR thus can't be tested for range()
  fcsfile <- system.file("extdata/CytoTrol_CytoTrol_1.fcs", package = "flowWorkspaceData")
  f1 = load_cytoframe_from_fcs(fcsfile)
  f1 <- f1[,c(1:6,8)]
  
  write.FCS(f1, tmpfile)
  f2 <- load_cytoframe_from_fcs(tmpfile)
  expect_equal(nrow(f2),  nrow(f1))
  expect_equal(ncol(f2),  7)
  expect_equal(colnames(f2),  colnames(f1))
  expect_equal(markernames(f2),  markernames(f1))
  rng <- range(f1)
  # rng[2,] <- rng[2,] + 1
  expect_equal(range(f2),  rng)
  
})

test_that("write.FCS -- subsetted (by row) flowframe", {
  tmp <- flowFrame_to_cytoframe(GvHD[[1]])
  #subset by rows
  tmp <- tmp[sample(1:nrow(exprs(tmp)), 1000), ]
  expect_equal(keyword(tmp)[["$TOT"]], "3420")
  
  #write to fcs
  tmpfile <- tempfile()
  suppressWarnings(write.FCS(tmp,tmpfile))
  tmp <- load_cytoframe_from_fcs(tmpfile)
  expect_equal(keyword(tmp)[["$TOT"]], "1000")
  
  #read the text segment without parsing
  con <- file(tmpfile, open="rb")
  offsets <- flowCore:::readFCSheader(con)
  seek(con, offsets["textstart"])
  txt <- readBin(con,"raw", offsets["textend"]-offsets["textstart"]+1)
  txt <- iconv(rawToChar(txt), "", "latin1", sub="byte")
  #validity check on the keyword 
  #(make sure it is not duplicated since it may pass flowCore parser but fail the third-party software like flowJo)
  expect_equal(length(grep("\\$TOT", strsplit(txt, split = "\\|")[[1]])), 1)
  close(con)
  
  
})

test_that("write.FCS -- update channel and marker", {
  
  inputFcs <- flowFrame_to_cytoframe(GvHD[[1]])
  
  kwParName <- "$P3N"
  kwParLabel <- "$P3S"
  parName <- "FL1-H" 
  newName <- "newname"
  newLabel <- "newLabel"
  
  #update channel
  colnames(inputFcs)[which(colnames(inputFcs) == parName)] <- newName
  #update stain/marker
  names(newLabel) <- newName
  markernames(inputFcs) <- newLabel
  #see updated data
  colnames(inputFcs)
  markernames(inputFcs)
  #write fcs
  tmpfile <- tempfile()
  write.FCS(inputFcs, tmpfile)  
  tmp1 <- load_cytoframe_from_fcs(tmpfile)
  expect_equal(colnames(inputFcs), colnames(tmp1))
  expect_equal(markernames(inputFcs), markernames(tmp1))
})

test_that("write.FCS -- data from the flowFrame constructor without $PnR keys", {
  set.seed(1)
  mat <- matrix(rnorm(1000),ncol=4)
  colnames(mat) <- LETTERS[1:4]
  fr1 <- flowFrame(mat)
  fr1 <- flowFrame_to_cytoframe(fr1)
  keyword(fr1)
  tmp <- tempfile()
  write.FCS(fr1, tmp)
  fr2 <- load_cytoframe_from_fcs(tmp)
  expect_equal(as.numeric(keyword(fr2)[["$P1R"]]), 4, tolerance = 3e-4)
})

test_that("write.FCS -- add new cols", {
  tmp <- flowFrame_to_cytoframe(GvHD[[1]])

  kf <- kmeansFilter("FSC-H"=c("Pop1","Pop2","Pop3"), filterId="myKmFilter")
  fres <- filter(tmp, kf)
  cols <- as.integer(fres@subSet)
  cols <- matrix(as.numeric(cols), dimnames = list(NULL, "km"))
  tmp <- cf_append_cols(tmp, cols)

  tmpfile <- tempfile()
  write.FCS(tmp, tmpfile)
  tmp1 <- load_cytoframe_from_fcs(tmpfile)
  expect_equivalent(exprs(tmp), exprs(tmp1), tolerance = 3e-08)

  #set transformation flag and reload it to append flowCore_$PnRmax
  keyword(tmp)[["transformation"]] <- "none"
  write.FCS(tmp, tmpfile)
  tmp <- load_cytoframe_from_fcs(tmpfile)
  #append again to check whether it takes care of flowCore_$PnRmax
  keyword(tmp)[["transformation"]] <- "custom"
  colnames(cols) <- "km1"
  tmp <- cf_append_cols(tmp, cols)
  write.FCS(tmp, tmpfile)
  tmp1 <- load_cytoframe_from_fcs(tmpfile)
  expect_equal(as.numeric(keyword(tmp1)[["flowCore_$P10Rmax"]]), 3)
  expect_equivalent(exprs(tmp), exprs(tmp1), tolerance = 3e-08)
})

test_that("write.FCS -- reordered cols", {
  tmp <- flowFrame_to_cytoframe(GvHD[[1]])
  idx <- c(3,1,2)
  tmp <- tmp[, idx]

  tmpfile <- tempfile()
  write.FCS(tmp, tmpfile)
  tmp1 <- load_cytoframe_from_fcs(tmpfile)
  pd <- parameters(tmp)
  cn <- as.vector(pd[["name"]])
  expect_equal(rownames(parameters(tmp1)), paste0("$P", 1:3))
  #TODO
  expect_equivalent(keyword(tmp1)[paste0("BD$P", 1:3, "N")], keyword(tmp)[paste0("BD$P", idx, "N")])
  expect_equivalent(exprs(tmp), exprs(tmp1), tolerance = 3e-08)
})
test_that("write.FCS -- handle umlaut characters", {
  tmp <- flowFrame_to_cytoframe(GvHD[[1]])
  keyword(tmp)[["FILENAME"]] <- "ü_umlaut"
  tmpfile <- tempfile()
  write.FCS(tmp, tmpfile)  
  tmp1 <- load_cytoframe_from_fcs(tmpfile)
  expect_equivalent(exprs(tmp), exprs(tmp1), tolerance = 3e-08)
})

#added by Jake
test_that("write.FCS compatibility", {
  fr <- GvHD[[1]]
  cf <- flowFrame_to_cytoframe(fr)
  
  tmp_fr <- tempfile()
  write.FCS(fr, tmp_fr)
  tmp_cf <- tempfile()
  write.FCS(cf, tmp_cf)
  
  
  fr_from_fr <- read.FCS(tmp_fr)
  fr_from_cf <- read.FCS(tmp_cf)
  
  keys_fr <- keyword(fr_from_fr)
  keys_cf <- keyword(fr_from_cf)
  # keys_cf will have a few different keys (like cytolib version)
  # and will thus also slightly offset BEGINDATA and ENDDATA
  keys_to_compare <- names(keys_fr)
  keys_to_compare <- keys_to_compare[!(keys_to_compare %in% c("$BEGINDATA", "$ENDDATA", "FILENAME", "GUID","ORIGINALGUID"))]
  to_compare <- keys_cf[keys_to_compare]
  expect_equal(keys_fr[keys_to_compare], keys_cf[keys_to_compare])
  expect_equal(exprs(fr_from_fr), exprs(fr_from_cf))
  
  cf_from_fr <- load_cytoframe_from_fcs(tmp_fr)
  cf_from_cf <- load_cytoframe_from_fcs(tmp_cf)
  
  keys_fr <- keyword(cf_from_fr)
  keys_cf <- keyword(cf_from_cf)
  # keys_cf will have a few different keys (like cytolib version)
  # and will thus also slightly offset BEGINDATA and ENDDATA
  keys_to_compare <- names(keys_fr)
  keys_to_compare <- keys_to_compare[!(keys_to_compare %in% c("$BEGINDATA", "$ENDDATA", "FILENAME", "GUID","ORIGINALGUID"))]
  to_compare <- keys_cf[keys_to_compare]
  expect_equal(keys_fr[keys_to_compare], keys_cf[keys_to_compare])
  expect_equal(exprs(cf_from_fr), exprs(cf_from_cf))
})