context("Functional Tests Related to Loading/Saving cytoframes from FCS")
# adapted from flowCore IO-testSuite.R
library(digest)
dataPath <- "~/rglab/workspace/flowCore/misc/"
expectRes <- readRDS("~/rglab/workspace/flowCore/tests/testthat/expectResults.rds")
expectRes.new <- readRDS("~/rglab/workspace/flowCore/tests/testthat/expectRes.new.rds")

# NOTE: These tests are adapted for load_cytoframe_from_fcs() with as few changes as possible from the I/O tests for
# read.FCS() from flowCore's IO-testSuite.R. Where a test was changed due to expected changes in behavior
# between cytoframe and flowFrame or between load_cytoframe_from_fcs() and read.FCS(), those were indicated
# with "CHANGE". Many tests will require further adaptation (particularly those relying on digest()), so
# this is a work in progress. All comments (and commented-out lines) were left intact. 

# saveRDS(expectRes.new, file = "~/rglab/workspace/flowCore/tests/testthat/expectRes.new.rds")

# test_that("Miltenyi's Macsquantify", {
#   expect_warning(
#     fr <- read.FCS(file.path(dataPath, "Miltenyi/1696_12017-04-30.0001_compatible.fcs"))
#     , "Missing the required")
#   expect_is(fr, "flowFrame")
#   
#   range(fr, type = "data")
#   range(fr)
#   expect_warning(fr <- read.FCS(file.path(dataPath, "double_precision/wishbone_thymus_panel1_rep1.fcs")), "Missing the required")
#   
#   expect_equal(nrow(fr), 250170)
#   
# })
# expectRes.new <- list()
test_that("big file", {
  #too slow to run
  # fr <- read.FCS(file.path(dataPath, "gigantic_file.fcs"), column.pattern = "FSC*")
  # CHANGE -- cytoset errors out for this, while trunk does not
  fr <- expect_error(load_cytoframe_from_fcs(file.path(dataPath, "gigantic_file.fcs")
                 , which.lines = c(1,1e9)
                 , column.pattern = "FSC*"), "which.lines exceeds the data boundary")
  # expect_equal(nrow(fr), 1)
  fr <- load_cytoframe_from_fcs(file.path(dataPath, "gigantic_file.fcs"), which.lines = 1:1e3)
  expect_equal(nrow(fr), 1e3)
  fr <- load_cytoframe_from_fcs(file.path(dataPath, "gigantic_file.fcs"), which.lines = (1e3+1):2e3)
  expect_equal(nrow(fr), 1e3)
  
})

# CHANGE: warnings are just console output for now. So all "expect_warning" calls are now "expect_output".
test_that("DATATYPE:'D'", {
  expect_output(fr <- load_cytoframe_from_fcs(file.path(dataPath, "double_precision/wishbone_myleoid_monocyte.fcs")), "warning:Missing the required")
  expect_is(fr, "cytoframe")
  filename  <- "wishbone_thymus_panel1_rep1.fcs"
  msg <- capture_output(fr <- load_cytoframe_from_fcs(file.path(dataPath, "double_precision", filename)))
  expect_match(msg,  "Missing the required")
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))
  expect_equal(nrow(fr), 250170)
  
})

test_that("multi data segment", {
  expect_output(fr <- load_cytoframe_from_fcs(file.path(dataPath, "multi-datasegment.fcs")), "additional data segment")
  expect_is(fr, "cytoframe")
  
  expect_equal(nrow(fr), 1244)
  filename  <- "multi-datasegment.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename), dataset = 1)
  expect_equal(nrow(fr), 1244)
  
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename), dataset = 10)
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))
  expect_equal(nrow(fr), 955)
  
})


test_that("FCS with both SPILL and $SPILLOVER present", {
  
  filename <- "example-spill-spillover.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))
  expect_equal(keyword(fr)[["SPILL"]], keyword(fr)[["$SPILLOVER"]])
  # CHANGE: spillover() now will allow direct specfication of keyword, but only returns single matrix
  tmp1 <- spillover(fr)
  tmp2 <- spillover(fr, "$SPILLOVER")
  expect_equal(tmp1, tmp2)
  tmp <- tempfile()
  write.FCS(fr, filename = tmp)
  fr <- load_cytoframe_from_fcs(tmp)
  expect_equal(keyword(fr)[["SPILL"]], keyword(fr)[["$SPILLOVER"]])
  
})

test_that("test uint_64 + diverse bitwidths + missing $NEXTDATA: '*' ", {
  filename <- "uint_64.lxb"
  # CHANGE: This warning is not implemented in load_cytoframe_from_fcs as it is in read.FCS. 
  # If '$PnE' not found, just defaults to '0,0' silently. Leaving the check in for now, but failure is expected.
  expect_output(fr <- load_cytoframe_from_fcs(file.path(dataPath, filename)), "No '$PnE' keyword")
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))

  
})

#TODO: investigate why the results is no longer consistent with the archived summary
test_that("mixed endian", {
  filename <- "mixedEndian.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  expect_is(fr, "cytoframe")
  # expect_equal(summary(fr), expectRes[["read.FCS"]][["mixedEndian"]])
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  
})

# 'FILENAME' keyword may change when file path is changed 
# so we hard code it to make the comparsion consistent in case the file is moved
test_that("test special delimiter character: '*' ", {
  filename <- "multi_data_segment.LMD"
  expect_output(fr <- load_cytoframe_from_fcs(file.path(dataPath, filename)), "additional data segment")
  expect_equal(summary(fr), expectRes[["read.FCS"]][["multi_data_segment"]], tolerance = 0.08)
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  
})

test_that("test special delimiter character: '*' ", {
  filename <- "specialDelimiter.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  expect_equal(summary(fr), expectRes[["read.FCS"]][["specialDelimiter"]], tolerance = 0.001)
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  
})

test_that("test flowJo exported data with missing some of PnR keywords ", {
  expect_output(expect_error(fr <- load_cytoframe_from_fcs(file.path(dataPath, "missing_PnR_flowJoExport.fcs"))
                              , "not contained"), "warning:Missing the required \\$BEGINDATA keyword")
  
})

test_that("test in consistent datastart between header and TEXT", {
  expect_output(expect_error(fr <- load_cytoframe_from_fcs(file.path(dataPath, "Accuri-C6", "Accuri - C6 - A02 Spherotech 8 Peak Beads.fcs"), emptyValue = FALSE)
                             , "HEADER and the TEXT")
                , "uneven number of tokens")
  filename <- "Accuri - C6 - A02 Spherotech 8 Peak Beads.fcs"
  msg <- capture_output(fr <- load_cytoframe_from_fcs(file.path(dataPath, "Accuri-C6", filename), emptyValue = FALSE, ignore.text.offset = TRUE))
  expect_match(msg, "HEADER and the TEXT")
  expect_match(msg, "uneven number of tokens")
  
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  
  expect_equal(nrow(fr), 60661)
  expect_equal(summary(fr), expectRes[["read.FCS"]][["Accuri-C6"]], tolerance = 0.001)
  
  msg <- capture_output(fs <- load_cytoset_from_fcs(file.path(dataPath, "Accuri-C6", filename), emptyValue = FALSE, ignore.text.offset = TRUE))
  expect_match(msg, "HEADER and the TEXT")
  expect_match(msg, "uneven number of tokens")
  expect_equal(nrow(fs[[1]]), 60661)
  
})

# CHANGE: C parser errors on odd bitwidth, so disable tests subsequent to parse attempt.
test_that("test odd-bitwidth FCS", {
  filename <- "Sample 2.fcs"
  expect_error(fr <- load_cytoframe_from_fcs(file.path(dataPath, filename)), "Sorry, C parser doesn't support odd bitwidth!")
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  # keyword(fr)[["FILENAME"]] <- "setToDummy" 
  # expect_equal(expectRes[["read.FCS"]][["Sample2"]], digest(fr))
  
  filename <- "11ColorSmall.fcs"
  expect_error(fr <- load_cytoframe_from_fcs(file.path(dataPath, "oddbitwith", filename)), "Sorry, C parser doesn't support odd bitwidth!")
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  # keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["11ColorSmall"]], summary(fr))
  
  #too slow to test
  #      fr <- read.FCS(file.path(dataPath, "oddbitwith/11ColorForOthers.fcs"))
  #      keyword(fr)[["FILENAME"]] <- "setToDummy"
  #      expect_equal(expectRes[["read.FCS"]][["11ColorForOthers"]], digest(fr))
  
  #      fr <- read.FCS("file.path(dataPath, "oddbitwith/11ColorFull.fcs"))
  #      keyword(fr)[["FILENAME"]] <- "setToDummy"
  #      expect_equal(expectRes[["read.FCS"]][["11ColorFull"]], digest(fr))
})

# CHANGE: Due to the differing structure of cytoframe vs flowFrame, we shouldn't expect these to hash the same.
# So for now just disable all digest() checks and add simple "expect_is(obj ,cytoframe)" checks for completion.
test_that("test other FCS", {
  filename <- "20110125240_F06_I025.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["ITN029ST"]], digest(fr))
  
  fr <- load_cytoframe_from_fcs(list.files(system.file("extdata", package="flowCore"),full=T)[1])
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["flowCore"]], digest(fr))
  
  filename <- "Blank.FCS"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  expect_equal(expectRes[["read.FCS"]][["Blank"]], summary(fr))
  
  filename <- "Bendall et al Cell Sample A_basal.fcs"
  expect_output(fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
                , "dropped")
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["Bendall"]], digest(fr))              
})

test_that("test delimiter issue", {
  filename <- "GFP_2Kfold_011911_noKan_QA-1.fcs"
  expect_error(load_cytoframe_from_fcs(file.path(dataPath, filename))
               , "Empty keyword name detected")
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename), emptyValue = F)
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  expect_equal(expectRes[["read.FCS"]][["GFP2Kfold"]], summary(fr))
  expect_match(keyword(fr)[["GTI$WORKLIST"]], "C:/Document")
  
  filename <- "RAINBOW_OK.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["RAINBOW"]], digest(fr))
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename), emptyValue = F)
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["RAINBOWEmptyValue"]], digest(fr))               
  
  #\ as delimiter  with empty value
  filename <- "sample_1071.001"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["sample1071"]], digest(fr))
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename),emptyValue=F)
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["sample1071.double"]], digest(fr))
})

# latest R no longer permit overflowed coersion by as.integer
test_that("test Beckman_Coulter_XDP issue", {
  
  frList <- lapply(list.files(file.path(dataPath, "Beckman_Coulter/Beckman_Coulter_XDP/"),full=T)
                   , function(thisFile){
                     fr <- load_cytoframe_from_fcs(thisFile)
                     keyword(fr)[["FILENAME"]] <- "setToDummy"
                     expect_is(fr, "cytoframe") # Simple check of parse completion until better checks put in place
                   })
  # expect_equal(expectRes[["read.FCS"]][["BeckmanCoulterXDP"]], digest(frList))
  
})

test_that("test Beckman_Coulter $SPILLOVER keyword", {
  frList <- lapply(list.files(file.path(dataPath, "Beckman_Coulter"),full=T, pattern = ".fcs")
                   , function(thisFile){
                     fr <- load_cytoframe_from_fcs(thisFile)
                     keyword(fr)[["FILENAME"]] <- "setToDummy"
                     expect_is(fr, "cytoframe") # Simple check of parse completion until better checks put in place
                   })
  # expect_equal(expectRes[["read.FCS"]][["BeckmanCoulterSPILLOVER"]], digest(frList))
  
})

test_that("test U mode", {
  expect_error(load_cytoframe_from_fcs(file.path(dataPath, "PartecPAI/A0006980.FCS"))
               , "MODE U")
})

test_that("test pre-gated data", {
  filename <- "HC002_Col1_P3.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["HC002_Col1_P3"]], digest(fr))
  expect_is(fr, "cytoframe") # Simple check of parse completion until better checks put in place
  
  filename <- "HC002_Col1.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["HC002_Col1"]], digest(fr))
  expect_is(fr, "cytoframe") # Simple check of parse completion until better checks put in place
})

test_that("test flowJo exported data with offset = 99999999 and  missing the $BEGINDATA and $ENDDATA keywords ", {
  filename <- "badFlowJoExport.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["badFlowJoExport"]], digest(fr))
  expect_is(fr, "cytoframe") # Simple check of parse completion until better checks put in place
})

test_that("test integer overflow issue", {
  filename <- "intOverFlow.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["intOverFlow"]], digest(fr))
  expect_is(fr, "cytoframe") # Simple check of parse completion until better checks put in place
  
  filename <- "MoFlo Astrios EQ 9C bis all.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath,"/Beckman_Coulter", filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["MoFlo EQ 9C"]],  digest(fr))
  expect_is(fr, "cytoframe") # Simple check of parse completion until better checks put in place
})

test_that("test diverse Bitwidths", {
  filename <- "diverseBitwidths.fcs"
  fr <- load_cytoframe_from_fcs(file.path(dataPath, filename))
  #expectRes.new[[filename]] <<- list(ncol = ncol(fr), nrow = nrow(fr), chnl = colnames(fr), marker = markernames(fr), range = range(fr), range_data= range(fr, "data"), colmean = colMeans(exprs(fr)))  
  keyword(fr)[["FILENAME"]] <- "setToDummy"
  # expect_equal(expectRes[["read.FCS"]][["diverseBitwidths"]], digest(fr))
  expect_is(fr, "cytoframe") # Simple check of parse completion until better checks put in place
})

test_that("handle > 2^32-1 bytes", {
  fr <- flowFrame(matrix(data = rnorm(3e8), nrow = 1e8, ncol =3, dimnames = list(NULL, c("A", "B", "C"))))
  expect_gt(object.size(exprs(fr)), 2^31-1)
  tmp <- tempfile()
  write.FCS(fr, tmp, what = "double")
  set.seed(1)
  lines <- sort(sample(1:1e8, 1e3))
  fr1 <- load_cytoframe_from_fcs(tmp, which.lines = lines)
  expect_equivalent(exprs(fr)[lines,], exprs(fr1))
})