context("workspace")
# resultDir <- "tests/testthat/expect_result/"
fjRes <- readRDS(file.path(resultDir, "flowJoWorkspace_expect.rds"))

test_that("show workspace",
    {
      thisRes <- capture.output(show(ws))[-(1:2)]
      expectRes <- fjRes[["ws_show"]][-(1:5)]
      # expectRes[3] <- sub("45", "35", expectRes[3])#now we getSampleGroups also include the samples with 0 populations
      expect_equal(thisRes, expectRes)
      
    })


test_that("getKeywordsBySampleID workspace",
    {
      thisExpectRes <- fjRes[["getkwByID_ws"]]
      thisExpectRes <- lapply(fjRes[["getkwByID_ws"]], function(kw)trimws(kw[["value"]]))
      names(thisExpectRes) <- lapply(fjRes[["getkwByID_ws"]], "[[", "name")
      
      expect_equal(getKeywords(ws, 1), thisExpectRes)
      
    })

test_that("getKeywords workspace",
    {
      expect_error(getKeywords(ws, "CytoTrol_CytoTrol_1.fcs"), "Multiple sample nodes found")
      thisExpectRes <- lapply(fjRes[["getkw_ws"]], trimws)
      expect_equal(getKeywords(ws, 1), thisExpectRes)
    })



test_that("getSamples&getSampleGroups workspace",
    {
      thisRes <- getSamples(ws)
      thisExpect <- fjRes[[".getSamples"]]
      #record the rows to be removed
      excludeIds <- as.integer(rownames(subset(thisExpect, pop.counts <=0)))
      thisExpect <- thisExpect[-excludeIds,-4]
      expect_equivalent(thisRes, thisExpect)
      
      
      thisRes <- getSampleGroups(ws)
      thisExpect <- fjRes[[".getSampleGroups"]]
      # thisExpect <- thisExpect[-excludeIds, ]#now we getSampleGroups also include the samples with 0 populations
      expect_equivalent(thisRes, thisExpect)
    })


