context("workspace")
# resultDir <- "tests/testthat/expect_result/"
fjRes <- readRDS(file.path(resultDir, "flowJoWorkspace_expect.rds"))

test_that("show workspace",
    {
      thisRes <- capture.output(show(ws))[-(1:2)]
      expectRes <- fjRes[["ws_show"]][-(1:5)]
      expect_equal(thisRes, expectRes)
      
    })

test_that("getWorkspaceType",
    {
      
      expect_equal(.getWorkspaceType("2.0"), "macII")
      expect_equal(.getWorkspaceType("3.0"), "macIII")
      
      expect_equal(.getWorkspaceType("1.6"), "win")
      expect_equal(.getWorkspaceType("1.61"), "win")
      
      expect_equal(.getWorkspaceType("1.8"), "vX")
      expect_equal(.getWorkspaceType("20.0"), "vX")
      
      expect_error(.getWorkspaceType("2.1"), "Unsupported version")
      expect_error(.getWorkspaceType("3.1"), "Unsupported version")
      expect_error(.getWorkspaceType("1.81"), "Unsupported version")
      expect_error(.getWorkspaceType("20.01"), "Unsupported version")
      expect_error(.getWorkspaceType("1.63"), "Unsupported version")
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



test_that(".getSamples workspace",
    {
      thisRes <- getSamples(ws)
      thisExpect <- fjRes[[".getSamples"]]
      
      expect_equivalent(thisRes, thisExpect)
    })

test_that(".getSampleGroups workspace",
    {
      thisRes <- getSampleGroups(ws)
      thisExpect <- fjRes[[".getSampleGroups"]]
      expect_equivalent(thisRes, thisExpect)
    })
