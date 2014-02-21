context("workspace")

fjRes <- readRDS(file.path(resultDir, "flowJoWorkspace_expect.rds"))

test_that("show workspace",
    {
      expect_output(ws, fjRes[["ws_show"]][-2])
    })

test_that("summary workspace",
    {
      expect_output(summary(ws), fjRes[["ws_show"]][-2])
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

test_that("getFileNames workspace",
    {
      expect_equal(.getFileNames(ws@doc, wsType = "macII"), fjRes[["getFn_ws"]])
      expect_equal(getFileNames(ws), fjRes[["getFn_ws"]])
      
    })

test_that("getKeywordsBySampleID workspace",
    {
      expect_equal(.getKeywordsBySampleID(ws, sid = 1, samplePath = "/Workspace/SampleList/Sample"), fjRes[["getkwByID_ws"]])
      expect_equal(.getKeywordsBySampleID(ws, sid = 1, kw = "P8DISPLAY", samplePath = "/Workspace/SampleList/Sample"), c(value = "LOG"))
    })

test_that("getKeywords workspace",
    {
      expect_equal(getKeywords(ws, "CytoTrol_CytoTrol_1.fcs"), fjRes[["getkw_ws"]])
      expect_equal(.getKeywords(ws@doc, "CytoTrol_CytoTrol_1.fcs", samplePath = "/Workspace/SampleList/Sample"), fjRes[["getkw_ws"]])
    })

test_that(".getKeyword workspace",
    {
      expect_equal(.getKeyword(ws, "$FIL", samplePath = "/Workspace/SampleList/Sample"), fjRes[[".getkw_ws"]])
    })


test_that("getFJWSubsetIndices workspace",
    {
      expect_equal(getFJWSubsetIndices(ws, group = 2, requiregates = TRUE), fjRes[["getFJWSubsetIndices_2"]])
      expect_equal(getFJWSubsetIndices(ws, group = 4, requiregates = TRUE), fjRes[["getFJWSubsetIndices_4"]])
      
      expect_equal(getFJWSubsetIndices(ws, group = 4
                                      , key = "TUBE NAME"
                                      , value = "CytoTrol"
                                      , requiregates = TRUE
                                      )
                  , integer(0))
              
    })

test_that(".getSamples workspace",
    {
      expect_equal(.getSamples(ws@doc, wsType = "macII"), fjRes[[".getSamples"]])
      expect_equal(getSamples(ws), fjRes[[".getSamples"]])
    })

test_that(".getSampleGroups workspace",
    {
      expect_equal(.getSampleGroups(ws@doc, wsType = "macII"), fjRes[[".getSampleGroups"]])
      expect_equal(getSampleGroups(ws), fjRes[[".getSampleGroups"]])
    })
