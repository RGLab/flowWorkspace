context("workspace")

fjRes <- readRDS(file.path(resultDir, "flowJoWorkspace_expect.rds"))

test_that("show workspace",
    {
      expect_output(ws, fjRes[["ws_show"]][-2])
    })

test_that("summary workspace",
    {
      expect_output(summary(ws), fjRes[["ws_show"]])
    })

test_that("summary workspace",
    {
      expect_output(summary(ws), fjRes[["ws_show"]])
    })

test_that("getWorkspaceType",
    {
      version_support <- flowWorkspace.par.get("flowJo_versions")
      
      expect_equal(.getWorkspaceType("2.0"), "mac")
#      expect_equal(.getWorkspaceType("3.0"), "mac")
      
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
      expect_equal(getFileNames(ws), fjRes[["getFn_ws"]])
    })

test_that("getKeywordsBySampleID workspace",
    {
      expect_equal(.getKeywordsBySampleID(ws, sid = 1), fjRes[["getkwByID_ws"]])
      expect_equal(.getKeywordsBySampleID(ws, sid = 1, kw = "P8DISPLAY"), c(value = "LOG"))
    })

test_that("getKeywords workspace",
    {
      expect_equal(getKeywords(ws, "CytoTrol_CytoTrol_1.fcs"), fjRes[["getkw_ws"]])
    })

test_that(".getKeyword workspace",
    {
      expect_equal(.getKeyword(ws, "$FIL"), fjRes[[".getkw_ws"]])
    })

test_that(".getKeywords workspace",
    {
      expect_equal(.getKeywords(ws@doc, "CytoTrol_CytoTrol_1.fcs"), fjRes[[".getkws_ws"]])
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
      expect_equal(.getSamples(ws@doc), fjRes[[".getSamples"]])
      expect_equal(getSamples(ws), fjRes[[".getSamples"]])
    })

test_that(".getSampleGroups workspace",
    {
      expect_equal(.getSampleGroups(ws@doc), fjRes[[".getSampleGroups"]])
      expect_equal(getSampleGroups(ws), fjRes[[".getSampleGroups"]])
    })

test_that(".getCalibrationTableSearch workspace",
    {
      thisRes <- .getCalibrationTableSearch(ws@doc, term = "Treg")
      
      expect_is(thisRes, "list")
      expect_is(thisRes[[1]], "function")
      
      expect_equal(names(thisRes),  c("C2 Treg CYTlyo <B710-A>"
                                        , "C2 Treg CYTlyo <G560-A>"
                                        , "C2 Treg CYTlyo <G780-A>"
                                        , "C2 Treg CYTlyo <R660-A>"
                                        , "C2 Treg CYTlyo <R780-A>"
                                        , "C2 Treg CYTlyo <V450-A>"
                                        , "C2 Treg CYTlyo <V545-A>"
                                        )
                    )
                          
    })

test_that("getTransformations workspace",
    {
      thisRes <- getTransformations(ws)
      
      expect_is(thisRes, "list")
      expect_equal(names(thisRes),  c("InputParameterTransform_Gain1_Offset1262144"
                                        , "C2 T-cell CYTlyo"                           
                                        , "C2 B-cell CYTlyo"                           
                                        , "C2 DC CYTlyo"                               
                                        , "C2 Thelper CYTlyo"                          
                                        , "C2 Treg CYTlyo"
                                     )
                          )
      thisRes <- thisRes[[6]]                          
      
      expect_is(thisRes, "list")
      expect_is(thisRes[[1]], "function")
      
      expect_equal(names(thisRes),  c("C2 Treg CYTlyo <B710-A>"
                                    , "C2 Treg CYTlyo <G560-A>"
                                    , "C2 Treg CYTlyo <G780-A>"
                                    , "C2 Treg CYTlyo <R660-A>"
                                    , "C2 Treg CYTlyo <R780-A>"
                                    , "C2 Treg CYTlyo <V450-A>"
                                    , "C2 Treg CYTlyo <V545-A>"
                                )
                            )                    
    })


test_that(".getCalibrationTableNames workspace",
    {
      expect_equal(.getCalibrationTableNames(ws@doc), fjRes[[".getCalibrationTableNames"]])
    })

test_that(".getCalibrationTable workspace",
    {
      expect_equal(.getCalibrationTable(ws@doc, name = "C2 Treg CYTlyo <B710-A>"), fjRes[[".getCalibrationTable"]])
    })

test_that("getCompensationMatrices workspace",
    {
      expect_equal(getCompensationMatrices(ws), fjRes[["getCompensationMatrices"]])
      expect_equal(.getCompensationMatrices(ws@doc), fjRes[["getCompensationMatrices"]])
      
    })

test_that("getCompensationMatrices workspace",
    {
      expect_equal(getCompensationMatrices(ws), fjRes[["getCompensationMatrices"]])
      expect_equal(.getCompensationMatrices(ws@doc), fjRes[["getCompensationMatrices"]])
      
    })

