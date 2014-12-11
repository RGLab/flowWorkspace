context("parse workspaces of various flowJo versions ")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite"

sink("/dev/null")
test_that("v 10.0.6 - vX 1.8",{
      
      thisPath <- file.path(path, "mssm")
      wsFile <- file.path(thisPath, "CFSP_Analysis14.wsp")
      
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = "Bcell", subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = "Bcell", subset = 1, isNcdf = TRUE)
      
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh, path = "full")[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
    })


test_that("v 10.0.7 - vX 20.0 (ellipsoidGate)",{
      
      thisPath <- file.path(path, "bioaster_ellipsoidGate")
      wsFile <- file.path(thisPath, "Matrice 1.wsp")
      
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = "Matrice", subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = "Matrice", subset = 1, isNcdf = TRUE)
      
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts, tol = 1e-3)
    })

#test_that("v 10.0.7 - vX 20.0 (McGill)",{
#      
#      thisPath <- file.path(path, "McGill/BMDCs")
#      wsFile <- file.path(thisPath, "20140124 BMDCs.1.wsp")
#      
#      ws <- openWorkspace(wsFile)
#      gs <- parseWorkspace(ws, name = 3, subset = 1, execute = FALSE)
#      expect_is(gs, "GatingSet")
#      gs <- parseWorkspace(ws, name = 3, subset = 1, isNcdf = TRUE)
#      
#      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
#      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
#      expect_equal(thisCounts, expectCounts)
#    })

test_that("v 10.0.7 - vX 20.0 (cytof no compensation)",{
      
      thisPath <- file.path(path, "CyTOF")
      wsFile <- file.path(thisPath, "cytof.wsp")
      
      ws <- openWorkspace(wsFile)
      
      gs <- parseWorkspace(ws, name = 1, path = file.path(path), execute = FALSE)
      
      expect_is(gs, "GatingSet")
#      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
#      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
#      expect_equal(thisCounts, expectCounts)
    })



test_that("v 7.6.5 - win 1.61 (PBMC)",{
      
      thisPath <- file.path(path, "PBMC/Blomberg")
      wsFile <- file.path(thisPath, "Exp2_Tcell.wsp")

      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 1, subset = 1, sampNloc = "sampleNode", execute = FALSE)
      expect_is(gs, "GatingSet")
      gs <- parseWorkspace(ws, name = 1, subset = 1, sampNloc = "sampleNode", isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
      
    })

test_that("v 7.6.5 - win 1.61 (sampNloc = 'sampleNode')",{
      
      thisPath <- file.path(path, "Cytotrol/Miami")
      wsFile <- file.path(thisPath, "flowJo/Cytotrol_061112_Tcell.wsp")

      ws <- openWorkspace(wsFile)
      
      gs <- parseWorkspace(ws, name = 1, subset = 1, path = file.path(thisPath,"Tcell"), sampNloc = "sampleNode", execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 1, subset = 1, path = file.path(thisPath,"Tcell"), sampNloc = "sampleNode", isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
      
    })


test_that("v 9.0.1 - mac 2.0 (HVTN RV144 -- options = 1)",{
      
      thisPath <- file.path(path, "HVTN/RV144")
      wsFile <- file.path(thisPath, "Batch 1264 RV144.xml")

      ws <- openWorkspace(wsFile, options = 1)
      gs <- parseWorkspace(ws, name = 4, subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 4, subset = 1, isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
      
    })

test_that("v 9.0.1 - mac 2.0 (HVTN 080-0880)",{
      
      thisPath <- file.path(path, "HVTN/080")
      wsFile <- file.path(thisPath, "080 batch 0880.xml")

      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 4, subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 4, subset = 1, isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
      
    })


test_that("v 9.2 - mac 2.0 (ITN029)",{
      
      thisPath <- file.path(path, "ITN029ST")
      wsFile <- file.path(thisPath, "QA_template.xml")

      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 2, subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 2, subset = 1, isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
      
    })
test_that("v 9.4.2 - mac 2.0",{
      
      thisPath <- file.path(path, "PBMC/HIPC_trial")
      wsFile <- file.path(thisPath, "data/HIPC_trial.xml")

      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 2, subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 2, subset = 1, isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts, tol = 35e-4)
      
    })


test_that("v 9.5.2 - mac 2.0",{
      
      thisPath <- file.path(path, "Cytotrol/NHLBI")
      wsFile <- file.path(thisPath, "flowJo/NHLBI.xml")

      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 2, subset = 1, path = file.path(thisPath,"Bcell"), execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 2, subset = 1, path = file.path(thisPath,"Bcell"), isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts, tol = 11e-3)
      
    })

test_that("v 9.6.3 - mac 2.0 (ignore highValue for FSC/SSC)",{
      
      thisPath <- file.path(path, "roch")
      wsFile <- file.path(thisPath, "PROP_20120118_TPHE.xml")

      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 1, subset = "Specimen_001_Tube_024.fcs", execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 1, subset = "Specimen_001_Tube_024.fcs", isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh, path = "full")[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts, tol = 8e-05)
      
    })

test_that("v 9.7.4 - mac 3.0",{
      thisPath <- file.path(path, "v9.7.4")
      wsFile <- file.path(thisPath, "T1 CHI-002v974.xml")

      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = "CHI-002 PBMC control", subset = "CHI-002 PBMC control_101211.fcs", execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = "CHI-002 PBMC control", subset = "CHI-002 PBMC control_101211.fcs", isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh, path = "full")[, list(flowJo.count,flowCore.count, node)]
      expectCounts[flowJo.count ==0, flowJo.count := -1]#fix the legacy counts
      expect_equivalent(thisCounts, expectCounts)
    })

test_that("v 9.7.5 - mac 3.0 (no compensation and using calibrationIndex)",{
      thisPath <- file.path(path, "Ragon")
      wsFile <- file.path(thisPath, "neut v non neut v9.xml")
      
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 5, subset = "477889_env_cct_norm_concatenated.txt", execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 5, subset = "477889_env_cct_norm_concatenated.txt", isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expectCounts[flowJo.count ==0, flowJo.count := -1] #fix the legacy counts
      expect_equivalent(thisCounts, expectCounts)
    })

test_that("v 9.7.5 - mac 3.0 (boolGate that refers to the non-sibling nodes)",{
      thisPath <- file.path(path, "094")
      wsFile <- file.path(thisPath, "1851-M-094.xml")
      
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 2, subset = "434713.fcs", execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = 2, subset = "434713.fcs", isNcdf = TRUE)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      
      expect_equal(thisCounts, expectCounts)
    })


sink()