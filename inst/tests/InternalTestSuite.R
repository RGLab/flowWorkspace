context("parse workspaces of various flowJo versions ")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite"

sink("/dev/null")

test_that("curlyQuad gate1 ",{
      thisPath <- file.path(path, "curlyQuad/example1")
      wsFile <- file.path(thisPath, "20151208_TBNK_DS.xml")
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name=2)
      
      res <- getPopStats(gs[[1]])
      expect_equal(res[, flowJo.freq], res[, flowCore.freq], tol = 7e-3)
    })

test_that("curlyQuad gate ",{
      thisPath <- file.path(path, "curlyQuad/example2")
      wsFile <- file.path(thisPath, "20-Apr-2016.wsp")
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name=1)
      
      res <- getPopStats(gs[[1]])
      expect_equal(res[, flowJo.freq], res[, flowCore.freq], tol = 1.6e-2)
    })

test_that("EllipsoidGate defined on log-transformed channels ",{
      thisPath <- file.path(path, "ellipsoid_log")
      wsFile <- file.path(thisPath, "xml_spillover2.xml")
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name=1, execute = T, sampNloc = "sampleNode", subset = "spillover_B2.fcs")
        
      res <- getPopStats(gs[[1]])
      expect_equal(res[, flowJo.count], res[, flowCore.count], tol = 1.1e-3)
})

test_that("No gate extension ",{
      thisPath <- file.path(path, "negCoordinates")
      wsFile <- file.path(thisPath, "08-Mar-2016.wsp")
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 1, subset = 1) #default extend_val = 0 will extend the gate  
      res <- getPopStats(gs[[1]])
      expect_gt(res[11, abs(flowJo.freq - flowCore.freq)], 0.1)
      
      gs <- parseWorkspace(ws, name = 1, extend_val = -2e3)#relax the threshold to disable extension
      res <- getPopStats(gs[[1]])
      expect_equal(res[, flowJo.freq], res[, flowCore.freq], tol = 4e-3)
    })


test_that("Time gate ",{
  thisPath <- file.path(path, "flin")
  wsFile <- file.path(thisPath, "A01.wsp")
  ws <- openWorkspace(wsFile)
  gs <- parseWorkspace(ws, name = 1, subset = 1)
  res <- getPopStats(gs[[1]])
  expect_equal(res[, flowJo.freq], res[, flowCore.freq], tol = 9e-4)
})
test_that("Time gate2--when computed timestep is very different from $TIMESTEP ",{
      thisPath <- file.path(path, "timegate")
      wsFile <- file.path(thisPath, "MX1 Analysis VISC.xml")
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws,name="Group 1",subset=11)
      res <- getPopStats(gs[[1]])[flowJo.count!=-1,]
      expect_equal(res[, flowJo.freq], res[, flowCore.freq], tol = 8e-3)
    })
test_that("Inverse function of flog ",{
      thisPath <- file.path(path, "inverse")
      wsFile <- file.path(thisPath, "Small.xml")
      ws <- openWorkspace(wsFile)
      
      gs <- parseWorkspace(ws, name=1, emptyValue=FALSE)
      
      gh <- gs[[1]]
      thisCounts <- getPopStats(gs)
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      expect_equal(thisCounts, expectCounts)
      
      
      trans <- getTransformations(gh)
      inverse <- getTransformations(gh, inverse = T)
      raw <- c(1, 1e2, 1e3,1e5)
      log <- trans[[1]](raw)
      expect_equal(inverse[[1]](log), raw)
      
    })

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
      
      #test double delimiter issue
      fcsname <- pData(gs)[["name"]]
      fr <- read.FCS(file.path(thisPath, fcsname))
      tmp <- tempdir()
      suppressWarnings(write.FCS(fr, filename = file.path(tmp, fcsname), delimiter = "/"))
      
      expect_error(gs <- parseWorkspace(ws, name = "Bcell", subset = 1, isNcdf = F, path = tmp)
                  , "Empty keyword name") #flowSet
      expect_error(gs <- parseWorkspace(ws, name = "Bcell", subset = 1, isNcdf = T, path = tmp)
          , "Empty keyword name")#ncdfFlowSet
              
      gs <- parseWorkspace(ws, name = "Bcell", subset = 1, isNcdf = T, path = tmp, emptyValue = F)#ncdf
      gh <- gs[[1]]
      thisCounts <- getPopStats(gh, path = "full")[, list(flowJo.count,flowCore.count, node)]            
      expect_equal(thisCounts, expectCounts)
      
      gs <- parseWorkspace(ws, name = "Bcell", subset = 1, isNcdf = F, path = tmp, emptyValue = F)#flowSet
      gh <- gs[[1]]
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

test_that("v 10.0.7 - vX 20.0 (missing_namespace and flin)",{
      
      thisPath <- file.path(path, "missing_namespace")
      wsFile <- file.path(thisPath, "BM_data.xml")
      
      ws <- openWorkspace(wsFile)
      expect_error(gs <- parseWorkspace(ws, name = 1, subset = 1, execute = FALSE)
                    , "*: unknown tranformation type!transforms:linear")
      
      
      wsFile <- file.path(thisPath, "BM_data_corrected.xml")
      
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 1, subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      gh <- gs[[1]]
      trans <- getTransformations(gh, only = F)
      expect_equal(trans[[2]][["name"]], "flowJo_flog")
    })

# invalid xml with Namespace prefix defintion missing #TODO: try to be robust on this kind of xml error
#test_that("v 10.0.7 - vX 20.0 (McGill/BMDCs) linear transformation",{
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

test_that("v 10.0.7 - vX 20.0 (McGill/treg) ellipseidGate (biexponential)",{
      
      thisPath <- file.path(path, "McGill/Treg")
      wsFile <- file.path(thisPath, "20131206_Treg.1.ellipseidGate.wsp")
      
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 3, subset = 4, execute = FALSE)
      expect_is(gs, "GatingSet")
      g <- getGate(gs[[1]], "CD4Ellipse")
      #transformed ellipse Gate
      expect_is(g, "polygonGate")
      expect_equal(range(g@boundaries[, "Comp-APC-A"]), c(142.8034, 208.1966), tol = 1e-6)
      expect_equal(range(g@boundaries[, "SSC-A"]), c(9884.187, 58723.813), tol = 1e-6)
      
      #skip gate transform
      gs <- parseWorkspace(ws, name = 3, subset = 4, execute = FALSE, transform = FALSE)
      g <- getGate(gs[[1]], "CD4Ellipse")
      expect_is(g, "polygonGate")
      #ellipsoidGate should be in 256 * 256 scale
      expect_equal(range(g@boundaries[, "Comp-APC-A"]), c(142.8034, 208.1966), tol = 1e-6)
      expect_equal(range(g@boundaries[, "SSC-A"]), c(9.652527, 57.347473), tol = 1e-6)
      
      gs <- parseWorkspace(ws, name = 3, subset = 4)
      
      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)
      expect_equal(thisCounts[,flowJo.freq], thisCounts[,flowCore.freq], tol = 8e-4)
    })

test_that("v 10.0.7 - vX 20.0 (PROVIDE/CyTOF) ellipseidGate (fasinh)",{
      
      thisPath <- file.path(path, "PROVIDE")
      wsFile <- file.path(thisPath, "batch1 local and week 53.wsp")
      
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 1, subset = 3, execute = FALSE, sampNloc = "sampleNode")
      expect_is(gs, "GatingSet")
      
      #won't find the file if $TOT is taken into account(most likely the data provided was wrong)
      expect_warning(expect_error(gs <- parseWorkspace(ws, name = 1, subset = 3, sampNloc = "sampleNode")
                   , "no sample to be added to GatingSet!")
               , "Can't find the FCS")
      
      #relax the rules (shouldn't be doing this, just for the sake of testing)
      gs <- parseWorkspace(ws, name = 1, subset = 3, sampNloc = "sampleNode", additional.keys = NULL)
      
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
    })

test_that("v 10.0.7 - vX 20.0 (cytof no compensation)",{
      
      thisPath <- file.path(path, "CyTOF")
      wsFile <- file.path(thisPath, "cytof.wsp")
      
      ws <- openWorkspace(wsFile)
      
      expect_warning(gs <- parseWorkspace(ws, name = 1, path = file.path(path), execute = FALSE), "different gating tree structures")
      
      expect_is(gs, "GatingSet")
#      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
#      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
#      expect_equal(thisCounts, expectCounts)
    })

test_that("v 10.0.7r2 - vX 20.0 (NotNode)",{
      
      thisPath <- file.path(path, "combineNode/NotNode")
      wsFile <- file.path(thisPath, "WSwithNotNodePopulation.wsp")
      
      ws <- openWorkspace(wsFile)
      
      gs <- parseWorkspace(ws, name = 1, path = file.path(path), execute = FALSE)
      
      expect_is(gs, "GatingSet")
      gh <- gs[[1]]
      g <- getGate(gh, "CD20+⁻")
      expect_is(g, "booleanFilter")
      expect_equal(g@deparse, "!LIVE/Single Cells/CD45+/CD20+")
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts[,c("flowJo.count", "node"), with = F], expectCounts[,c("flowJo.count", "node"), with = F])
    })

test_that("v 10.0.8r1 - vX 20.0 (OrNode)",{
      
      thisPath <- file.path(path, "combineNode/OrNode")
      wsFile <- file.path(thisPath, "Test_EW.wsp")
      
      ws <- openWorkspace(wsFile)
      
      gs <- parseWorkspace(ws, name = 1, path = file.path(path))
      
      expect_is(gs, "GatingSet")
      gh <- gs[[1]]
      g <- getGate(gh, "CD44+")
      expect_is(g, "booleanFilter")
      expect_equal(g@deparse, "FCS singlets/SSC singlets/Lymphocytes/CD8/F5/Live/Q6: CD44+ , CD62L+|FCS singlets/SSC singlets/Lymphocytes/CD8/F5/Live/Q7: CD44+ , CD62L-")
      
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
    })


test_that("v 10.0.8 - vX 20.0 (slash_issue_vX)",{
      thisPath <- file.path(path, "slash_issue_vX")
      wsFile <- file.path(thisPath, "IFEP004.wsp")
      
      ws <- openWorkspace(wsFile)
      
      gs <- parseWorkspace(ws, name = 5, path = file.path(thisPath), execute = T)
      
      expect_is(gs, "GatingSet")
      gh <- gs[[1]]
#      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)
      expect_equal(thisCounts[, flowJo.count], thisCounts[, flowCore.count], tol = 3e-3)
      
    })
test_that("v 7.6.1- win 1.6 (use default biexp trans when channel-specific trans not found within its respective trans group )",{
      
      thisPath <- file.path(path, "GYO")
      wsFile <- file.path(thisPath, "whole blood GYO-0109 050214.wsp")
      ws <- openWorkspace(wsFile)
      
      gs <- parseWorkspace(ws, name = 2, subset = 1, path = thisPath,  execute = FALSE)
      expect_is(gs, "GatingSet")
      
      expect_warning(gs <- parseWorkspace(ws, name = 2, path = thisPath), "Can't find the FCS")
      gh <- gs[[1]]
      
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
      
    })

test_that("v 7.6.5 - win 1.61 (PBMC)",{
      
      thisPath <- file.path(path, "PBMC/Blomberg")
      wsFile <- file.path(thisPath, "Exp2_Tcell.wsp")

      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = 1, subset = 1, sampNloc = "sampleNode", execute = FALSE)
      expect_is(gs, "GatingSet")
      gs <- parseWorkspace(ws, name = 1, subset = 1, sampNloc = "sampleNode")
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
      suppressWarnings(expectCounts <- fread(file.path(thisPath, "expectCounts.csv")))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
      
    })


test_that("v 9.0.1 - mac 2.0 (HVTN RV144 -- options = 1)",{
      
      thisPath <- file.path(path, "HVTN/RV144")
      wsFile <- file.path(thisPath, "Batch 1264 RV144.xml")

      ws <- openWorkspace(wsFile, options = 1)
      
      #not sure how to suppress the long stacks of C messages (XML package)
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

#1. boolean specification contains double spaces and non-spaces, which leads to the fix that uses boolean operator as delimiter instead of space)
#2. boolean gate refers to the node (AM) that appears both at sibling and children level, which leads to further checking in getRefNodes routines
#3. boolean gate has quotedString which leads to more generic xpath searching for gatePath and trailing space removal.
test_that("v 9.4.4 - mac 2.0 ",{
      
      thisPath <- file.path(path, "JJ")
      wsFile <- file.path(thisPath, "JJ_FlowJo_.xml")
      
      ws <- openWorkspace(wsFile)
      gs <- parseWorkspace(ws, name = "Test", subset = 1, execute = FALSE)
      expect_is(gs, "GatingSet")
      
      gs <- parseWorkspace(ws, name = "Test", subset = 1)
      gh <- gs[[1]]
      expectCounts <- fread(file.path(thisPath, "expectCounts.csv"))      
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts)
      
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
      
      #create a temp folder and symlink to original files to test the feature of searching sample by keyword $FIL
      # in the use case where the fcs has been modified
      fcs <- list.files(pattern = "fcs", file.path(thisPath, "Bcell"), full = T)[[1]]
      tmp <- tempfile()
      dir.create(tmp)
      newFCS <- file.path(tmp, "test.fcs")
      file.symlink(fcs, newFCS)
      
      gs <- parseWorkspace(ws, name = 2, subset = 1, path = tmp)
      gh <- gs[[1]]
      thisCounts <- getPopStats(gh)[, list(flowJo.count,flowCore.count, node)]
      expect_equal(thisCounts, expectCounts, tol = 11e-3)
      unlink(tmp,recursive = T)
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
