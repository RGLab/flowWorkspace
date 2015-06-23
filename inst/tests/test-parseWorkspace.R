context("parseWorkspace")

wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)

ws <- openWorkspace(wsfile);
test_that("can load xml workspace",
{
  
  expect_that(ws, is_a("flowJoWorkspace"))
})

source("flowJoWorkspace-testSuite.R", local = TRUE)


gs <- NULL

test_that("Can parse workspace",{
    dd <- capture.output(suppressMessages(gs <<- try(parseWorkspace(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", additional.keys = NULL))))
	expect_that(gs, is_a("GatingSet"));
        
    expect_warning(expect_error(suppressMessages(parseWorkspace(ws
                                                                , path = file.path(dataDir, "gs_manual")
                                                                , name = 4
                                                                , subset = "CytoTrol_CytoTrol_1.fcs"
                                                                , additional.keys = NULL
                                                                )
                                                )
                                  , "no sample")
                       , "Can't find")
                
	
})


gh <- NULL
test_that("extract GatingHierarchy from GatingSet",{
      gh <<- gs[[1]]
      expect_that(gh, is_a("GatingHierarchy"));  
    })


test_that("parse without gating",{
      
      dd <- capture.output(suppressMessages(gs1 <- try(parseWorkspace(ws, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", execute = FALSE))))
      expect_that(gs1, is_a("GatingSet"));
      gh1 <- gs1[[1]]
      
      thisStats <- getPopStats(gh1)[, list(flowJo.freq,flowJo.count, node)]
      expectStats <- getPopStats(gh)[, list(flowJo.freq,flowJo.count, node)]
      expect_equal(thisStats, expectStats)
      
      #exclude the gates that require extension since the extend_to are different 
      # based on whether data is loaded
      nodes <- getNodes(gh)[ -c(6:13, 15:22)]
      thisGates <- sapply(nodes[-1], getGate, obj = gh1)
      expectGates <- sapply(nodes[-1], getGate, obj = gh)
      expect_equal(thisGates, expectGates)
      
      
    })

# make sure this test is invoked before GatingSet-testSuite since the trans is gonna be lost
# during clone and rbind2 test
test_that("getTransformations ",{
      
      thisRes <- getTransformations(gh)
      expectRes <- readRDS(file.path(resultDir, "getTransformations_gh.rds"))
      expectRes <- sapply(expectRes, function(f){attr(f, "type") <- "caltbl"; f})
      expect_equal(thisRes,expectRes)
      
    })
source("GatingHierarchy-testSuite.R", local = TRUE)
source("GatingSet-testSuite.R", local = TRUE)

test_that("use additional keywords for guid",{
      dd <- capture.output(suppressMessages(gs2 <- try(parseWorkspace(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", additional.keys = "$TOT"))))
      expect_equal(sampleNames(gs2[[1]]), paste(sampleNames(gh), flowWorkspace:::trimWhiteSpace(keyword(gh)[["$TOT"]]), sep = "_"))
      expect_equal(getPopStats(gs2[[1]]), getPopStats(gh))
        
    })

test_that("supply sampleID--file mapping through 'path'",{
      mapping <- data.frame(sampleID1 = '1', file = file.path(dataDir, "CytoTrol_CytoTrol_11.fcs"))
      expect_error(dd <- capture.output(suppressMessages(gs3 <- parseWorkspace(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
                  , "When 'path' is a data.frame, it must contain columns")
      colnames(mapping)[1] <- "sampleID"
      expect_error(dd <- capture.output(suppressMessages(gs3 <- parseWorkspace(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
          , "must be numeric")
      mapping[["sampleID"]] <- 1
      expect_error(dd <- capture.output(suppressMessages(gs3 <- parseWorkspace(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
          , "No sample")
      mapping[["sampleID"]] <- 19
      expect_error(dd <- capture.output(suppressMessages(gs3 <- parseWorkspace(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
          , "not a valid file")
      mapping[["file"]] <- file.path(dataDir, "CytoTrol_CytoTrol_1.fcs")
      dd <- capture.output(suppressMessages(gs3 <- parseWorkspace(ws, path = mapping, name = 4, subset = "CytoTrol_CytoTrol_1.fcs")))
      expect_equal(getPopStats(gs3[[1]]), getPopStats(gh))
      
    })

test_that("parse pData from keyword", {
      #parse pData from xml
    dd <- capture.output(suppressMessages(gs1 <- parseWorkspace(ws, path = dataDir, name = 4, keywords = c("PATIENT ID", "SAMPLE ID", "$TOT", "EXPERIMENT NAME"), keywords.source = "XML", additional.keys = "$TOT", execute = F)))
    pd1 <- pData(gs1)
    expect_equal(nrow(pd1), 4)
    
    #parse pData from FCS
    dd <- capture.output(suppressWarnings(suppressMessages(gs2 <- parseWorkspace(ws, path = dataDir, name = 4, keywords = c("PATIENT ID", "SAMPLE ID", "$TOT", "EXPERIMENT NAME"), keywords.source = "FCS", additional.keys = "$TOT"))))
    pd2 <- pData(gs2)
    expect_equal(nrow(pd2), 2)
        
    expect_equivalent(pd1[1:2, ], pd2)
    
    })


test_that("subset", {

    #subset by keyword  
    dd <- capture.output(suppressMessages(gs1 <- parseWorkspace(ws, path = dataDir, name = 4, subset = `TUBE NAME` %in% c("CytoTrol_1", "CytoTrol_2"), keywords = "TUBE NAME", execute = F)))
    #subset by sample names
    dd <- capture.output(suppressMessages(gs2 <- parseWorkspace(ws, path = dataDir, name = 4, subset = c("CytoTrol_CytoTrol_1.fcs", "CytoTrol_CytoTrol_2.fcs"), keywords = "TUBE NAME", execute = F)))
    expect_equivalent(pData(gs1), pData(gs2))
    
    #subset by numeric index
    dd <- capture.output(suppressMessages((gs3 <- parseWorkspace(ws, path = dataDir, name = 4, subset = 1:2, keywords = "TUBE NAME", execute = F))))
    expect_equivalent(pData(gs1), pData(gs3))
    
    expect_error(gs4 <- parseWorkspace(ws, path = dataDir, name = 4, subset = 1:2, keywords = "TUBE NAME", execute = F, keywords.source = "FCS")
                , "Please set 'execute' to TRUE")
    
            
    })
test_that("closeWorkspace",
    {
      closeWorkspace(ws)
      thisRes <- paste(capture.output(show(ws))[-2], collapse = "")
      expectRes <- paste(fjRes[["ws_show_close"]][-2], collapse = "")
      expect_output(thisRes, expectRes)
      
    })


