context("parseWorkspace")

wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)
library(CytoML)
ws <- openWorkspace(wsfile);
test_that("can load xml workspace",
{
  
  expect_that(ws, is_a("flowJoWorkspace"))
})


gs <- NULL
test_that("Can parse workspace",{
    dd <- capture.output(suppressMessages(gs <<- try(parseWorkspace(ws, path = dataDir, name = 4, subset = "CytoTrol_CytoTrol_1.fcs", additional.keys = NULL))))
	  expect_that(gs, is_a("GatingSet"));
        
    expect_output(expect_error(suppressMessages(parseWorkspace(ws
                                                                , path = file.path(dataDir, "gs_manual")
                                                                , name = 4
                                                                , subset = "CytoTrol_CytoTrol_1.fcs"
                                                                , additional.keys = NULL
                                                                )
                                                )
                                  , "No sample")
                       , "FCS not found")
                
	
})


gh <- gs[[1]]

# make sure this test is invoked before GatingSet-testSuite since the trans is gonna be lost
# during clone and rbind2 test
test_that("getTransformations ",{
      
      thisRes <- getTransformations(gh)
      expectRes <- readRDS(file.path(resultDir, "getTransformations_gh.rds"))
      expectRes <- sapply(expectRes, function(f){
                                attr(f, "type") <- "biexp"
                                f
                              })
      thisRes <- sapply(thisRes, function(f){
                attr(f, "parameters") <- NULL
                f
              })
      names(expectRes) <- sapply(names(expectRes), function(tn){strsplit(tn, " ")[[1]][4]}, USE.NAMES = FALSE)
      
      expect_equal(thisRes[1:7],expectRes)
      
      
      
    })
isCpStaticGate <<- TRUE
source("GatingHierarchy-testSuite.R", local = TRUE)
source("GatingSet-testSuite.R", local = TRUE)


# we need test trans so have to put this test here since the legacy archived gs doesn't have trans
test_that("updateChannles",{
  
  dd <- capture.output(suppressMessages(gs1 <- parseWorkspace(ws, path = dataDir, name = 4, subset = 1:2, keywords = "TUBE NAME")))
  oldCols <- colnames(getData(gs1)[[1, use.exprs = F]])
  comp_cols <- parameters(getCompensationMatrices(gs1[[1]]))
  trans_names <- names(getTransformations(gs1[[1]]))
  map <- data.frame(old = c("FSC-A", "V450-A", "non-exist", "B710-A")
                    , new = c("fsc", "v450-a", "newchnl", "b710"))
  
  #update flow data
  gs1 <- updateChannels(gs1, map)
  expect_is(gs1, "GatingSet")
  cols <- colnames(getData(gs1))
  expect_equal(cols, oldCols %>% 
                 gsub("V450-A", "v450-a", .) %>%
                 gsub("FSC-A", "fsc", .) %>%
                 gsub("B710-A", "b710", .)
  )
  
  #check gates
  expect_equivalent(unique(lapply(getGate(gs1, "singlets"), parameters))[[1]], c("fsc", "FSC-H"))
  expect_equivalent(unique(lapply(getGate(gs1, "CD3+"), parameters))[[1]], c("<v450-a>", "SSC-A"))
  expect_equivalent(unique(lapply(getGate(gs1, "CD4"), parameters))[[1]], c("<b710>", "<R780-A>"))
  
  #check comps
  comp <- unique(lapply(gs1, getCompensationMatrices))[[1]]
  expect_is(comp, "compensation")  
  expect_equivalent(parameters(comp), comp_cols %>% 
                                        gsub("V450-A", "v450-a", .) %>%
                                        gsub("FSC-A", "fsc", .) %>%
                                        gsub("B710-A", "b710", .)
                  )
  
  #check trans
  trans <- getTransformations(gs1[[1]], channel = "all")
  expect_equal(names(trans)[1:7], trans_names %>% gsub("B710-A", "b710", .) %>% gsub("V450-A", "v450-a", .))
  
  
  
})

