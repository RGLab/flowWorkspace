context("-- parsed gs")

wsfile <- list.files(dataDir, pattern="manual.xml",full=TRUE)
library(CytoML)
ws <- open_flowjo_xml(wsfile);
test_that("can load xml workspace",
{
  
  expect_that(ws, is_a("flowjo_workspace"))
})

dd <- capture.output(gs <- flowjo_to_gatingset(ws, path = dataDir, name = 4
                                               , subset = "CytoTrol_CytoTrol_1.fcs"
                                               , additional.keys = NULL
                                               
                                               )
                     )

gh <- gs[[1]]

# make sure this test is invoked before GatingSet-testSuite since the trans is gonna be lost
# during gs_clone and gslist_to_gs test
test_that("gh_get_transformations ",{
      
      thisRes <- gh_get_transformations(gh)
      expect_equal(attr(thisRes[[1]], "type"), "biexp")
      expect_true(setequal(names(thisRes), colnames(gh)[5:11]))
      
    })
isCpStaticGate <<- TRUE
source("GatingHierarchy-testSuite.R", local = TRUE)
if(!win32_flag)
  source("GatingSet-testSuite.R", local = TRUE)


# we need test trans so have to put this test here since the legacy archived gs doesn't have trans
test_that("updateChannles",{

  dd <- capture.output(suppressMessages(gs1 <- flowjo_to_gatingset(ws, path = dataDir, name = 4
                                                                   , subset = 1:2#`TUBE NAME` %in% c("CytoTrol_1", "CytoTrol_2")
                                                                   , keywords = "TUBE NAME"
                                                                   
                                                                   )
                                        )
                       )
  oldCols <- colnames(gs_pop_get_data(gs1)[[1, use.exprs = F]])
  comp_cols <- parameters(gh_get_compensations(gs1[[1]]))
  trans_names <- names(gh_get_transformations(gs1[[1]]))
  map <- data.frame(old = c("FSC-A", "V450-A", "non-exist", "B710-A")
                    , new = c("fsc", "v450-a", "newchnl", "b710"))
  
  gs1 <- gs_update_channels(gs1, map)
  
  #check gates
  expect_equivalent(unique(lapply(gs_pop_get_gate(gs1, "singlets"), parameters))[[1]], c("fsc", "FSC-H"))
  expect_equivalent(unique(lapply(gs_pop_get_gate(gs1, "CD3+"), parameters))[[1]], c("<v450-a>", "SSC-A"))
  expect_equivalent(unique(lapply(gs_pop_get_gate(gs1, "CD4"), parameters))[[1]], c("<b710>", "<R780-A>"))
  
  #check comps
  comp <- unique(lapply(gs1, gh_get_compensations))[[1]]
  expect_is(comp, "compensation")  
  expect_equivalent(parameters(comp), comp_cols %>% 
                                        gsub("V450-A", "v450-a", .) %>%
                                        gsub("FSC-A", "fsc", .) %>%
                                        gsub("B710-A", "b710", .)
                  )
  
  #check trans
  trans <- gh_get_transformations(gs1[[1]], channel = "all")
  expect_equal(names(trans)[1:7], trans_names %>% gsub("B710-A", "b710", .) %>% gsub("V450-A", "v450-a", .))
  
  expect_is(gs1, "GatingSet")
  cols <- colnames(gs_pop_get_data(gs1))
  expect_equal(cols, oldCols %>% 
                       gsub("V450-A", "v450-a", .) %>%
                       gsub("FSC-A", "fsc", .) %>%
                       gsub("B710-A", "b710", .)
              )
  
  
})

