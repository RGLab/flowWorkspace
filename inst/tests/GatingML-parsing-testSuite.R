context("parse gatingMLs exported from Cytobank ")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite/gatingML"



test_that("gatingML-cytobank parsing: no transformations",{
  xmlfile <- file.path(path, "no_trans.xml")
  g <- read.gatingML.cytobank(xmlfile)
  expect_is(g, "graphGML")
  expect_null(getTransformations(g))
})

test_that("gatingML-cytobank parsing: cytotrol tcell",{
  xmlfile <- system.file("extdata/cytotrol_tcell_cytobank.xml", package = "flowWorkspace")
  fcsFiles <- list.files(pattern = "CytoTrol", system.file("extdata", package = "flowWorkspaceData"), full = T)
  gs <- parse.gatingML(xmlfile, fcsFiles)
  
  
  #' ## verify the stats are correct
  statsfile <- system.file("extdata/cytotrol_tcell_cytobank_counts.csv", package = "flowWorkspace")
  dt_merged <- compare.counts(gs, statsfile, id.vars = "population")
  
  
  expect_equal(dt_merged[, count.x], dt_merged[, count.y], tol = 5e-4)
  
  
  
})

test_that("gatingML-cytobank parsing: Merck FirstExample",{
  thisPath <- file.path(path, "Merck/firstExample")
  xmlfile <- file.path(thisPath, "CytExp_10623_Gates_v5.xml")
  fcsFiles <- list.files(pattern = "\\.fcs", thisPath, full = T)
  gs <- parse.gatingML(xmlfile, fcsFiles[c(1,3,6)])
  
  ### Verify the stats are correct
  statsfile <- file.path(thisPath,"population_counts.csv")
  dt_merged <- compare.counts(gs, statsfile)
  
  unequaled <- dt_merged[count.x != count.y]
  expect_equal(nrow(unequaled), 0)
})

test_that("gatingML-cytobank parsing: Merck SecondExample",{
  thisPath <- file.path(path, "/Merck/SecondExample")
  xmlfile <- file.path(thisPath, "CytExp_10624_Gates_v3.xml")
  fcsFiles <- list.files(pattern = "\\.fcs", thisPath, full = T)
  
  gs <- parse.gatingML(xmlfile, fcsFiles[c(1,4,6,12)])
  
  ### Verify the stats are correct
  statsfile <- file.path(thisPath,"secondExample.csv")
  dt_merged <- compare.counts(gs, statsfile, id.vars = "population") #subset the files to speed up testing
  
  
  unequaled <- dt_merged[count.x != count.y]
  expect_equal(nrow(unequaled), 0)
})
