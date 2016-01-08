context("parse gatingMLs exported from Cytobank ")

path <- "~/rglab/workspace/flowWorkspace/wsTestSuite"

test_that("gatingML-cytobank parsing: cytotrol tcell",{
  xmlfile <- system.file("extdata/cytotrol_tcell_cytobank.xml", package = "flowWorkspace")
  g <- read.gatingML.cytobank(xmlfile)
  fcsFiles <- list.files(pattern = "CytoTrol", system.file("extdata", package = "flowWorkspaceData"), full = T)
  fs <- read.ncdfFlowSet(fcsFiles)
  
  
  ## Compensate the data with the compensation information stored in `graphGML` object
  fs <- compensate(fs, g)
  ## Extract transformation functions from `graphGML` and transform the data
  trans <- getTransformations(g)
  fs <- transform(fs, trans)
  
  
  ## Construct the **GatingSet** and apply gates stored in `graphGML`
  gs <- GatingSet(fs)
  gating(g, gs)
  
  
  #' ## verify the stats are correct
  
  #' 
  #load stats from cytobank
  cytobank_counts <- read.csv(system.file("extdata/cytotrol_tcell_cytobank_counts.csv", package = "flowWorkspace"), skip = 7, stringsAsFactors = F)
  #convert it to the same format
  colnames(cytobank_counts)[1] <- "Population"
  cytobank_counts <- plyr::arrange(cytobank_counts, Population)
  
  #load openCyto stats
  openCyto_counts <- getPopStats(gs, statType = "count")
  openCyto_counts <- reshape2::dcast(openCyto_counts, Population ~ name, value.var = "Count")
  
  
  expect_equal(cytobank_counts, openCyto_counts, tol = 5e-4)
  
  
  
})

test_that("gatingML-cytobank parsing: Merck FirstExample",{
  thisPath <- file.path(path, "/gatingML/Merck/firstExample")
  xmlfile <- file.path(thisPath, "CytExp_10623_Gates_v5.xml")
  g <- read.gatingML.cytobank(xmlfile)
  fcsFiles <- list.files(pattern = "\\.fcs", thisPath, full = T)
  fs <- read.ncdfFlowSet(fcsFiles)
  
  fs <- compensate(fs, g)
  
  trans <- getTransformations(g)
  fs <- transform(fs, trans)
  
  gs <- GatingSet(fs)
  gating(g, gs)
  
  ### Verify the stats are correct
  #load stats from cytobank
  cytobank_counts <- fread(file.path(thisPath,"population_counts.csv"), stringsAsFactors = FALSE)
  # Melt the data
  cytobank_counts_long <- melt(cytobank_counts, variable.name = "population", value.name = "count", id.vars = "FCS Filename")
  # Change column names
  setnames(cytobank_counts_long, c("FCS Filename"), c("fcs_filename"))
  # Properly format the column names
  cytobank_counts_long <- cytobank_counts_long[,population := gsub("_EventCounts", "", population)]
  
  
  # extract the counts from our gating sets
  #load openCyto stats
  opencyto_counts <- getPopStats(gs, statType = "count")
  setnames(opencyto_counts, names(opencyto_counts), c("fcs_filename", "population", "parent", "count", "parent_count"))
  #drop the parent column for simplicity
  opencyto_counts <- opencyto_counts[,.(fcs_filename, population, count)]
  #Remove spaces in population names as cytobank removes them here
  opencyto_counts <- opencyto_counts[, population := gsub(" ", "", population)]
  
  
  
  # merge the two data.tables
  
  # set key (fcs_filename, population)
  setkey(opencyto_counts, fcs_filename, population)
  setkey(cytobank_counts_long, fcs_filename, population)
  dt_merged <- merge(opencyto_counts, cytobank_counts_long)
  unequaled <- dt_merged[count.x != count.y]
  expect_equal(nrow(unequaled), 0)
})

test_that("gatingML-cytobank parsing: Merck SecondExample",{
  thisPath <- file.path(path, "/gatingML/Merck/SecondExample")
  xmlfile <- file.path(thisPath, "CytExp_10624_Gates_v3.xml")
  g <- read.gatingML.cytobank(xmlfile)
  fcsFiles <- list.files(pattern = "\\.fcs", thisPath, full = T)
  fs <- read.ncdfFlowSet(fcsFiles)
  
  fs <- compensate(fs, g)
  
  trans <- getTransformations(g)
  fs <- transform(fs, trans)
  
  gs <- GatingSet(fs)
  gating(g, gs)
  
  ### Verify the stats are correct
  #load stats from cytobank
  cytobank_counts <- read.csv(file.path(thisPath, "secondExample.csv"), skip = 7, stringsAsFactors = F, check.names = F)
  #convert it to the same format
  colnames(cytobank_counts)[1] <- "Population"
  cytobank_counts[["Population"]] <- gsub("_EventCounts", "", cytobank_counts[["Population"]])
  cytobank_counts <- plyr::arrange(cytobank_counts, Population)
  
  #load openCyto stats
  openCyto_counts <- getPopStats(gs, statType = "count")
  openCyto_counts <- reshape2::dcast(openCyto_counts, Population ~ name, value.var = "Count")
  openCyto_counts[["Population"]] <- gsub(" ", "", openCyto_counts[["Population"]])
  #compare two
  expect_equal(cytobank_counts, openCyto_counts)
  
  
  
})
