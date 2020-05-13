context("cytoset accessors")
skip_if(win32_flag)
# fs <- GvHD[pData(GvHD)$Patient %in% 6:7][1:4]#can't use it due to its malformated FCS TEXT making test difficult
fcs_files <- list.files(dataDir, "Cyto", full.names = TRUE)
fs <- read.flowSet(fcs_files)
lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)

#tile version
context("cytoset test suite--tile")
backend_mode <- "tile"
source("cytoset-suite.R", local = TRUE)

context("cytoset test suite--h5")
backend_mode <- "h5"
source("cytoset-suite.R", local = TRUE)

context("cytoset test suite--mem")
backend_mode <- "mem"
source("cytoset-suite.R", local = TRUE)



