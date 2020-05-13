skip_if(win32_flag)
fcs_file <- list.files(dataDir, "Cyto", full.names = TRUE)[1]
fr <- read.FCS(fcs_file)
lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)
rectGate <- rectangleGate(filterId="nonDebris","FSC-H"=c(200,Inf))

#tile version
context("cytoframe test suite--tile")
cf <- load_cytoframe_from_fcs(fcs_file, backend = "tile")
backend_mode <- "tile"
cf_lock(cf)
source("cytoframe-suite.R", local = TRUE)

context("cytoframe test suite--h5")
cf <- load_cytoframe_from_fcs(fcs_file, backend = "h5")
backend_mode <- "h5"
cf_lock(cf)
source("cytoframe-suite.R", local = TRUE)

context("cytoframe test suite--mem")

#in-mem version
cf <- load_cytoframe_from_fcs(fcs_file)
backend_mode <- "mem"
cf_lock(cf)
source("cytoframe-suite.R", local = TRUE)



