context("cytoframe test suite--h5")
skip_if(win32_flag)
fcs_file <- list.files(dataDir, "Cyto", full.names = TRUE)[1]
fr <- read.FCS(fcs_file)
cf <- load_cytoframe_from_fcs(fcs_file, format = "h5")

lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)

rectGate <- rectangleGate(filterId="nonDebris","FSC-H"=c(200,Inf))
ish5 <- TRUE
cf_lock(cf)
source("cytoframe-suite.R", local = TRUE)

context("cytoframe test suite--mem")

#in-mem version
cf <- load_cytoframe_from_fcs(fcs_file)
ish5 <- FALSE
cf_lock(cf)
source("cytoframe-suite.R", local = TRUE)

context("cytoframe test suite--tile")

#tile version
cf <- load_cytoframe_from_fcs(fcs_file, format = "tile")
ish5 <- TRUE
cf_lock(cf)
source("cytoframe-suite.R", local = TRUE)
