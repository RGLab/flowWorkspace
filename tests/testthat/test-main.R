skip_if(win32_flag)

backend_mode <<- "tile"
context(paste("backend:", backend_mode))
source("cytoframe-suite.R", local = TRUE)
source("cytoset-suite.R", local = TRUE)
source("gs-archive.R", local = TRUE)
source("gs-parsed.R", local = TRUE)
source("comp-trans-gs.R", local = TRUE)
source("copy-gh.R", local = TRUE)
source("s3-gs.R", local = TRUE)

backend_mode <<- "h5"
context(paste("backend:", backend_mode))
source("cytoframe-suite.R", local = TRUE)
source("cytoset-suite.R", local = TRUE)
source("gs-archive.R", local = TRUE)
source("gs-parsed.R", local = TRUE)
source("comp-trans-gs.R", local = TRUE)
source("copy-gh.R", local = TRUE)

backend_mode <<- "mem"
context(paste("backend:", backend_mode))
source("cytoframe-suite.R", local = TRUE)
source("cytoset-suite.R", local = TRUE)



