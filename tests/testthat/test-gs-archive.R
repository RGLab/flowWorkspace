skip_if(win32_flag)
backend_mode <- "tile"
context(paste0("GatingSet archive -- ", backend_mode))
source("gs-archive.R")

backend_mode <- "h5"
context(paste0("GatingSet archive -- ", backend_mode))
source("gs-archive.R")