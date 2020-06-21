skip_if(win32_flag)
if(have_tiledb())
{
  backend_mode <<- "tile"
  set_default_backend(backend_mode)
  context(paste("backend:", backend_mode))
  source("cytoframe-suite.R", local = TRUE)
  source("cytoset-suite.R", local = TRUE)
  source("gs-archive.R", local = TRUE)
  source("gs-parsed.R", local = TRUE)
  source("comp-trans-gs.R", local = TRUE)
  source("copy-gh.R", local = TRUE)
  source("s3-gs.R", local = TRUE)
  source("pop-add.R", local = TRUE)
  source("cleanup_temp.R", local = TRUE)
  source("copy_gate.R", local = TRUE)
  source("filter_to_list.R", local = TRUE)
  source("parallel_load_gs.R", local = TRUE)
  source("scale_gate.R", local = TRUE)
  source("write.FCS.R", local = TRUE)
  source("gh_pop_move.R", local = TRUE)
}
backend_mode <<- "h5"
set_default_backend(backend_mode)
context(paste("backend:", backend_mode))
source("cytoframe-suite.R", local = TRUE)
source("cytoset-suite.R", local = TRUE)
source("gs-archive.R", local = TRUE)
source("gs-parsed.R", local = TRUE)
source("comp-trans-gs.R", local = TRUE)
source("copy-gh.R", local = TRUE)
source("cleanup_temp.R", local = TRUE)
source("convert-legacy.R", local = TRUE)
source("gs_pop_get_count_fast.R", local = TRUE)
source("parallel_load_gs.R", local = TRUE)

backend_mode <<- "mem"
set_default_backend(backend_mode)
context(paste("backend:", backend_mode))
source("cytoframe-suite.R", local = TRUE)
source("cytoset-suite.R", local = TRUE)



