context("gh_pop_move")
skip_if(win32_flag)
test_that("gh_pop_move", {
  
  gs <- load_gs(file.path(dataDir,"gs_manual"))
  gh <- gs[[1]]
  stats <- gh_pop_compare_stats(gh)
  
  old.parent <- gs_pop_get_parent(gh, "CD4")
  
  new.parent <- "CD4"
  expect_error(gh_pop_move(gh, "CD4", new.parent), "itself", class = "error")
  
  new.parent <- "CD4/CCR7- 45RA+"
  expect_error(gh_pop_move(gh, "CD4", new.parent), "descendants", class = "error")
  
  new.parent <- "singlets"
  suppressMessages(gh_pop_move(gh, "CD4", new.parent))
  expect_equal(gs_pop_get_parent(gh, "CD4", path = "auto"), new.parent)
  
  #mv back to original parent
  suppressMessages(gh_pop_move(gh, "CD4", old.parent))
  expect_equal(gs_pop_get_parent(gh, "CD4"), old.parent)
  expect_equal(gh_pop_compare_stats(gh), stats)
  
})
