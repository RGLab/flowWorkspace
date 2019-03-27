context("moveNode")
test_that("moveNode", {
  
  gs <- load_gs(file.path(dataDir,"gs_manual"))
  gh <- gs[[1]]
  stats <- gh_get_pop_stats(gh)
  
  old.parent <- gs_get_parent(gh, "CD4")
  
  new.parent <- "CD4"
  expect_error(moveNode(gh, "CD4", new.parent), "itself")
  
  new.parent <- "CD4/CCR7- 45RA+"
  expect_error(moveNode(gh, "CD4", new.parent), "descendants")
  
  new.parent <- "singlets"
  suppressMessages(moveNode(gh, "CD4", new.parent))
  expect_equal(gs_get_parent(gh, "CD4", path = "auto"), new.parent)
  
  #mv back to original parent
  suppressMessages(moveNode(gh, "CD4", old.parent))
  expect_equal(gs_get_parent(gh, "CD4"), old.parent)
  expect_equal(gh_get_pop_stats(gh), stats)
  
})
