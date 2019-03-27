context("gs_get_pop_stats")
test_that("group and merge the GatingSet object", {
  
  gs <- load_gs(file.path(dataDir,"gs_bcell_auto"))
  stats <- gs_get_pop_stats(gs)
  
  #change the order of nodes by removing and adding it back
  g <- getGate(gs[[1]], "CD19")
  gh_remove_gate("CD19", gs[[1]])
  gh_add_gate(gs[[1]], g, parent = "Live")
  recompute(gs)
  expect_true(setequal(gs_get_pop_paths(gs[[1]]), gs_get_pop_paths(gs[[2]])))#same set
  expect_false(isTRUE(all.equal(gs_get_pop_paths(gs[[1]]), gs_get_pop_paths(gs[[2]]))))#but different order now
  # stats.new <- rbind(gs_get_pop_stats(gs[1]), gs_get_pop_stats(gs[2]))
  stats.new <- gs_get_pop_stats(gs)
  setkey(stats, name, Population, Parent)
  setkey(stats.new, name, Population, Parent)
  expect_equal(stats, stats.new)
  
})
