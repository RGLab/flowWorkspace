context("moveNode")
test_that("group and merge the GatingSet object", {
  
  gs <- load_gs(file.path(dataDir,"gs_bcell_auto"))
  stats <- getPopStats(gs)
  
  #change the order of nodes by removing and adding it back
  g <- getGate(gs[[1]], "CD19")
  Rm("CD19", gs[[1]])
  add(gs[[1]], g, parent = "Live")
  recompute(gs)
  expect_true(setequal(getNodes(gs[[1]]), getNodes(gs[[2]])))#same set
  expect_false(isTRUE(all.equal(getNodes(gs[[1]]), getNodes(gs[[2]]))))#but different order now
  # stats.new <- rbind(getPopStats(gs[1]), getPopStats(gs[2]))
  stats.new <- getPopStats(gs)
  setkey(stats, name, Population, Parent)
  setkey(stats.new, name, Population, Parent)
  expect_equal(stats, stats.new)
  
})
