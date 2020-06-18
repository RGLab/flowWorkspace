context("gs_pop_get_count_fast")
skip_if(win32_flag)
test_that("group and merge the GatingSet object", {
  
  gs <- load_gs(file.path(dataDir,"gs_bcell_auto"))
  stats <- gs_pop_get_count_fast(gs)
  
  #change the order of nodes by removing and adding it back
  g <- gh_pop_get_gate(gs[[1]], "CD19")
  gh_pop_remove(gs[[1]], "CD19")
  pop_add(g, gs[[1]], parent = "Live")
  recompute(gs)
  expect_true(setequal(gs_get_pop_paths(gs[[1]]), gs_get_pop_paths(gs[[2]])))#same set
  expect_false(isTRUE(all.equal(gs_get_pop_paths(gs[[1]]), gs_get_pop_paths(gs[[2]]))))#but different order now
  # stats.new <- rbind(gs_pop_get_count_fast(gs[1]), gs_pop_get_count_fast(gs[2]))
  stats.new <- gs_pop_get_count_fast(gs)
  setkey(stats, name, Population, Parent)
  setkey(stats.new, name, Population, Parent)
  expect_equal(stats, stats.new)
  
  # Make sure frequencies are working as expected
  normalized_in_c <- gs_pop_get_count_fast(gs, "freq")
  normalized <- lapply(gs, function(gh){
    df <- gs_pop_get_count_fast(gh)
    totalEvents <- as.numeric(keyword(gh, "$TOT"))
    df$Frequency <- df$Count / totalEvents
    df$ParentFrequency <- df$ParentCount / totalEvents
    # Need to do this because otherwise the row orders differ
    df[order(match(df$Population, normalized_in_c$Population))]
  })
  normalized_in_r <- do.call(rbind, normalized)[,-c("Count", "ParentCount")]
  expect_equal(normalized_in_r, normalized_in_c, tolerance = 1e-6)
  
})

