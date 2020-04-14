context("copyNode")
skip_if(win32_flag)
test_that("copyNode", {
  
  gs <- load_gs(file.path(dataDir,"gs_manual"))
  gh <- gs[[1]]
  gs1 <- gs_clone(gs)
  gs2 <- gs_clone(gs)
  gh1<- gs1[[1]]
  gh2<- gs2[[1]]
  # Add in a few more nodes to test recursive depth
  rg1 <- rectangleGate(filterId="HLA+", "<V545-A>"=c(2400, Inf))
  rg2 <- rectangleGate(filterId="HLA-", "<V545-A>"=c(2400, Inf))
  pop_add(gh = gh,rg1, parent = "/not debris/singlets/CD3+/CD4/38- DR+")
  pop_add(gh = gh,rg2, parent = "/not debris/singlets/CD3+/CD4/38- DR+", negated = TRUE)
  pop_add(gh = gh,rg1, parent = "/not debris/singlets/CD3+/CD4/38- DR-")
  pop_add(gh = gh,rg2, parent = "/not debris/singlets/CD3+/CD4/38- DR-", negated = TRUE)
  recompute(gh)
  # Hide a node
  gh_pop_set_visibility(gh,"/not debris/singlets/CD3+/CD4/38- DR-/HLA-", FALSE)
  
  old.stats <- gh_pop_compare_stats(gh, path = "full")
  old.parent <- gs_pop_get_parent(gh, "CD4", path="full")
  old.children <- gs_pop_get_children(gh, "CD4", path = 1)
  old.descendants <- sub("^.*CD4", "", gh_pop_get_descendants(gh, "CD4"))
  
  new.parent <- "CD4"
  expect_error(gh_copy_gate(gh, "CD4", new.parent), "itself")
  new.parent <- "CD4/CCR7- 45RA+"
  expect_error(gh_copy_gate(gh, "CD4", new.parent), "descendants")

  new.parent <- "singlets"
  suppressMessages(gh_copy_gate(gh, "CD4", new.parent))
  # Make sure everything moved to the new location in the tree
  expect_equal(gs_pop_get_parent(gh, "/not debris/singlets/CD4", path = "auto"), new.parent)
  expect_equal(gs_pop_get_children(gh, "/not debris/singlets/CD4", path = 1), old.children)
  new.descendants <- sub("^.*CD4", "", gh_pop_get_descendants(gh, "/not debris/singlets/CD4", path = "full"))
  expect_equal(new.descendants, old.descendants)
  expect_true(gh_pop_is_hidden(gh, "/not debris/singlets/CD4/38- DR-/HLA-"))
  
  # Make sure everything is still unchanged in its original location in the tree
  expect_equal(gs_pop_get_parent(gh, "/not debris/singlets/CD3+/CD4", path = "full"), old.parent)
  expect_equal(gs_pop_get_children(gh, "/not debris/singlets/CD3+/CD4", path = 1), old.children)
  new.descendants <- sub("^.*CD4", "", gh_pop_get_descendants(gh, "/not debris/singlets/CD3+/CD4", path = "full"))
  expect_equal(new.descendants, old.descendants)
  expect_true(gh_pop_is_hidden(gh, "/not debris/singlets/CD3+/CD4/38- DR-/HLA-"))
  
  # Make sure the calculations are unaltered on the original branch
  new.stats <- gh_pop_compare_stats(gh, path = "full")
  new.rows <- new.stats[!(new.stats$node %in% old.stats$node),]
  new.stats <- new.stats[new.stats$node %in% old.stats$node,]

  expect_equal(new.stats, old.stats)
  # Make sure the stats are getting computed for the new nodes
  expect_true(is.numeric(new.rows$openCyto.freq) && all(!is.na(new.rows$openCyto.freq)))
  expect_true(is.numeric(new.rows$openCyto.count) && all(!is.na(new.rows$openCyto.count)))
  
  # Testing broadcasting a node, per use case in feature request
  rg1 <- rectangleGate(filterId="HLA+", "<V545-A>"=c(2400, Inf))
  rg2 <- rectangleGate(filterId="HLA-", "<V545-A>"=c(2400, Inf))
  # Add some extra nodes to one GatingHierarchy
  pop_add(gh = gh1,rg1, parent = "/not debris/singlets/CD3+/CD4/38- DR+")
  pop_add(gh = gh1,rg2, parent = "/not debris/singlets/CD3+/CD8/CCR7+ 45RA+", negated = TRUE)
  recompute(gh1)
  # Add those nodes in a different location on another GatingHierarchy
  pop_add(gh = gh2,rg1, parent = "/not debris/singlets/CD3+/CD8/CCR7- 45RA-")
  pop_add(gh = gh2,rg2, parent = "/not debris/singlets/CD3+/CD4/38- DR-", negated = TRUE)
  recompute(gh2)

  # Broadcasting 
  # Maybe make a convenience wrapper for similar logic for the user if it's a common use case
  gh1_bc1 <- c(gs_pop_get_children(gh1, "CD4", path = "full"), gs_pop_get_children(gh1, "CD8", path = "full"))
  gh1_bc1 <- gh1_bc1[gh1_bc1!=gs_pop_get_parent(gh1, "HLA+", path = "full")]
  gh1_bc2 <- c(gs_pop_get_children(gh1, "CD4", path = "full"), gs_pop_get_children(gh1, "CD8", path = "full"))
  gh1_bc2 <- gh1_bc2[gh1_bc2!=gs_pop_get_parent(gh1, "HLA-", path = "full")]

  gh2_bc1 <- c(gs_pop_get_children(gh2, "CD4", path = "full"), gs_pop_get_children(gh2, "CD8", path = "full"))
  gh2_bc1 <- gh2_bc1[gh2_bc1!=gs_pop_get_parent(gh2, "HLA+", path = "full")]
  gh2_bc2 <- c(gs_pop_get_children(gh2, "CD4", path = "full"), gs_pop_get_children(gh2, "CD8", path = "full"))
  gh2_bc2 <- gh2_bc2[gh2_bc2!=gs_pop_get_parent(gh2, "HLA-", path = "full")]
  
  path1_1 <- gh_pop_get_full_path(gh1, "HLA+")
  path1_2 <- gh_pop_get_full_path(gh1, "HLA-")
  path2_1 <- gh_pop_get_full_path(gh2, "HLA+")
  path2_2 <- gh_pop_get_full_path(gh2, "HLA-")
    
  lapply(gh1_bc1, function(x) gh_copy_gate(gh1[[1]], path1_1, x))
  lapply(gh1_bc2, function(x) gh_copy_gate(gh1[[1]], path1_2, x))
  lapply(gh2_bc1, function(x) gh_copy_gate(gh2[[1]], path2_1, x))
  lapply(gh2_bc2, function(x) gh_copy_gate(gh2[[1]], path2_2, x))
  
  # Verify that the results are the same for different broadcast sources
  stats1 <- gh_pop_compare_stats(gh1, path = "full")[order(node),]
  stats2 <- gh_pop_compare_stats(gh2, path = "full")[order(node),]
  expect_equal(stats1, stats2)
})
