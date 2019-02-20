context("copyNode")
test_that("copyNode", {
  
  gs <- load_gs(file.path(dataDir,"gs_manual"))
  gh <- gs[[1]]
  gh1 <- clone(gh)
  gh2 <- clone(gh)
  
  # Add in a few more nodes to test recursive depth
  rg1 <- rectangleGate(filterId="HLA+", "<V545-A>"=c(2400, Inf))
  rg2 <- rectangleGate(filterId="HLA-", "<V545-A>"=c(2400, Inf))
  add(gh, rg1, parent = "/not debris/singlets/CD3+/CD4/38- DR+")
  add(gh, rg2, parent = "/not debris/singlets/CD3+/CD4/38- DR+", negated = TRUE)
  add(gh, rg1, parent = "/not debris/singlets/CD3+/CD4/38- DR-")
  add(gh, rg2, parent = "/not debris/singlets/CD3+/CD4/38- DR-", negated = TRUE)
  recompute(gh)
  # Hide a node
  setNode(gh, "/not debris/singlets/CD3+/CD4/38- DR-/HLA-", FALSE)
  
  old.stats <- getPopStats(gh, path = "full")
  old.parent <- getParent(gh, "CD4", path="full")
  old.children <- getChildren(gh, "CD4", path = 1)
  old.descendants <- sub("^.*CD4", "", getDescendants(gh, "CD4"))
  
  new.parent <- "CD4"
  expect_error(copyNode(gh, "CD4", new.parent), "itself")
  new.parent <- "CD4/CCR7- 45RA+"
  expect_error(copyNode(gh, "CD4", new.parent), "descendants")

  new.parent <- "singlets"
  suppressMessages(copyNode(gh, "CD4", new.parent))
  # Make sure everything moved to the new location in the tree
  expect_equal(getParent(gh, "/not debris/singlets/CD4", path = "auto"), new.parent)
  expect_equal(getChildren(gh, "/not debris/singlets/CD4", path = 1), old.children)
  new.descendants <- sub("^.*CD4", "", getDescendants(gh, "/not debris/singlets/CD4", path = "full"))
  expect_equal(new.descendants, old.descendants)
  expect_true(isHidden(gh, "/not debris/singlets/CD4/38- DR-/HLA-"))
  
  # Make sure everything is still unchanged in its original location in the tree
  expect_equal(getParent(gh, "/not debris/singlets/CD3+/CD4", path = "full"), old.parent)
  expect_equal(getChildren(gh, "/not debris/singlets/CD3+/CD4", path = 1), old.children)
  new.descendants <- sub("^.*CD4", "", getDescendants(gh, "/not debris/singlets/CD3+/CD4", path = "full"))
  expect_equal(new.descendants, old.descendants)
  expect_true(isHidden(gh, "/not debris/singlets/CD3+/CD4/38- DR-/HLA-"))
  
  # Make sure the calculations are unaltered on the original branch
  new.stats <- getPopStats(gh, path = "full")
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
  add(gh1, rg1, parent = "/not debris/singlets/CD3+/CD4/38- DR+")
  add(gh1, rg2, parent = "/not debris/singlets/CD3+/CD8/CCR7+ 45RA+", negated = TRUE)
  recompute(gh1)
  # Add those nodes in a different location on another GatingHierarchy
  add(gh2, rg1, parent = "/not debris/singlets/CD3+/CD8/CCR7- 45RA-")
  add(gh2, rg2, parent = "/not debris/singlets/CD3+/CD4/38- DR-", negated = TRUE)
  recompute(gh2)

  # Broadcasting 
  # Maybe make a convenience wrapper for similar logic for the user if it's a common use case
  gh1_bc1 <- c(getChildren(gh1, "CD4", path = "full"), getChildren(gh1, "CD8", path = "full"))
  gh1_bc1 <- gh1_bc1[gh1_bc1!=getParent(gh1, "HLA+", path = "full")]
  gh1_bc2 <- c(getChildren(gh1, "CD4", path = "full"), getChildren(gh1, "CD8", path = "full"))
  gh1_bc2 <- gh1_bc2[gh1_bc2!=getParent(gh1, "HLA-", path = "full")]

  gh2_bc1 <- c(getChildren(gh2, "CD4", path = "full"), getChildren(gh2, "CD8", path = "full"))
  gh2_bc1 <- gh2_bc1[gh2_bc1!=getParent(gh2, "HLA+", path = "full")]
  gh2_bc2 <- c(getChildren(gh2, "CD4", path = "full"), getChildren(gh2, "CD8", path = "full"))
  gh2_bc2 <- gh2_bc2[gh2_bc2!=getParent(gh2, "HLA-", path = "full")]
  
  path1_1 <- getFullNodePath(gh1, "HLA+")
  path1_2 <- getFullNodePath(gh1, "HLA-")
  path2_1 <- getFullNodePath(gh2, "HLA+")
  path2_2 <- getFullNodePath(gh2, "HLA-")
    
  lapply(gh1_bc1, function(x) copyNode(gh1, path1_1, x))
  lapply(gh1_bc2, function(x) copyNode(gh1, path1_2, x))
  lapply(gh2_bc1, function(x) copyNode(gh2, path2_1, x))
  lapply(gh2_bc2, function(x) copyNode(gh2, path2_2, x))
  
  # Verify that the results are the same for different broadcast sources
  stats1 <- getPopStats(gh1, path = "full")[order(node),]
  stats2 <- getPopStats(gh2, path = "full")[order(node),]
  expect_equal(stats1, stats2)
})