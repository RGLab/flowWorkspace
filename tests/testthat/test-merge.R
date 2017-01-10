context("merg/standardize GatingSets")

gs0 <- load_gs(file.path(dataDir,"gs_manual"))
gs1 <- clone(gs0)
sampleNames(gs1) <- "1.fcs"

# simply the tree
nodes <- getNodes(gs1)
for(toRm in nodes[grepl("CCR", nodes)])
  Rm(toRm, gs1)

# remove two terminal nodes
gs2 <- clone(gs1)
sampleNames(gs2) <- "2.fcs"
#create a merged gs
gs6 <- rbind2(GatingSetList(list(gs1, gs2)))
Rm("DPT", gs6[[1]])
Rm("DNT", gs6[[1]])

Rm("DPT", gs2)
Rm("DNT", gs2)

# remove singlets gate
gs3 <- clone(gs2)
Rm("singlets", gs3)
add(gs3, getGate(gs2, "CD3+"), parent = "not debris")
for(tsub in c("CD4", "CD8"))
{
  add(gs3, getGate(gs2, tsub), parent = "CD3+")
  for(toAdd in getChildren(gs2, tsub))
  {
    thisParent <- getParent(gs2[[1]], toAdd, path = "auto")
    add(gs3, getGate(gs2, toAdd), parent = thisParent)
  }
}
sampleNames(gs3) <- "3.fcs"

# spin the branch to make it isomorphic
gs4 <- clone(gs3)
# rm cd4 branch first
Rm("CD4", gs4)
# add it back
add(gs4, getGate(gs3, "CD4"), parent = "CD3+")
# add all the chilren back
for(toAdd in getChildren(gs3, "CD4"))
{
  thisParent <- getParent(gs3[[1]], toAdd)
  add(gs4, getGate(gs3, toAdd), parent = thisParent)
}
sampleNames(gs4) <- "4.fcs"

gs5 <- clone(gs4)
# add another redundant node
add(gs5, getGate(gs0, "CD4/CCR7+ 45RA+")[[1]], parent = "CD4")
add(gs5, getGate(gs0, "CD4/CCR7+ 45RA-")[[1]], parent = "CD4")
sampleNames(gs5) <- "5.fcs"


gs_groups <- NULL
gslist <- list(gs1, gs2, gs3, gs4, gs5)
test_that("groupByTree", {
  
  gs_groups <<- groupByTree(gslist)
  expect_equal(length(gs_groups), 4)
  
})

toRm <- NULL
test_that("checkRedundantNodes", {
  expect_error(checkRedundantNodes(gs_groups), "Can't drop the non-terminal nodes: singlets")
  for(i in c(2,4))
    for(gs in gs_groups[[i]])
      invisible(setNode(gs, "singlets", FALSE))
  toRm <<- checkRedundantNodes(gs_groups)
  expect_equal(toRm, list(c("CCR7+ 45RA+", "CCR7+ 45RA-")
                          , c("DNT", "DPT")
                          , character(0)
                          , character(0))
               )
  
  
})

test_that("dropRedundantNodes", {
  dropRedundantNodes(gs_groups, toRm)
  expect_equal(length(groupByTree(gslist)), 1)
})

test_that("dropRedundantChannels", {
  gs1 <- dropRedundantChannels(gs1)
  expect_equal(setdiff(colnames(gs0), colnames(gs1)), c("FSC-H", "FSC-W", "<G560-A>", "<G780-A>", "Time"))
})

test_that("group and merge the GatingSet object", {
  #test gs version
  gs_groups <- groupByTree(gs6)
  expect_equal(length(gs_groups), 2)
  toRm <- checkRedundantNodes(gs_groups)
  expect_equal(toRm, list(c("DNT", "DPT"), character(0)))
  dropRedundantNodes(gs_groups, toRm)
  expect_equal(length(groupByTree(gs6)), 1)
  
})