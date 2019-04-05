context("add method")
fs <- GvHD[1:2]
gs <- GatingSet(fs)
cfile <- system.file("extdata","compdata","compmatrix", package="flowCore")
comp.mat <- read.table(cfile, header=TRUE, skip=2, check.names = FALSE)
## create a compensation object 
comp <- compensation(comp.mat)
#compensate GatingSet
gs <- compensate(gs, comp)
chnls <- parameters(comp)
transList <- estimateLogicle(gs[[1]], chnls)
gs <- transform(gs, transList)


test_that("add rectangleGate", {
  node <- "rectangle"
  rg <- rectangleGate("FSC-H"=c(500,300), "SSC-H"=c(0, 400), filterId=node)
  gs_add_gate(gs, rg)
  recompute(gs)
  
  expect_equal(gs_get_pop_paths(gs), c("root", "/rectangle"))
  expect_equal(gh_get_count(gs[[1]], node), 649)
  expect_equivalent(gh_get_gate(gs[[1]], node), rg)
})


test_that("add quadGate", {
  qg <- quadGate("FL1-H"= 2, "FL2-H"= 3)
  gs_add_gate(gs,qg,parent="rectangle")
  expect_equal(gs_get_pop_paths(gs), c("root", "/rectangle"
                               , "/rectangle/CD15 FITC-CD45 PE+"
                               , "/rectangle/CD15 FITC+CD45 PE+"
                               , "/rectangle/CD15 FITC+CD45 PE-"
                               , "/rectangle/CD15 FITC-CD45 PE-"
                               ))
  recompute(gs)
  expect_equal(gh_get_count(gs[[1]], "CD15 FITC-CD45 PE+"), 155)
})


test_that("add filterResult", {
  
  g <- gs_get_gate(gs, "CD15 FITC-CD45 PE+")
  fs <- getData(gs, "rectangle")
  fres <- filter(fs, g)
  expect_error(gs_add_gate(gs, fres, name = "g1", parent = "root"), "does not match to the parent")
  
  #local ind (relative to parent)
  gs_add_gate(gs, fres, name = "g1", parent = "rectangle")
  
  expect_equal(gs_get_pop_paths(gs)[7], "/rectangle/g1")
  expect_equal(gh_get_count(gs[[1]], "g1"), 155)
  gs_remove_gate("g1", gs)
  
})

test_that("add logical vector", {
  #local indice (relative to parent)
  g <- gs_get_gate(gs, "CD15 FITC-CD45 PE+")
  fs <- getData(gs, "rectangle")
  fres <- filter(fs, g)
  ind <- lapply(fres, slot, "subSet")
  expect_error(gs_add_gate(gs, ind, name = "g1", parent = "root"), "does not match to the parent")
  gs_add_gate(gs, ind, name = "g1", parent = "rectangle")
  
  expect_equal(gs_get_pop_paths(gs)[7], "/rectangle/g1")
  expect_equal(gh_get_count(gs[[1]], "g1"), 155)
  gs_remove_gate("g1", gs)
  
#global indice (relative to root)  
  ind <- lapply(gs, function(gh)getIndices(gh, "CD15 FITC-CD45 PE+"))
  gs_add_gate(gs, ind, name = "g1", parent = "rectangle")
  expect_is(gh_get_gate(gs[[1]], "g1"), "booleanFilter")
  expect_equal(gs_get_pop_paths(gs)[7], "/rectangle/g1")
  expect_equal(gh_get_count(gs[[1]], "g1"), 155)
  gs_remove_gate("g1", gs)
})

test_that("add factor vector", {
  
  fac.list <- lapply(gs, function(gh){
    ind1 <- getIndices(gh, "CD15 FITC-CD45 PE+")
    ind2 <- getIndices(gh, "CD15 FITC+CD45 PE+")
    ind3 <- getIndices(gh, "CD15 FITC+CD45 PE-")  
    ind4 <- getIndices(gh, "CD15 FITC-CD45 PE-") 
    #make a factor vector (mimic a clustering result)
    vec <- character(length(ind1))#global
    vec[ind1] <- "Q1"
    vec[ind2] <- "Q2"
    vec[ind3] <- "Q3"
    vec[ind4] <- "Q4"
    #convert to local ind relative to parent
    vec <- vec[vec!=""]
    vec[vec == "Q4"] <- NA#mimic NA vals in factor
    vec <- as.factor(vec)
    
    #add empty pop by adding levels
    levels(vec) <- c(levels(vec) , "Q5")
    vec
  })
  
  
  expect_error(gs_add_gate(gs, fac.list, parent = "rectangle"), "Must specify the name of the cluster method through 'name' argument")
  gs_add_gate(gs, fac.list, parent = "rectangle", name = "clusterA")
  
  expect_equal(gs_get_pop_paths(gs)[7:10], c("/rectangle/clusterA_Q1", "/rectangle/clusterA_Q2"
                                     , "/rectangle/clusterA_Q3", "/rectangle/clusterA_Q5"))
  expect_equal(gh_get_count(gs[[1]], "clusterA_Q1"), gh_get_count(gs[[1]], "CD15 FITC-CD45 PE+"))
  expect_equal(gh_get_count(gs[[1]], "clusterA_Q2"), gh_get_count(gs[[1]], "CD15 FITC+CD45 PE+"))
  expect_equal(gh_get_count(gs[[1]], "clusterA_Q3"), gh_get_count(gs[[1]], "CD15 FITC+CD45 PE-"))
  expect_equal(gh_get_count(gs[[1]], "clusterA_Q5"),0)
  
  expect_error(gs_add_gate(gs, fac.list, parent = "rectangle", name = "clusterA")
               , "exist")
  
  #retrieve clustering results
  expect_error(gh_get_cluster_labels(gs[[1]], "rectangle", "cluster"), "No clustering results")
  labels <- gh_get_cluster_labels(gs[[1]], "rectangle", "clusterA")
  
  pind <- getIndices(gs[[1]], "rectangle")
  expect_true(all(is.na(labels[!pind])))
  
  labels <- labels[pind]#convert to local
  
  expect_equal(labels, fac.list[[1]])
  
  gs_remove_gate("clusterA_Q1", gs)
  gs_remove_gate("clusterA_Q2", gs)
  gs_remove_gate("clusterA_Q3", gs)
  
  gs_remove_gate("clusterA_Q5", gs)
  
  
  })

test_that("add boolean filter", {
  #relative ref path
  or_node <- "test_or"
  bf <- booleanFilter(`CD15 FITC-CD45 PE+|CD15 FITC-CD45 PE-`)
  gs_add_gate(gs, bf, name = or_node, parent = "/rectangle")
  recompute(gs)
  expect_equal(gh_get_count(gs[[1]], or_node), 561)
  gs_remove_gate(or_node, gs)
  
  #abs path
  bf <- booleanFilter(`/rectangle/CD15 FITC-CD45 PE+|/rectangle/CD15 FITC-CD45 PE-`)
  gs_add_gate(gs, bf, name = or_node, parent = "/rectangle")
  recompute(gs)
  expect_equal(gh_get_count(gs[[1]], or_node), 561)
  gs_remove_gate(or_node, gs)
  
  #abs path
  setNode(gs, "CD15 FITC-CD45 PE+", "Q1")
  setNode(gs, "CD15 FITC-CD45 PE-", "Q4")
  bf <- booleanFilter(Q1|rectangle/Q4)
  gs_add_gate(gs, bf, name = or_node, parent = "/rectangle")
  recompute(gs)
  expect_equal(gh_get_count(gs[[1]], or_node), 561)
  
  
  #abs path
  expect_error(bf <- booleanFilter("/rectangle/CD15 FITC-CD45 PE+|/rectangle/CD15 FITC-CD45 PE-"), "character")
  })
