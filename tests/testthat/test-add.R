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
  gs_pop_add(gs, rg)
  recompute(gs)
  
  expect_equal(gs_get_pop_paths(gs), c("root", "/rectangle"))
  expect_equal(gh_pop_get_stats(gs[[1]], node)[[2]], 649)
  expect_equivalent(gh_pop_get_gate(gs[[1]], node), rg)
})


test_that("add quadGate", {
  qg <- quadGate("FL1-H"= 2, "FL2-H"= 3)
  gs_pop_add(gs,qg,parent="rectangle")
  expect_equal(gs_get_pop_paths(gs), c("root", "/rectangle"
                               , "/rectangle/CD15 FITC-CD45 PE+"
                               , "/rectangle/CD15 FITC+CD45 PE+"
                               , "/rectangle/CD15 FITC+CD45 PE-"
                               , "/rectangle/CD15 FITC-CD45 PE-"
                               ))
  recompute(gs)
  expect_equal(gh_pop_get_stats(gs[[1]], "CD15 FITC-CD45 PE+")[[2]], 155)
})


test_that("add filterResult", {
  
  g <- gs_pop_get_gate(gs, "CD15 FITC-CD45 PE+")
  fs <- gs_pop_get_data(gs, "rectangle")
  fres <- filter(fs, g)
  expect_error(gs_pop_add(gs, fres, name = "g1", parent = "root"), "does not match to the parent")
  
  #local ind (relative to parent)
  gs_pop_add(gs, fres, name = "g1", parent = "rectangle")
  
  expect_equal(gs_get_pop_paths(gs)[7], "/rectangle/g1")
  expect_equal(gh_pop_get_stats(gs[[1]], "g1")[[2]], 155)
  gs_pop_remove(gs, "g1")
  
})

test_that("add logical vector", {
  #local indice (relative to parent)
  g <- gs_pop_get_gate(gs, "CD15 FITC-CD45 PE+")
  fs <- gs_pop_get_data(gs, "rectangle")
  fres <- filter(fs, g)
  ind <- lapply(fres, slot, "subSet")
  expect_error(gs_pop_add(gs, ind, name = "g1", parent = "root"), "does not match to the parent")
  gs_pop_add(gs, ind, name = "g1", parent = "rectangle")
  
  expect_equal(gs_get_pop_paths(gs)[7], "/rectangle/g1")
  expect_equal(gh_pop_get_stats(gs[[1]], "g1")[[2]], 155)
  gs_pop_remove(gs, "g1")
  
#global indice (relative to root)  
  ind <- lapply(gs, function(gh)gh_pop_get_indices(gh, "CD15 FITC-CD45 PE+"))
  gs_pop_add(gs, ind, name = "g1", parent = "rectangle")
  expect_is(gh_pop_get_gate(gs[[1]], "g1"), "booleanFilter")
  expect_equal(gs_get_pop_paths(gs)[7], "/rectangle/g1")
  expect_equal(gh_pop_get_stats(gs[[1]], "g1")[[2]], 155)
  gs_pop_remove(gs, "g1")
})

test_that("add factor vector", {
  
  fac.list <- lapply(gs, function(gh){
    ind1 <- gh_pop_get_indices(gh, "CD15 FITC-CD45 PE+")
    ind2 <- gh_pop_get_indices(gh, "CD15 FITC+CD45 PE+")
    ind3 <- gh_pop_get_indices(gh, "CD15 FITC+CD45 PE-")  
    ind4 <- gh_pop_get_indices(gh, "CD15 FITC-CD45 PE-") 
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
  
  
  expect_error(gs_pop_add(gs, fac.list, parent = "rectangle"), "Must specify the name of the cluster method through 'name' argument")
  gs_pop_add(gs, fac.list, parent = "rectangle", name = "clusterA")
  
  expect_equal(gs_get_pop_paths(gs)[7:10], c("/rectangle/clusterA_Q1", "/rectangle/clusterA_Q2"
                                     , "/rectangle/clusterA_Q3", "/rectangle/clusterA_Q5"))
  expect_equal(gh_pop_get_count(gs[[1]], "clusterA_Q1"), gh_pop_get_count(gs[[1]], "CD15 FITC-CD45 PE+"))
  expect_equal(gh_pop_get_count(gs[[1]], "clusterA_Q2"), gh_pop_get_count(gs[[1]], "CD15 FITC+CD45 PE+"))
  expect_equal(gh_pop_get_count(gs[[1]], "clusterA_Q3"), gh_pop_get_count(gs[[1]], "CD15 FITC+CD45 PE-"))
  expect_equal(gh_pop_get_count(gs[[1]], "clusterA_Q5"),0)
  
  expect_error(gs_pop_add(gs, fac.list, parent = "rectangle", name = "clusterA")
               , "exist")
  
  #retrieve clustering results
  expect_error(gh_get_cluster_labels(gs[[1]], "rectangle", "cluster"), "No clustering results")
  labels <- gh_get_cluster_labels(gs[[1]], "rectangle", "clusterA")
  
  pind <- gh_pop_get_indices(gs[[1]], "rectangle")
  expect_true(all(is.na(labels[!pind])))
  
  labels <- labels[pind]#convert to local
  
  expect_equal(labels, fac.list[[1]])
  
  gs_pop_remove(gs, "clusterA_Q1")
  gs_pop_remove(gs, "clusterA_Q2")
  gs_pop_remove(gs, "clusterA_Q3")
  
  gs_pop_remove(gs, "clusterA_Q5")
  
  
  })

test_that("add boolean filter", {
  #relative ref path
  or_node <- "test_or"
  bf <- booleanFilter(`CD15 FITC-CD45 PE+|CD15 FITC-CD45 PE-`)
  gs_pop_add(gs, bf, name = or_node, parent = "/rectangle")
  recompute(gs)
  expect_equal(gh_pop_get_count(gs[[1]], or_node), 561)
  gs_pop_remove(gs, or_node)
  
  #abs path
  bf <- booleanFilter(`/rectangle/CD15 FITC-CD45 PE+|/rectangle/CD15 FITC-CD45 PE-`)
  gs_pop_add(gs, bf, name = or_node, parent = "/rectangle")
  recompute(gs)
  expect_equal(gh_pop_get_count(gs[[1]], or_node), 561)
  gs_pop_remove(or_node, gs = gs)
  
  #abs path
  gs_pop_set_name(gs, "CD15 FITC-CD45 PE+", "Q1")
  gs_pop_set_name(gs, "CD15 FITC-CD45 PE-", "Q4")
  bf <- booleanFilter(Q1|rectangle/Q4)
  gs_pop_add(gs, bf, name = or_node, parent = "/rectangle")
  recompute(gs)
  expect_equal(gh_pop_get_count(gs[[1]], or_node), 561)
  
  
  #abs path
  expect_error(bf <- booleanFilter("/rectangle/CD15 FITC-CD45 PE+|/rectangle/CD15 FITC-CD45 PE-"), "character")
  })
