context("add method")
fs <- GvHD[1:2]
cfile <- system.file("extdata","compdata","compmatrix", package="flowCore")
comp.mat <- read.table(cfile, header=TRUE, skip=2, check.names = FALSE)
## create a compensation object 
comp <- compensation(comp.mat)
chnls <- parameters(comp)

gs <- GatingSet(fs)
#compensate GatingSet
gs <- compensate(gs, comp)
transList <- estimateLogicle(gs[[2]], chnls)
gs <- transform(gs, transList)


test_that("add rectangleGate", {
  node <- "rectangle"
  rg <- rectangleGate("FSC-H"=c(500,300), "SSC-H"=c(0, 400), filterId=node)
  add(gs, rg)
  recompute(gs)
  
  expect_equal(getNodes(gs), c("root", "/rectangle"))
  #TODO: somehow R method has less cells, which could be due to the edge cells and need to be investigated
  cnt <- nrow(Subset(getData(gs[[1]], "root"), rg)) + 2
  expect_equal(getTotal(gs[[1]], node), cnt)
  expect_equivalent(getGate(gs[[1]], node), rg)
})


test_that("add quadGate", {
  qg <- quadGate("FL1-H"= 2, "FL2-H"= 3)
  add(gs,qg,parent="rectangle")
  expect_equal(getNodes(gs), c("root", "/rectangle"
                               , "/rectangle/CD15 FITC-CD45 PE+"
                               , "/rectangle/CD15 FITC+CD45 PE+"
                               , "/rectangle/CD15 FITC+CD45 PE-"
                               , "/rectangle/CD15 FITC-CD45 PE-"
                               ))
  recompute(gs)
  node <- "CD15 FITC-CD45 PE+"
  expect_equal(getTotal(gs[[1]], node), sum((getData(gs[[1]], "rectangle")%in%qg) == node))
})

#restore filter method during debug mode
filter <- flowCore::filter#it is masked by dplyr during load_all()

test_that("add filterResult", {
  
  g <- getGate(gs, "CD15 FITC-CD45 PE+")
  pnode <- "rectangle"
  fs <- getData(gs, pnode)
  fres <- filter(fs, g)
  expect_error(add(gs, fres, name = "g1", parent = "root"), "does not match to the parent")
  
    #local ind (relative to parent)
  add(gs, fres, name = "g1", parent = pnode)
  
  expect_equal(getNodes(gs)[7], "/rectangle/g1")
  expect_equal(getTotal(gs[[1]], "g1"), nrow(Subset(getData(gs[[1]], pnode), fres[[1]])))
  Rm("g1", gs)
  
})

test_that("add logical vector", {
  #local indice (relative to parent)
  g <- getGate(gs, "CD15 FITC-CD45 PE+")
  pnode <- "rectangle"
  fs <- getData(gs, pnode)
  fres <- filter(fs, g)
  ind <- lapply(fres, slot, "subSet")
  expect_error(add(gs, ind, name = "g1", parent = "root"), "does not match to the parent")
  add(gs, ind, name = "g1", parent = pnode)
  
  expect_equal(getNodes(gs)[7], "/rectangle/g1")
  expect_equal(getTotal(gs[[1]], "g1"), nrow(Subset(getData(gs[[1]], pnode), fres[[1]])))
  Rm("g1", gs)
  
#global indice (relative to root)  
  ind <- lapply(gs, function(gh)getIndices(gh, "CD15 FITC-CD45 PE+"))
  add(gs, ind, name = "g1", parent = pnode)
  expect_is(getGate(gs[[1]], "g1"), "booleanFilter")
  expect_equal(getNodes(gs)[7], "/rectangle/g1")
  expect_equal(getTotal(gs[[1]], "g1"), sum(ind[[1]]))
  Rm("g1", gs)
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
  
  
  expect_error(add(gs, fac.list, parent = "rectangle"), "Must specify the name of the cluster method through 'name' argument")
  add(gs, fac.list, parent = "rectangle", name = "clusterA")
  
  expect_equal(getNodes(gs)[7:10], c("/rectangle/clusterA_Q1", "/rectangle/clusterA_Q2"
                                     , "/rectangle/clusterA_Q3", "/rectangle/clusterA_Q5"))
  expect_equal(getTotal(gs[[1]], "clusterA_Q1"), getTotal(gs[[1]], "CD15 FITC-CD45 PE+"))
  expect_equal(getTotal(gs[[1]], "clusterA_Q2"), getTotal(gs[[1]], "CD15 FITC+CD45 PE+"))
  expect_equal(getTotal(gs[[1]], "clusterA_Q3"), getTotal(gs[[1]], "CD15 FITC+CD45 PE-"))
  expect_equal(getTotal(gs[[1]], "clusterA_Q5"),0)
  
  expect_error(add(gs, fac.list, parent = "rectangle", name = "clusterA")
               , "exist")
  
  #retrieve clustering results
  expect_error(gh_get_cluster_labels(gs[[1]], "rectangle", "cluster"), "No clustering results")
  labels <- gh_get_cluster_labels(gs[[1]], "rectangle", "clusterA")
  
  pind <- getIndices(gs[[1]], "rectangle")
  expect_true(all(is.na(labels[!pind])))
  
  labels <- labels[pind]#convert to local
  
  expect_equal(labels, fac.list[[1]])
  
  Rm("clusterA_Q1", gs)
  Rm("clusterA_Q2", gs)
  Rm("clusterA_Q3", gs)
  
  Rm("clusterA_Q5", gs)
  
  
  })

test_that("add boolean filter", {
  #relative ref path
  or_node <- "test_or"
  pnode <- "/rectangle"
  bf <- booleanFilter(`CD15 FITC-CD45 PE+|CD15 FITC-CD45 PE-`)
  add(gs, bf, name = or_node, parent = pnode)
  recompute(gs)
  cnt <- sum(getIndices(gs[[1]] , "CD15 FITC-CD45 PE+")|getIndices(gs[[1]] , "CD15 FITC-CD45 PE-"))
  expect_equal(getTotal(gs[[1]], or_node), cnt)
  Rm(or_node, gs)
  
  #abs path
  bf <- booleanFilter(`/rectangle/CD15 FITC-CD45 PE+|/rectangle/CD15 FITC-CD45 PE-`)
  add(gs, bf, name = or_node, parent = pnode)
  recompute(gs)
  expect_equal(getTotal(gs[[1]], or_node), cnt)
  Rm(or_node, gs)
  
  #abs path
  setNode(gs, "CD15 FITC-CD45 PE+", "Q1")
  setNode(gs, "CD15 FITC-CD45 PE-", "Q4")
  bf <- booleanFilter(Q1|rectangle/Q4)
  add(gs, bf, name = or_node, parent = pnode)
  recompute(gs)
  expect_equal(getTotal(gs[[1]], or_node), cnt)
  
  
  #abs path
  expect_error(bf <- booleanFilter("/rectangle/CD15 FITC-CD45 PE+|/rectangle/CD15 FITC-CD45 PE-"), "character")
  })
