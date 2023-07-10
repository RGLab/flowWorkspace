context("add method")
skip_if(win32_flag)
fs <- GvHD[1:2]
cfile <- system.file("extdata","compdata","compmatrix", package="flowCore")
comp.mat <- read.table(cfile, header=TRUE, skip=2, check.names = FALSE)
## create a compensation object 
comp <- compensation(comp.mat)
chnls <- parameters(comp)
#a hack to resolve the discrepancy between range slot and PnR keyword
#so that the values in range slot are preserved during write.fcs/load_fcs
for(i in seq_along(colnames(fs)))
{  
  rid <- paste("$P", i,"R",sep="")
  # newr <- paste("flowCore_$P", i,"Rmax",sep="")
  keyword(fs[[1]])[[rid]] <- range(fs[[1]])[2,i]
}


gs <- GatingSet(fs)
#compensate GatingSet
gs <- compensate(gs, comp)
transList <- estimateLogicle(gs[[1]], chnls)
gs <- transform(gs, transList)

test_that("add multiRangeGate", {
  node <- "multirange"
  mrg <- multiRangeGate(filterId = node,ranges=list(min=c(0,60,120,131),max=c(10,100,130,Inf)))
  gs_pop_add(gs, mrg, negated=TRUE)
  recompute(gs)
  expect_equal(gs_get_pop_paths(gs), c("root", "/multirange"))
  expect_equivalent(gh_pop_get_gate(gs[[1]], node), mrg)
  expect_equal(gh_pop_get_stats(gs[[1]], node)[[2]], 480)
  expect_equal(sum(gh_pop_get_indices(gs[[1]], node)),480)
  expect_equal(sum(!flowFrame(exprs(gh_pop_get_data(gs[[1]])))%in%mrg),480)
  gs_pop_remove(gs,node)
})

test_that("add rectangleGate", {
  node <- "rectangle"
  rg <- rectangleGate("FSC-H"=c(500,300), "SSC-H"=c(0, 400), filterId=node)
  gs_pop_add(gs, rg)
  recompute(gs)
  
  expect_equal(gs_get_pop_paths(gs), c("root", "/rectangle"))
  expect_equal(gh_pop_get_stats(gs[[1]], node)[[2]], 649)
  expect_equivalent(gh_pop_get_gate(gs[[1]], node), rg)
})

test_that("test get_data methods with inverse.transform=TRUE", {
  
  expect_equivalent(exprs(compensate(fs[[1]], comp)), exprs(gs_cyto_data(gs, inverse.transform=TRUE)[[1]]), tol = 6-4)
  expect_equivalent(exprs(compensate(fs[[2]], comp)), exprs(gs_cyto_data(gs, inverse.transform=TRUE)[[2]]), tol = 6-4)
  expect_equivalent(exprs(compensate(fs[[1]], comp)), exprs(gs_cyto_data(gs[[1]], inverse.transform=TRUE)[[1]]), tol = 6-4)

  fr_pre <- gh_pop_get_data(gs[[1]], inverse.transform = TRUE)
  expect_equivalent(exprs(compensate(fs[[1]], comp)), exprs(fr_pre), tol = 6-4)
  fr_pre1 <- gh_pop_get_data(gs[[1]], "rectangle", inverse.transform = TRUE)
  fr_pre2 <- gh_pop_get_data(gs[[1]], "rectangle")
  expect_condition(!all.equal(exprs(fr_pre1),exprs(fr_pre2)))

  fs_pre <- gs_pop_get_data(gs, inverse.transform = TRUE)
  expect_equivalent(exprs(compensate(fs[[1]], comp)), exprs(fs_pre[[1]]), tol = 6-4)
  expect_equivalent(exprs(compensate(fs[[2]], comp)), exprs(fs_pre[[2]]), tol = 6-4)
  fs_pre1 <- gs_pop_get_data(gs, "rectangle", inverse.transform = TRUE)
  fs_pre2 <- gs_pop_get_data(gs, "rectangle")
  expect_condition(!all.equal(exprs(fs_pre1),exprs(fs_pre2)))
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
  node <- "CD15 FITC-CD45 PE+"
  expect_equal(gh_pop_get_stats(gs[[1]], node)[, count], sum((gh_pop_get_data(gs[[1]], "rectangle")%in%qg) == node))
  g <- gh_pop_get_gate(gs[[1]], "CD15 FITC-CD45 PE+")
  expect_is(g, "rectangleGate")
  #check the attached quadgate info
  expect_equal(attr(g, "quadintersection"), c("FL1-H"= 2, "FL2-H"= 3))
  quadrants <- 1:4
  names(quadrants) <- gh_pop_get_children(gs[[1]], "rectangle", path = "auto")
  expect_equal(attr(g, "quadrants"), quadrants)
  
  #test the case where cells on the intersection lines
  qg2 <- quadGate(list(`FSC-H` = 500, `SSC-H` = 600))
  gs_pop_add(gs, qg2, names = c("A", "B", "C", "D"))
  recompute(gs)
  expect_equal(gh_pop_get_count(gs[[1]], "root"), gh_pop_get_stats(gs[[1]], c("A", "B", "C", "D"))[,sum(count)])
  #save and load
  tmp <- tempfile()
  save_gs(gs, tmp)
  gs <- load_gs(tmp)
  recompute(gs)
  expect_equal(gh_pop_get_count(gs[[1]], "root"), gh_pop_get_stats(gs[[1]], c("A", "B", "C", "D"))[,sum(count)])
  
  #set quadgate
  qg2@boundary[[1]] <- 200
  expect_error(gh_pop_set_gate(gs[[1]], c("A"), qg), "do not match")
  expect_error(gh_pop_set_gate(gs[[1]], c("rectangle"), qg), "not a quadGate")
  gh_pop_set_gate(gs[[1]], c("A"), qg2)
  g1 <- gs_pop_get_gate(gs, "A")[[1]]
  g2 <- rectangleGate(list(`FSC-H`= c(-Inf, 200), `SSC-H` = c(600, Inf)), filterId = "A")
  expect_equal(g1@min, g2@min)
  expect_equal(g1@max, g2@max)
  g1 <- gs_pop_get_gate(gs, "B")[[1]]
  g2 <- rectangleGate(list(`FSC-H`= c(200, Inf), `SSC-H` = c(600, Inf)), filterId = "A")
  expect_equal(g1@min, g2@min)
  expect_equal(g1@max, g2@max)
  
    })
gs_pop_remove(gs, "A")
gs_pop_remove(gs, "B")
gs_pop_remove(gs, "C")
gs_pop_remove(gs, "D")
#restore filter method during debug mode
filter <- flowCore::filter#it is masked by dplyr during load_all()

test_that("add filterResult", {
  
  g <- gs_pop_get_gate(gs, "CD15 FITC-CD45 PE+")
  pnode <- "rectangle"
  fs <- gs_pop_get_data(gs, pnode)
  fres <- filter(fs, g)
  expect_error(gs_pop_add(gs, fres, name = "g1", parent = "root"), "does not match to the parent")
  
    #local ind (relative to parent)
	gs_pop_add(gs, fres, name = "g1", parent = pnode)
  
  expect_equal(gs_get_pop_paths(gs)[7], "/rectangle/g1")
  expect_equal(gh_pop_get_stats(gs[[1]], "g1")[, count], nrow(Subset(gh_pop_get_data(gs[[1]], pnode), fres[[1]])))
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
               , "exist", class = "error")
  
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
  pnode <- "/rectangle"
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

