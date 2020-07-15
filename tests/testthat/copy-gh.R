skip_if(win32_flag)
test_that("gh_apply_to_new_fcs",
          {
            fcs <- list.files(dataDir, pattern = "CytoTrol_CytoTrol",full = TRUE)[1]
            cs <- load_cytoset_from_fcs(fcs)
            gs1 <- GatingSet(cs)
            comp <- spillover(cs[[1]])[[1]]
            compensate(gs1, comp)
            channels <- colnames(comp)
            
            #define different trans to test if they are copied correctly in c
            gs <- gs_clone(gs1)
            translist <- list(logtGml2_trans(), logicle_trans(), flowjo_biexp_trans(), asinhtGml2_trans(), logicleGml2_trans())
            trans <- transformerList(channels, translist)
            transform(gs, trans)
            #add gates
            openCyto::gs_add_gating_method(gs, "cd3", "+", "root", "cd3", "mindensity")
            openCyto::gs_add_gating_method(gs, "cd4", "+", "root", "cd4", "mindensity")
            openCyto::gs_add_gating_method(gs, "cd8", "+", "root", "cd8", "mindensity")
            openCyto::gs_add_gating_method(gs, "CD38", "+", "root", "CD38", "mindensity")
            suppressWarnings(openCyto::gs_add_gating_method(gs, "HLA", "+", "root", "HLA", "mindensity"))
            gh <- gs[[1]]
            gs2 <- gh_apply_to_new_fcs(gh, fcs)

            expect_equal(range(gs_cyto_data(gs2)[[1]], "data"), range(gh_pop_get_data(gh), "data"), tol = 6e-8)
            expect_equal(gs_pop_get_count_fast(gs), gs_pop_get_count_fast(gs2))
            expect_equal(gs_get_compensations(gs), gs_get_compensations(gs2))
            expect_equal(gs_get_transformlists(gs), gs_get_transformlists(gs2))
            expect_false(cs_get_uri(gs)==cs_get_uri(gs2))
            
            #add non-supported trans to test trans cp at R level
            gs <- gs_clone(gs1)
            translist <- list(flowjo_fasinh_trans(), logicle_trans(), flowjo_biexp_trans(), asinhtGml2_trans(), logicleGml2_trans())
            trans <- transformerList(channels, translist)
            gs <- transform(gs, trans)
            #add gates
            openCyto::gs_add_gating_method(gs, "cd3", "+", "root", "cd3", "mindensity")
            openCyto::gs_add_gating_method(gs, "cd4", "+", "root", "cd4", "mindensity")
            openCyto::gs_add_gating_method(gs, "cd8", "+", "root", "cd8", "mindensity")
            openCyto::gs_add_gating_method(gs, "CD38", "+", "root", "CD38", "mindensity")
            suppressWarnings(openCyto::gs_add_gating_method(gs, "HLA", "+", "root", "HLA", "mindensity"))
            gh <- gs[[1]]
            gs2 <- gh_apply_to_new_fcs(gh, fcs)

            expect_equal(range(gs_cyto_data(gs2)[[1]], "data"), range(gh_pop_get_data(gh), "data"), tolerance = 1e-6)
            expect_equal(gs_pop_get_count_fast(gs), gs_pop_get_count_fast(gs2))
            expect_equal(gs_get_compensations(gs), gs_get_compensations(gs2))
            expect_equal(gs_get_transformlists(gs), gs_get_transformlists(gs2))
            expect_false(cs_get_uri(gs)==cs_get_uri(gs2))
            
          })

test_that("gh_apply_to_cs", {
  # Run all of the following tests with
  # 1) All trans supported at C-level
  # 2) Unsupported trans to test R level
  translistlist <- list()
  translistlist[[1]] <- list(logtGml2_trans(), logicle_trans(), flowjo_biexp_trans(), asinhtGml2_trans(), logicleGml2_trans())
  translistlist[[2]] <- list(flowjo_fasinh_trans(), logicle_trans(), flowjo_biexp_trans(), asinhtGml2_trans(), logicleGml2_trans())
  
  for(translist in translistlist){
    
    fcs <- list.files(dataDir, pattern = "CytoTrol_CytoTrol",full = TRUE)
    cs <- load_cytoset_from_fcs(fcs[[1]])
    gs_nocomp <- GatingSet(cs)
    comp <- spillover(cs[[1]])[[1]]
    
    gs_normal <- gs_clone(gs_nocomp)
    gs_marked <- gs_clone(gs_nocomp)
    
    compensate(gs_normal, comp)
    # Mark the compensation to track if it is inherited
    comp_marked <- comp
    comp_marked[4,1] <- 0.00042
    compensate(gs_marked, comp_marked)
    channels <- colnames(comp)
    
    trans <- transformerList(channels, translist)
    transform(gs_nocomp, trans)
    transform(gs_normal, trans)
    transform(gs_marked, trans)
    #add gates
    rg <- rectangleGate(filterId="cd3+", "V450-A"=c(0.5, Inf))
    gs_pop_add(gs_nocomp, rg, parent = "root")
    gs_pop_add(gs_normal, rg, parent = "root")
    gs_pop_add(gs_marked, rg, parent = "root")
    rg <- rectangleGate(filterId="cd4+", "B710-A"=c(0.5, Inf))
    gs_pop_add(gs_nocomp, rg, parent = "root")
    gs_pop_add(gs_normal, rg, parent = "root")
    gs_pop_add(gs_marked, rg, parent = "root")
    rg <- rectangleGate(filterId="cd8+", "R780-A"=c(2700, Inf))
    gs_pop_add(gs_nocomp, rg, parent = "root")
    gs_pop_add(gs_normal, rg, parent = "root")
    gs_pop_add(gs_marked, rg, parent = "root")
    rg <- rectangleGate(filterId="CD38+", "R660-A"=c(3, Inf))
    gs_pop_add(gs_nocomp, rg, parent = "root")
    gs_pop_add(gs_normal, rg, parent = "root")
    gs_pop_add(gs_marked, rg, parent = "root")
    rg <- rectangleGate(filterId="HLA+", "V545-A"=c(0.55, Inf))
    gs_pop_add(gs_nocomp, rg, parent = "root")
    gs_pop_add(gs_normal, rg, parent = "root")
    gs_pop_add(gs_marked, rg, parent = "root")
    recompute(gs_nocomp)
    recompute(gs_normal)
    recompute(gs_marked)
    gh_normal <- gs_normal[[1]]
    gh_marked <- gs_marked[[1]]
    
    # Default, should be applying the compensation from each file
    # so this should not pick up the mark
    cs <- load_cytoset_from_fcs(fcs)
    gs2 <- gh_apply_to_cs(gh_marked, cs)
    
    comp_compare <- gs_get_compensations(gs2)[[1]]@spillover
    rownames(comp_compare) <- NULL
    expect_equal(comp_compare, comp)
    expect_equal(range(gs_cyto_data(gs2)[[1]], "data"), range(gh_pop_get_data(gs_normal[[1]]), "data"), tolerance = 1e-6)

    expect_equal(gs_pop_get_count_fast(gs_normal), gs_pop_get_count_fast(gs2[1]))
    expect_equal(gs_get_compensations(gs_normal), gs_get_compensations(gs2[1]))
    expect_equal(gs_get_transformlists(gs_normal), gs_get_transformlists(gs2[1]))
    expect_false(cs_get_uri(gs_marked)==cs_get_uri(gs2))
    
    # In this case, the samples should pick up the mark from the template
    cs <- load_cytoset_from_fcs(fcs)
    gs3 <- gh_apply_to_cs(gh_marked, cs, compensation_source = "template")
    
    comp_compare <- gs_get_compensations(gs3)[[1]]@spillover
    rownames(comp_compare) <- NULL
    expect_equal(comp_compare, comp_marked)
    expect_equal(range(gs_cyto_data(gs3)[[1]], "data"), range(gh_pop_get_data(gs_marked[[1]]), "data"), tolerance = 1e-6)
    expect_equal(gs_pop_get_count_fast(gs_marked), gs_pop_get_count_fast(gs3[1]))
    expect_equal(gs_get_compensations(gs_marked), gs_get_compensations(gs3[1]))
    expect_equal(gs_get_transformlists(gs_marked), gs_get_transformlists(gs3[1]))
    expect_false(cs_get_uri(gs_marked)==cs_get_uri(gs3))
    
    # In this case, the compensation should be null
    cs <- load_cytoset_from_fcs(fcs)
    gs4<- gh_apply_to_cs(gh_marked, cs, compensation_source = "none")
    
    expect_null(gs_get_compensations(gs4)[[1]])
    expect_equal(range(gs_cyto_data(gs4)[[1]], "data"), range(gh_pop_get_data(gs_nocomp[[1]]), "data"), tolerance = 1e-6)
    expect_equal(gs_pop_get_count_fast(gs_nocomp), gs_pop_get_count_fast(gs4[1]))
    expect_equal(gs_get_compensations(gs_nocomp), gs_get_compensations(gs4[1]))
    expect_equal(gs_get_transformlists(gs_nocomp), gs_get_transformlists(gs4[1]))
    expect_false(cs_get_uri(gs_marked)==cs_get_uri(gs4))
  }
  
})
