test_that("gh_apply_to_new_fcs",
          {
            fcs <- list.files(dataDir, pattern = "CytoTrol_CytoTrol",full = TRUE)[1]
            cs <- read.ncdfFlowSet(fcs)
            gs1 <- GatingSet(cs)
            comp <- spillover(cs[[1]])[[1]]
            gs1 <- compensate(gs1, comp)
            channels <- colnames(comp)
            
            #define different trans to test if they are copied correctly in c
            gs <- gs_clone(gs1)
            translist <- list(logtGml2_trans(), logicle_trans(), flowjo_biexp_trans(), asinhtGml2_trans(), logicleGml2_trans())
            trans <- transformerList(channels, translist)
            gs <- transform(gs, trans)
            #add gates
            openCyto::gs_add_gating_method(gs, "cd3", "+", "root", "cd3", "mindensity")
            openCyto::gs_add_gating_method(gs, "cd4", "+", "root", "cd4", "mindensity")
            openCyto::gs_add_gating_method(gs, "cd8", "+", "root", "cd8", "mindensity")
            openCyto::gs_add_gating_method(gs, "CD38", "+", "root", "CD38", "mindensity")
            openCyto::gs_add_gating_method(gs, "HLA", "+", "root", "HLA", "mindensity")
            gh <- gs[[1]]
            gs2 <- gh_apply_to_new_fcs(gh, fcs)
            #exclude time channel since current trunk doesn't perform time scaling in transform method 
            expect_equal(range(gs_cyto_data(gs2)[[1]], "data")[,-12], range(gh_pop_get_data(gh), "data")[,-12], tol = 6e-8)
            expect_equal(gs_pop_get_count_fast(gs), gs_pop_get_count_fast(gs2))
            expect_equal(gs@compensation[[1]], gs2@compensation[[1]]@spillover)
            expect_equal(gh_get_transformations(gs[[1]]), gh_get_transformations(gs2[[1]]))

            
          })
