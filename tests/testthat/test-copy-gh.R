test_that("gh_apply_to_new_fcs",
          {
            fcs <- list.files(dataDir, pattern = "CytoTrol_CytoTrol",full = TRUE)[1]
            cs <- load_cytoset_from_fcs(fcs, is_h5 = TRUE)
            gs <- GatingSet(cs)
            comp <- spillover(cs[[1]])[[1]]
            compensate(gs, comp)
            channels <- colnames(comp)
            #define different trans to test if they are copied correctly in c
            trans <- list(logtGml2_trans(), logicle_trans(), flowjo_biexp_trans(), asinhtGml2_trans(), logicleGml2_trans())
            trans <- transformerList(channels, trans)
            transform(gs, trans)
            #add gates
            openCyto::gs_add_gating_method(gs, "cd3", "+", "root", "cd3", "mindensity")
            openCyto::gs_add_gating_method(gs, "cd4", "+", "root", "cd4", "mindensity")
            openCyto::gs_add_gating_method(gs, "cd8", "+", "root", "cd8", "mindensity")
            openCyto::gs_add_gating_method(gs, "CD38", "+", "root", "CD38", "mindensity")
            openCyto::gs_add_gating_method(gs, "HLA", "+", "root", "HLA", "mindensity")
            gh <- gs[[1]]
            gs1 <- gh_apply_to_new_fcs(gh, fcs)
            expect_equal(gs_pop_get_count_fast(gs), gs_pop_get_count_fast(gs1))
            expect_equal(gs_get_compensations(gs), gs_get_compensations(gs1))
            expect_equal(gs_get_transformlists(gs), gs_get_transformlists(gs1))
            expect_false(cs_get_h5_file_path(gs)==cs_get_h5_file_path(gs1))
          })