context("parallel IO")
skip_if(win32_flag)

gs <- GatingSet(GvHD[c(4,2,3,1)])
tmp <- tempfile()
save_gs(gs, tmp)
sid <- sampleNames(gs)
sid.h5 <- sampleNames(tmp)
test_that("partial load_gs", {
      #load full set
    gs <- load_gs(tmp)
    expect_equal(length(gs), 4)
    expect_equal(sampleNames(gs), sid)
    
    #sub load by sn
    sel <- c(3, 1)
    sid.sel <- sid.h5[sel]
    expect_error(load_gs(tmp, select = c(sid.sel, "dd")), "out of boundary", class = "std::domain_error")
    
    gs1 <- load_gs(tmp, select = sid.sel)
    expect_equal(length(gs1), 2)
    #ordered by select sn
    expect_equal(sampleNames(gs1), sid.sel)
    sn <- sid.sel[1]
    expect_equal(nrow(gh_pop_get_data(gs1[[sn]])), nrow(gh_pop_get_data(gs[[sn]])))
    expect_false(identifier(gs)==identifier(gs1))
    
    #sub by int
    expect_error(load_gs(tmp, select = c(1,5)), "out of boundary")
    
    gs1 <- load_gs(tmp, select = sel)
    #idx is based on sid.h5
    expect_equal(sampleNames(gs1), sid.sel)
    #diff from the h5 file order 
    expect_false(all(sampleNames(gs1) == sid[sel]))
    
    #prove it is real subset
    tmp1 <- tempfile()
    save_gs(gs1, tmp1)
    expect_equal(length(list.files(tmp1, ".h5")), 2)
    expect_equal(length(list.files(tmp1, ".pb")), 2)
})

test_that("parallel load_gs", {

f <- function(i,path){
  gs <- load_gs(path, select = i)
  nrow(gh_pop_get_data(gs[[1]]))
}
expect_equivalent(mclapply(sid, f, path = tmp), lapply(gs[sid], function(gh)nrow(gh_pop_get_data(gh))))

})
