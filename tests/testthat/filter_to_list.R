context("filterObject (convert flowCore filter to list")
skip_if(win32_flag)

test_that("filterObject from multiRangeGate", {
  mrg <- multiRangeGate(filterId = "dummy_multiRangeGate", ranges=list(min=c(0,50,150),max=c(10,100,Inf)))
  ranges=NULL
  for(i in seq_along(mrg@ranges[["min"]])){
    ranges = c(ranges,mrg@ranges[["min"]][i],mrg@ranges[["max"]][i])
  }
  expectRes <- list(type = as.integer(11),
                    params = parameters(mrg),
                    ranges = ranges,
                    filterId = mrg@filterId)
  expect_equal(filter_to_list(mrg), expectRes)
})

test_that("filterObject from logical", {
  #logical vector
  lg <- rep(TRUE, 10)
  
  expectRes <- list(type = 6L
                    , negated = FALSE
                    , filterId = "dummy_logicalGate"
  )
  expect_equal(filter_to_list(lg), expectRes)
  
  #filterResult
  # rg <- rectangleGate(filterId="myRectGate", "FSC-H"=c(200, 600))
  # fr <- GvHD[[1]]
  # lg <- filter(fr, rg)
  # 
  # expect_equal(filter_to_list(lg), expectRes)
  
})
test_that("filterObject from rectangleGate", {
      #2d gate
      rg <- rectangleGate(filterId="myRectGate", "FSC-H"=c(200, 600), "SSC-H"=c(0, 400))
      expectRes <- list(type = 5L
                        , params = parameters(rg)
                        , boundaries = rbind(rg@min, rg@max)
                        , filterId = rg@filterId
                      )
      expect_equivalent(filter_to_list(rg), expectRes)
      
      #1d gate
      rg <- rectangleGate(filterId="myRectGate", "FSC-H"=c(200, 600))
      expectRes <- list(type = 2L
                        , params = parameters(rg)
                        , range = c(rg@min, rg@max) 
                        , filterId = rg@filterId
                    )
      expect_equivalent(filter_to_list(rg), expectRes)
      
    })

test_that("filterObject from polygonGate", {
      
      sqrcut <- matrix(c(300,300,600,600,50,300,300,50),ncol=2,nrow=4)
      colnames(sqrcut) <- c("FSC-H","SSC-H")
      pg <- polygonGate(filterId="nonDebris", .gate = sqrcut)
      
      
      expectRes <- list(type = 1L
                        , params = parameters(pg)
                        , boundaries = pg@boundaries
                        , filterId = pg@filterId
                        ) 
          
          
      expect_equal(filter_to_list(pg), expectRes)
      
    })

test_that("filterObject from booleanFilter", {
      expectRes <- list(type = 3L
                        , refs = c("IL2", "TNFa", "IFNg")
                        , isNot = c(FALSE, FALSE, TRUE)
                        , op = c("&", "&", "&")
                      ) 
      
      bf <- booleanFilter(IL2&TNFa&!IFNg)#without filterId
      expectRes[["filterId"]] <- bf@filterId
      expect_equal(filter_to_list(bf), expectRes)
      
      
      bf <- booleanFilter(IL2&TNFa&!IFNg , filterId = "myBoolFilter")
      expectRes[["filterId"]] <- bf@filterId
      expect_equal(filter_to_list(bf), expectRes)
      
      
      #with node path
      bf <- booleanFilter(cd4/IL2&cd4/TNFa&!cd4/IFNg , filterId = "myBoolFilter")
      expectRes$refs <- file.path("cd4", expectRes$refs) 
      expect_equal(filter_to_list(bf), expectRes)
      
      #extra ! symbol
      bf <- booleanFilter(!IL2&!!TNFa&!IFNg , filterId = "myBoolFilter")
      expect_error(filter_to_list(bf), "extra '!' symbol found in the reference node names of boolean fitler")
      
      #extra & symbol
      bf <- booleanFilter(IL2&&TNFa&!IFNg , filterId = "myBoolFilter")
      expect_error(filter_to_list(bf), "'&&' or '||' found in the expression of booleanFilter")

      #extra & symbol
      bf <- booleanFilter(IL2||TNFa&!IFNg , filterId = "myBoolFilter")
      expect_error(filter_to_list(bf), "double operater")
      
      
    })
