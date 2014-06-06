context("filterObject (convert flowCore filter to list")

test_that("filterObject from rectangleGate", {
      #2d gate
      rg <- rectangleGate(filterId="myRectGate", "FSC-H"=c(200, 600), "SSC-H"=c(0, 400))
      expectRes <- list(type = 5L
                        , params = parameters(rg)
                        , boundaries = rbind(rg@min, rg@max)
                        , filterId = rg@filterId
                      )
      expect_equal(filterObject(rg), expectRes)
      
      #1d gate
      rg <- rectangleGate(filterId="myRectGate", "FSC-H"=c(200, 600))
      expectRes <- list(type = 2L
                        , params = parameters(rg)
                        , range = c(rg@min, rg@max) 
                        , filterId = rg@filterId
                    )
      expect_equal(filterObject(rg), expectRes)
      
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
          
          
      expect_equal(filterObject(pg), expectRes)
      
    })

test_that("filterObject from booleanFilter", {
      
      bf <- booleanFilter(IL2&TNFa&!IFNg , filterId = "myBoolFilter")
      expectRes <- list(type = 3L
                        , refs = c("IL2", "TNFa", "IFNg")
                        , isNot = c(FALSE, FALSE, TRUE)
                        , op = c("&", "&", "&")
                        , filterId = bf@filterId
                      ) 
      
      expect_equal(filterObject(bf), expectRes)
      
      #with node path
      bf <- booleanFilter(cd4/IL2&cd4/TNFa&!cd4/IFNg , filterId = "myBoolFilter")
      expectRes$refs <- file.path("cd4", expectRes$refs) 
      expect_equal(filterObject(bf), expectRes)
      
      #extra ! symbol
      bf <- booleanFilter(!IL2&!!TNFa&!IFNg , filterId = "myBoolFilter")
      expect_error(filterObject(bf), "extra '!' symbol found in the reference node names of boolean fitler")
      
      #extra & symbol
      bf <- booleanFilter(IL2&&TNFa&!IFNg , filterId = "myBoolFilter")
      expect_error(filterObject(bf), "'&&' or '||' found in the expression of booleanFilter")

      #extra & symbol
      bf <- booleanFilter(IL2||TNFa&!IFNg , filterId = "myBoolFilter")
      expect_error(filterObject(bf), "double operater")
      
      
    })