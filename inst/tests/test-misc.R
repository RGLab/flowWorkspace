context("internal functions")


test_that("parse Conditional variable from formula",{
      f1 <- `FSC-A` ~ `SSC-A` | PTID + VISITNO + STIM
      cond <- f1[[3]][[3]]
      expect_equal(.parseCond(cond), c("PTID", "VISITNO", "STIM"))  
      
      f1 <- `FSC-A` ~ `SSC-A` | PTID
      cond <- f1[[3]][[3]]
      expect_equal(.parseCond(cond), c("PTID"))
      
      f1 <- `FSC-A` ~ `SSC-A` | PTID * VISITNO
      cond <- f1[[3]][[3]]
      expect_equal(.parseCond(cond), c("PTID", "VISITNO"))
    })


test_that("formula parser for plotGate (xyplot)",{
      f1 <- `FSC-A` ~ `SSC-A` | PTID + VISITNO + STIM
      expect_identical(.formulaParser(f1), list(xTerm = as.symbol('SSC-A')
                                                , yTerm = as.symbol('FSC-A')
                                                , xfunc = NULL
                                                , yfunc = NULL
                                                , groupBy = c("PTID", "VISITNO", "STIM")
                                                )
                      )
      
      f1 <- `FSC-A` ~ `SSC-A` 
      expect_identical(.formulaParser(f1), list(xTerm = as.symbol('SSC-A')
                                                , yTerm = as.symbol('FSC-A')
                                                , xfunc = NULL
                                                , yfunc = NULL
                                                , groupBy = NULL
                                            )
                       )
                       
     f1 <- factor(`FSC-A`) ~  log(`SSC-A`) | PTID + VISITNO + STIM
     expect_identical(.formulaParser(f1), list(xTerm = as.symbol('SSC-A')
                                         , yTerm = as.symbol('FSC-A')
                                         , xfunc = as.symbol('log')
                                         , yfunc = as.symbol('factor')
                                         , groupBy = c("PTID", "VISITNO", "STIM")
                                     )
                      )
                       
                      
                      
    })

