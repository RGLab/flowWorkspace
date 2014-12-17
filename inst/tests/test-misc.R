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

test_that("mkformula",{
      
      expect_equal(mkformula(c("FSC-A", "SSC-A")),`FSC-A` ~ `SSC-A`)  
      
      expect_equal(mkformula(c("SSC-A", "FSC-A")),`SSC-A` ~ `FSC-A`)
      
      expect_equal(mkformula(c(x = "SSC-A", y = "FSC-A")),`FSC-A` ~ `SSC-A`)
      
      expect_equal(mkformula(c(y = "SSC-A", x = "FSC-A")), `SSC-A` ~ `FSC-A`)
      
      expect_warning(mkformula(c(x = "SSC-A", x = "FSC-A")), "invalid axis names:")
      
      expect_equal(suppressWarnings(mkformula(c(x = "SSC-A", x = "FSC-A"))), `SSC-A` ~ `FSC-A`)
      
      expect_equal(mkformula(c("SSC-A", "FSC-A")),`SSC-A` ~ `FSC-A`)
      
      expect_equal(mkformula(c("SSC-A", "FSC-A"), isChar = TRUE),"`SSC-A`~`FSC-A`")
      
      
      
      f1 <- `FSC-A` ~ `SSC-A` | PTID
      cond <- f1[[3]][[3]]
      expect_equal(.parseCond(cond), c("PTID"))
      
      f1 <- `FSC-A` ~ `SSC-A` | PTID * VISITNO
      cond <- f1[[3]][[3]]
      expect_equal(.parseCond(cond), c("PTID", "VISITNO"))
    })

test_that("trimWhiteSpace",
    {
      expect_equal(trimWhiteSpace(" a b "), "a b")
      expect_equal(trimWhiteSpace(" a b \n"), "a b")
      expect_equal(trimWhiteSpace(" a b \r\n"), "a b")
      expect_equal(trimWhiteSpace(" a b \t\r\n"), "a b")
      expect_equal(trimWhiteSpace("\t a \r b \t\r\n"), "a \r b")
    })


