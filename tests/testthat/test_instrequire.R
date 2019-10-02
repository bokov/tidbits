context("Auto install/load libraries with instrequire");

test_that("instrequire"
          ,{
            expect_null(instrequire(c('readr','tibble','dplyr')))
            expect_equal(length(intersect(search()
                                          ,c('package:readr','package:tibble'
                                             ,'package:dplyr'))),3)
          })
