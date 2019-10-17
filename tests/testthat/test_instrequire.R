context("Auto install/load libraries with instrequire");

test_that("instrequire",{
  # list of libraries, including a previously not installed one are processed
  expect_null(instrequire(c('extrafontdb','readr','readxl','tibble')));
  # now they should all show up as loaded
  expect_length(intersect(paste0('package:',c('extrafontdb','readr','readxl'
                                              ,'tibble')),search()),4);
  expect_error(instrequire(c('NONEXISTENT','readr','readxl')));
});

# cleanup
remove.packages('extrafontdb',.libPaths());
c()
