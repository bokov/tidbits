context("Auto install/load libraries with instrequire");

test_that("instrequire",{
  #dir.create(testrlib <- normalizePath('.Rlib'));
  #expect_true(dir.exists(testrlib));
  #.libPaths(testrlib);
  #expect_true(testrlib %in% .libPaths());
  # list of libraries, including a previously not installed one are processed
  #message('Installing package that exists');
  expect_null(instrequire(c('extrafontdb','readr','readxl','tibble')));
  # now they should all show up as loaded
  #message('Checking that package loaded');
  expect_length(intersect(paste0('package:',c('extrafontdb','readr','readxl'
                                              ,'tibble')),search()),4);
  #message('Trying to load a package that does not exist');
  expect_error(instrequire(c('NONEXISTENT','readr','readxl')));
  try(unloadNamespace('extrafontdb'));
  try(remove.packages('extrafontdb'));
  #.libPaths(.libPaths()[-1]);
  #try(unlink(testrlib,recursive = TRUE, force = TRUE));
})

