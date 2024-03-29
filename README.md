
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidbits

The goal of tidbits is to package up some utility functions that have
proven useful in multiple data analysis projects and teaching, so they
can be properly documented and more easily deployed. Including
`autoread()` function which wraps readers for a wide variety of data
formats so the same script can run on different files without editing
the file-loading code.

## Installation

You can install tidbits from github with:

``` r
# install.packages("devtools")
devtools::install_github("bokov/tidbits")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidbits);
#> 
#> Attaching package: 'tidbits'
#> The following object is masked from 'package:grDevices':
#> 
#>     cm
# Read data from the NAACCR website ...
dat00 <- autoread('https://www.naaccr.org/wp-content/uploads/2017/02/naaccr_cina_2009_2013_stage.sas7bdat');
# Build an automatic data dictionary
dct0 <- tblinfo(dat00)
```

[![Travis-CI Build
Status](https://travis-ci.org/bokov/tidbits.svg?branch=integration)](https://travis-ci.org/bokov/tidbits)
[![Coverage
Status](https://img.shields.io/codecov/c/github/bokov/tidbits/integration.svg)](https://codecov.io/github/bokov/tidbits?branch=integration)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/bokov/tidbits?branch=integration&svg=true)](https://ci.appveyor.com/project/bokov/tidbits)
