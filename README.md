
<!-- README.md is generated from README.Rmd. Please edit that file -->

# packr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/TylerGrantSmith/packr.svg?branch=master)](https://travis-ci.org/TylerGrantSmith/packr)
[![Codecov test
coverage](https://codecov.io/gh/TylerGrantSmith/packr/branch/master/graph/badge.svg)](https://codecov.io/gh/TylerGrantSmith/packr?branch=master)
<!-- badges: end -->

The goal of packr is to …

## Installation

``` r
# install.packages("devtools")
devtools::install_github("TylerGrantSmith/packr")
```

## Example

`unpack` will modify expressions by appending explicit package
references.

``` r
library(packr)
#> Registered S3 method overwritten by 'pryr':
#>   method      from
#>   print.bytes Rcpp
unpack(unpack)
#> packr::unpack
```

`unpack_function` will make explicit functions imported from other
packages and non-exported functions from the package itself.

``` r
library(packr)
unpack
#> function(x, envir = caller_env()) {
#>   expr <- enexpr(x)
#>   unpack_(expr, envir)
#> }
#> <bytecode: 0x0000000013e9fc10>
#> <environment: namespace:packr>
unpack_function(unpack)
#> function(x, envir = rlang::caller_env()) {
#>     expr <- rlang::enexpr(x)
#>     packr:::unpack_(expr, envir)
#> }
```
