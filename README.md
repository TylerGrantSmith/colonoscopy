
<!-- README.md is generated from README.Rmd. Please edit that file -->

# colonoscopy <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/TylerGrantSmith/colonoscopy.svg?branch=master)](https://travis-ci.org/TylerGrantSmith/colonoscopy)
[![Codecov test
coverage](https://codecov.io/gh/TylerGrantSmith/colonoscopy/branch/master/graph/badge.svg)](https://codecov.io/gh/TylerGrantSmith/colonoscopy?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`colonoscopy` provides functions and RStudio addins to easily add or
remove explicit package scoping (`::`, `:::`)

## Installation

``` r
# install.packages("devtools")
devtools::install_github("TylerGrantSmith/colonoscopy")
```

## Example

`scope` will modify expressions or functions by appending explicit
package references.

``` r
library(colonoscopy)
library(ggplot2)
scope("aes")
#> ggplot2::aes
aes
#> function (x, y, ...) 
#> {
#>     exprs <- enquos(x = x, y = y, ..., .ignore_empty = "all")
#>     aes <- new_aes(exprs, env = parent.frame())
#>     rename_aes(aes)
#> }
#> <bytecode: 0x00000000132d55f8>
#> <environment: namespace:ggplot2>
scope(aes)
#> function (x, y, ...)
#> {
#>     exprs <- rlang::enquos(x = x, y = y, ..., .ignore_empty = "all")
#>     aes <- new_aes(exprs, env = parent.frame())
#>     rename_aes(aes)
#> }
scope(aes, inPackage = FALSE)
#> function (x, y, ...)
#> {
#>     exprs <- rlang::enquos(x = x, y = y, ..., .ignore_empty = "all")
#>     aes <- ggplot2:::new_aes(exprs, env = parent.frame())
#>     ggplot2:::rename_aes(aes)
#> }
```

`unscope` will modify expressions or functions by removing `::` and
`:::` operators (and the package name).

``` r
unscope("ggplot2::alpha")
#> alpha
alpha
#> function (colour, alpha = NA) 
#> {
#>     if (length(colour) != length(alpha)) {
#>         if (length(colour) > 1 && length(alpha) > 1) {
#>             stop("Only one of colour and alpha can be vectorised")
#>         }
#>         if (length(colour) > 1) {
#>             alpha <- rep(alpha, length.out = length(colour))
#>         }
#>         else {
#>             colour <- rep(colour, length.out = length(alpha))
#>         }
#>     }
#>     rgb <- farver::decode_colour(colour, alpha = TRUE)
#>     rgb[!is.na(alpha), 4] <- alpha[!is.na(alpha)]
#>     farver::encode_colour(rgb, rgb[, 4])
#> }
#> <bytecode: 0x0000000013834df0>
#> <environment: namespace:scales>
unscope(alpha)
#> function (colour, alpha = NA)
#> {
#>     if (length(colour) != length(alpha)) {
#>         if (length(colour) > 1 && length(alpha) > 1) {
#>             stop("Only one of colour and alpha can be vectorised")
#>         }
#>         if (length(colour) > 1) {
#>             alpha <- rep(alpha, length.out = length(colour))
#>         }
#>         else {
#>             colour <- rep(colour, length.out = length(alpha))
#>         }
#>     }
#>     rgb <- decode_colour(colour, alpha = TRUE)
#>     rgb[!is.na(alpha), 4] <- alpha[!is.na(alpha)]
#>     encode_colour(rgb, rgb[, 4])
#> }
```
