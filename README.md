
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flametree

<!-- badges: start -->

[![R-CMD-check](https://github.com/djnavarro/flametree/workflows/R-CMD-check/badge.svg)](https://github.com/djnavarro/flametree/actions)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/flametree/branch/master/graph/badge.svg)](https://codecov.io/gh/djnavarro/flametree?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/flametree)](https://CRAN.R-project.org/package=flametree)
<!-- badges: end -->

The goal of flametree is to make pretty pictures.

## Installation

You can install the development version of flametree from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("djnavarro/flametree")
```

## Example

``` r
library(flametree)

flametree_grow(seed = 4, time = 12)  %>% 
  flametree_plot()         
```

<img src="man/figures/README-example-1-1.png" width="80%" />

``` r
flametree_grow(seed = 1, trees = 5)  %>% 
  flametree_plot(style = "voronoi")
```

<img src="man/figures/README-example-2-1.png" width="80%" />

``` r
flametree_grow(trees = 16)  %>% 
  flametree_plot(style = "nativeflora")
```

<img src="man/figures/README-example-3-1.png" width="80%" />
