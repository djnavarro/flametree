
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

dat <- flametree_grow(seed = 4, time = 13) # data structure
img <- flametree_plot(tree = dat)          # ggplot object
plot(img)
```

<img src="man/figures/README-example-1.png" width="80%" />