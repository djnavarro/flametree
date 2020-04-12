
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flametree

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/djnavarro/flametree.svg?branch=master)](https://travis-ci.org/djnavarro/flametree)
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

ftree <- flametree_grow(seed = 4, generations = 13)
image <- flametree_plot(tree = ftree)
plot(image)
```

<img src="man/figures/README-example-1.png" width="100%" />
