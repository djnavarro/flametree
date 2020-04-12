
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flametree

<!-- badges: start -->

<!-- badges: end -->

The goal of flametree is to …

## Installation

You can install the released version of flametree from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("flametree")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("djnavarro/flametree")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(flametree)

ftree <- flametree_grow(seed = 4, generations = 13)
image <- flametree_plot(tree = ftree)
plot(image)
```

<img src="man/figures/README-example-1.png" width="100%" />
