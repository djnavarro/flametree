# Spark functions to control tree growth

Spark functions to control tree growth

## Usage

``` r
spark_linear(x = 0, y = 0, tree = 0, time = 0, constant = 0)

spark_decay(x = 0, y = 0, tree = 0, time = 0, multiplier = 2, constant = 0)

spark_random(multiplier = 3, constant = 0)

spark_nothing()
```

## Arguments

- x:

  Weight given to the horizontal co-ordinate

- y:

  Weight given to the vertical co-ordinate

- tree:

  Weight given to the tree number

- time:

  Weight given to the time point

- constant:

  Constant value to be added to the output

- multiplier:

  Scaling parameter that multiplies the output

## Value

A function that takes `coord_x`, `coord_y`, `id_tree`, and `id_time` as
input, and returns a numeric vector as output.

## Details

Some arguments to
[`flametree_grow()`](https://flametree.djnavarro.net/reference/flametree_grow.md)
take numeric input, but `seg_col`, `seg_wid`, `shift_x`, `shift_y`, and
`prune` all take functions as their input, and are used to control how
the colours (`seg_col`) and width (`seg_wid`) of the segments are
created, the horizontal (`shift_x`) and vertical (`shift_y`)
displacement of the trees, and the probability that a given shoot stops
growing (`prune`). Functions passed to these arguments take four inputs:
`coord_x`, `coord_y`, `id_tree`, and `id_time` as input. Any function
that takes these variables as input and produces a numeric vector of the
same length as the input can be used for this purpose. However, as a
convenience, four "spark" functions are provided that can be used to
create functions that are suitable for this purpose: `spark_linear()`,
`spark_decay()`, `spark_random()`, and `spark_nothing()`. Arguments
passed to one of the spark functions determine the specific function is
generated. For example, `spark_linear()` can be used to construct any
linear combination of the inputs: `spark_linear(x = 3, y = 2)` would
return a function that computes the sum `(3 * coord_x) + (2 * coord_y)`.
Different values provided as input produce different linear functions.
Analogously, `spark_decay()` returns functions that are exponentially
decaying functions of a linear combination of inputs. The
`spark_random()` generator can be used to generate functions that return
random values, and `spark_nothing()` produces a function that always
returns zero regardless of input.

## Examples

``` r
# returns a linear function of x and y
spark_linear(x = 3,  y = 2)
#> function (coord_x, coord_y, id_tree, id_time) 
#> {
#>     (x * coord_x) + (y * coord_y) + (tree * id_tree) + (time * 
#>         id_time) + constant
#> }
#> <bytecode: 0x55598a401f70>
#> <environment: 0x555992b2a848>

# returns a function of time that decays
# exponentially to an asymptote
spark_decay(time = .1, constant = .1)
#> function (coord_x, coord_y, id_tree, id_time) 
#> {
#>     multiplier * exp(-abs((x * coord_x) + (y * coord_y) + (tree * 
#>         id_tree) + (time * id_time))^2) + constant
#> }
#> <bytecode: 0x55598a41ef40>
#> <environment: 0x5559927c37b8>

# returns a numeric vector containing
# copies of the same uniform random number
# constrained to lie between -2.5 and 2.5
spark_random(multiplier = 5)
#> function (coord_x, coord_y, id_tree, id_time) 
#> {
#>     n <- length(coord_x)
#>     u <- stats::runif(1, min = -multiplier/2, max = multiplier/2) + 
#>         constant
#>     return(rep(u, n))
#> }
#> <bytecode: 0x55598a3df240>
#> <environment: 0x55598fce4540>

# returns a function that always produces
# a vector of zeros
spark_nothing()
#> function (coord_x, coord_y, id_tree, id_time) 
#> {
#>     n <- length(coord_x)
#>     return(rep(0, n))
#> }
#> <bytecode: 0x55598a3ff950>
#> <environment: 0x555991558568>
```
