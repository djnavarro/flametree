# Save the plot

Save the plot

## Usage

``` r
flametree_save(plot, filename, ...)
```

## Arguments

- plot:

  The ggplot object

- filename:

  The path to the file

- ...:

  Other arguments to be passed to ggsave

## Value

Invisibly returns NULL.

## Details

The `flametree_save()` function provides a very thin wrapper around the
`ggsave()` function from ggplot2. It reverses the order of the first two
arguments: the plot argument comes before filename, in order to be more
pipe-friendly. The second thing it does is inspect the plot object to
determine the background colour, and ensures that colour is also used to
specify the background colour for the graphics device (e.g., the bg
argument to [`png()`](https://rdrr.io/r/grDevices/png.html)). The reason
for doing this is that plots created using
[`flametree_plot()`](https://flametree.djnavarro.net/reference/flametree_plot.md)
typically force the coordinates to be on the same scale using
`coord_equal()`. As a consequence, if the aspect ratio of the image
differs from the aspect ratio of the ggplot there will be sections of
the image that show the background colour of the graphics device rather
than the background colour specified by the ggplot object. By overriding
the default behaviour of `ggsave()`, the `flametree_save()` function
ensures that the image has the same background colour everywhere.

## Examples

``` r
if (FALSE) { # \dontrun{
# typical usage
flametree_grow(trees = 5, time = 8) |>
  flametree_plot(style = "voronoi") |>
  flametree_save(filename = "~/Desktop/myfile.png")

# passing additional arguments to ggsave()
flametree_grow(trees = 5, time = 8) |>
  flametree_plot(style = "voronoi") |>
  flametree_save(
    filename = "~/Desktop/myfile.png",
    height = 8,
    width = 8
  )
} # }
```
