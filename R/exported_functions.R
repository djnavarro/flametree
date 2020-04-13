

#' Generate data for a flametree
#'
#' @param seed for the random number generator
#' @param generations number of generations to iterate
#' @param scale_shifts vector of possible rescales
#' @param angle_shifts vector of possible angle changes
#' @param n_shoots maximum number of shoots from each tip
#' @param p_prune probability a shoot is pruned
#'
#' @return a tibble
#' @export
flametree_grow <- function(
  seed = 1,
  generations = 6,
  scale_shifts = c(.8, .9),
  angle_shifts = c(-30, -20, 20, 30),
  n_shoots = 2,
  p_prune = 0
) {

  param <- list(
    seed = seed,
    generations = generations,
    scale_shifts = scale_shifts,
    angle_shifts = angle_shifts,
    n_shoots = 2,
    p_prune = 0
  )

  set.seed(param$seed)
  tree <- grow_tree(param)
  return(tree)
}


#' Create a plot from a flametree data frame
#'
#' @param tree the data frame
#' @param background the background colour
#' @param palette the palette
#'
#' @return a ggplot2 object
#' @export
flametree_plot <- function(
  tree = flametree_grow(),
  background = "antiquewhite4",
  palette = "viridis::inferno"
) {

  # construct the mapping from tree object to the plot
  mapping <- ggplot2::aes(
    x = coord_x,
    y = coord_y,
    group = id_path,
    size = exp(-.5 * id_time),
    color = sqrt(coord_x ^ 2 + coord_y ^ 2) + (seg_deg - 90) / 10
  )

  # build the ggplot
  picture <- ggplot2::ggplot(data = tree, mapping = mapping) +
    ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
    paletteer::scale_color_paletteer_c(palette = palette) +
    theme_mono(color = background)

  return(picture)
}


#' Save a flametree image
#'
#' @param plot the plot to save
#' @param filename path to file
#' @param pixels height and width of the image in pixels
#' @param ... arguments passed to ggsave
#'
#' @export
flametree_save <- function(plot, filename, pixels = 5000, ...) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = pixels/300,
    height = pixels/300,
    dpi = 300,
    ...
  )
}

