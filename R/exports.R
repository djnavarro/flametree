

#' Generate data for a flametree
#'
#' @param seed for the random number generator
#' @param time number of generations to iterate
#' @param scale vector of possible rescales
#' @param angle vector of possible angle changes
#' @param split maximum number of shoots from each tip
#' @param prune probability a shoot is pruned
#'
#' @return a tibble
#' @export
flametree_grow <- function(
  seed = 1,
  time = 6,
  scale = c(.8, .9),
  angle = c(-30, -20, 20, 30),
  split = 2,
  prune = 0
) {

  # parameters defining the tree
  param <- list(
    seed = seed,    # seed for the RNG
    time = time,    # time (iterations) to grow the tree
    scale = scale,  # possible values for rescaling at each time
    angle = angle,  # possible values for redirect at each time
    split = split,  # number of new shoots from each old shoot at each time
    prune = prune   # probability of immediately pruning a new shoot
  )

  # set the seed for the random number generator
  set.seed(param$seed)

  # growing the tree is a 3-step process
  acorn <- build_acorn()              # acorn is the first segment
  shrub <- build_shrub(acorn, param)  # grow the tree structure
  ftree <- shape_shrub(shrub)         # make it suitable for plotting

  return(ftree)
}


#' Create a plot from a flametree data frame
#'
#' @param tree the data frame
#' @param background the background colour
#' @param palette the palette
#' @param mapping an aesthetic mapping passed to ggplot
#'
#' @return a ggplot2 object
#' @export
flametree_plot <- function(
  tree = flametree_grow(),
  background = "antiquewhite4",
  palette = "viridis::inferno",
  mapping = NULL
) {

  # in most cases, the user would probably use this mapping
  if(is.null(mapping)) {
    mapping <- ggplot2::aes(
      x = coord_x,      # x-coordinate
      y = coord_y,      # y-coordinate
      group = id_path,  # each segment/path is a single bezier curve
      size = seg_wid,   # the seg_wid variable is used to set line width
      color = seg_col   # the seg_col variable is used to set line colour
    )
  }

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

