
#' Generate the data specifying a flametree
#'
#' @param seed Integer seed for the random number generator
#' @param time Number of generations to run the iterative process
#' @param scale Vector of possible values for the "size rescaling" at each iteration
#' @param angle Vector of possible angle changes (in degrees) at each iteration
#' @param split Number of splits at each time point
#' @param trees Number of trees to generate
#' @param seg_col Spark function to control the segment colour
#' @param seg_wid Spark function to control the segment width
#' @param shift_x Spark function to control horizontal jitter
#' @param shift_y Spark function to control vertical jitter
#'
#' @details Generative art created with flametree is a visualisation of a data
#' structure created by calling \code{flametree_grow()}. The underlying
#' algorithm is an iterative branching process: each tree starts out as a single
#' vertical segment, to which multiple new segments are added at the end of the
#' first iteration. Over multiple iterations this creates a tree-like structure.
#'
#' The user can control how this iterative process unfolds. By setting the
#' \code{seed} argument the random number generator is reset using
#' \code{set.seed()}. The \code{trees} argument specifies the number of trees
#' to create using this process, the \code{time} argument specifies how many
#' iterations of the branching process will be run (at least two), and the
#' \code{split} argument specifies how many new segments (at least two) will be
#' created each time abranching occurs.
#'
#' When a new segment is created, its size and orientation are controlled by the
#' \code{scale} and \code{angle} arguments. The \code{scale} argument takes a
#' vector of at least two positive numbers. One of these numbers is selected at
#' random whenever a new segment is created, and the length of the new segment
#' is equal to the length of the "parent" segment from which it was created,
#' multiplied by this scaling factor. The orientation of the new segment is
#' controlled by the angle argument in an analogous way. Every time a new
#' segment is generated, one of these angles (interpreted in degrees, not
#' radians) is selected at random. The orientation of the new segment is equal
#' to the orientation of the parent segment plus the sampled angle. Like the
#' \code{scale} argument, \code{angle} must contain at least two values.
#'
#' The remaining arguments (\code{seg_col}, \code{seg_wid}, \code{shift_x},
#' and \code{shift_y}) all take functions as their input, and are used to
#' control how the colours (\code{seg_col}) and width (\code{seg_wid}) of the
#' segments are created, as well as the horizontal (\code{shift_x}) and
#' vertical (\code{shift_y}) displacement of the trees are generated. Functions
#' passed to these arguments take four inputs: \code{coord_x}, \code{coord_y},
#' \code{id_tree}, and \code{id_time}. Any function that takes
#' these variables as input can be used for this purpose. However, as a
#' convenience, four "spark" functions are provided that can be used to create
#' functions that are suitable for this purpose: \code{spark_linear()},
#' \code{spark_decay()}, \code{spark_random()}, and \code{spark_nothing()}.
#'
#' These functions are documented in their own help files. To give an example,
#' the default behaviour of \code{flametree_grow()} adds a random horizontal
#' displacement to each tree to give the impression of multiple trees growing
#' side by side. To suppress this horizontal displacement, set
#' \code{shift_x = spark_nothing()}.
#'
#' @return The output of \code{flametree_grow()}` is a tibble with the following
#' columns: \code{coord_x}, \code{coord_y}, \code{id_tree}, \code{id_time},
#' \code{id_path}, \code{id_leaf}, \code{id_pathtree}, \code{id_step},
#' \code{seg_deg}, \code{seg_len}, \code{seg_col}, and \code{seg_wid}. Each
#' row in the tibble specifies a single point: every curved segment is defined
#' by three such rows.
#'
#' The two "coord" columns are numeric variables that specify the location of
#' the point itself. The "id" columns are used as indicators of various kinds.
#' The \code{id_tree} column contains numbers specifying which tree each point
#' belongs to, and similarly the \code{id_time} column is a numeric identifier
#' that specifies the time point at which the point was generated (i.e., the
#' iteration of the generative process). The \code{id_step} column contains
#' a number (0, 1, or 2) indicating whether the point is the first point, the
#' midpoint, or the end point of the relevant curved segment in a tree. In
#' addition, there are two identifier columns used to denote the segments
#' themselves. The \code{id_path} column is numeric, and assigns value 1 to
#' the "first" segment (i.e., the lowest part of the tree trunk) for every
#' tree, with values increasing numerically for each subsequent segment. Values
#' for \code{id_path} will uniquely identify a segment within a tree, but when
#' multiple trees are generated there will be multiple segments that have the
#' same \code{id_path} value. If a unique identifier across trees is needed,
#' use the \code{id_pathtree} column, which is a character vector constructed
#' by pasting the \code{id_path} and \code{id_tree} values into a string, with
#' an underscore as the separator character.
#'
#' In addition to the two coordinate columns and the six identifier columns,
#' the data generated by \code{flametree_grow()} contains four "seg" columns
#' that are intended to map onto different visual characteristics of a plot.
#' The \code{seg_deg} column specifies the orientation of the segment, whereas
#' \code{seg_len} denotes the length of the segment, \code{seg_col} specifies
#' the colour (as a numeric value that could be interpreted by a palette), and
#' \code{seg_wid} specifies the width of the segment. Note that this information
#' use used differently by the \code{flametree_plot()} function, depending on
#' what style of plot is generated.
#'
#' @examples
#' # flametree data structure with default parameters
#' flametree_grow()
#'
#' # setting time = 10 runs the generative process
#' # longer resulting in a table with more rows
#' flametree_grow(time = 10)
#'
#' # default behaviour is to randomly displace trees
#' # by random horizontal perturbation: to switch this
#' # off use the spark_nothing() function
#' flametree_grow(shift_x = spark_nothing())
#'
#' @export
flametree_grow <- function(
  seed = 286,
  time = 6,
  scale = c(.6, .8, .9),
  angle = c(-10, 10, 20),
  split = 2,
  trees = 1,
  seg_col = spark_linear(tree = 2, time = 1),
  seg_wid = spark_decay(time = .3, multiplier = 5, constant = .1),
  shift_x = spark_random(multiplier = 3),
  shift_y = spark_nothing()
) {

  # collect parameters into a list
  options <- list(
    seed = seed,    # seed for the RNG
    time = time,    # time (iterations) to grow the tree
    scale = scale,  # possible values for rescaling at each time
    angle = angle,  # possible values for redirect at each time
    split = split,  # number of splits at each time point
    trees = trees,  # number of trees to include
    shift_x = shift_x, # function to control horizontal jitter
    shift_y = shift_y, # function to control vertical jitter
    seg_col = seg_col, # function to control segment colour
    seg_wid = seg_wid # function to control segment width
  )
  ft__check_opts(options)

  set.seed(options$seed)
  seeds <- sample(100000, size = options$trees)
  trees <- purrr::map2_dfr(
    .x = 1:options$trees,
    .y = seeds,
    .f = ~ ft__grow_tree(options, .x, .y)
  )
  attr(trees, "options") <- options
  return(trees)
}



# to grow the whole tree we need to "accumulate" the growth: starting with
# the sapling (a single shoot) we grow the second layer; the set of shoots
# that make the second layer are then used to grow the third later; and so on
ft__grow_tree <- function(param, id, local_seed) {

  set.seed(local_seed)

  tree <- purrr::accumulate(
    .x = 1:param$time,
    .f = ft__grow_layer,
    .init = ft__grow_sapling(),
    param = param,
    id = id
  )

  tree <- tree %>%
    ft__shape_tree(id) %>%
    dplyr::mutate(
      coord_x = coord_x + param$shift_x(coord_x, coord_y, id, id_time),
      coord_y = coord_y + param$shift_y(coord_x, coord_y, id, id_time),
      seg_col = param$seg_col(coord_x, coord_y, id, id_time),
      seg_wid = param$seg_wid(coord_x, coord_y, id, id_time)
    )

  return(tree)
}



# to grow a "layer" of the shrub, we extend (and possibly prune) each
# existing shoot multiple times
ft__grow_layer <- function(shoots, time, param, id) {

  new_shoots <- purrr::map_dfr(
    .x = 1:param$split,
    .f = ft__grow_shoots,
    shoots = shoots,
    param = param
  )
  return(new_shoots)
}



# for each existing shoot on the tree, grow an additional shoot that
# extends it; then prune some of them away
ft__grow_shoots <- function(time, shoots, param) {

  n_shoots <- nrow(shoots)

  ch_seg_len <- sample(x = param$scale, size = n_shoots, replace = TRUE)
  ch_seg_deg <- sample(x = param$angle, size = n_shoots, replace = TRUE)

  shoots <- shoots %>%
    dplyr::mutate(
      x_0 = x_2,
      y_0 = y_2,
      seg_len = seg_len * ch_seg_len,
      x_1 = x_0 + ft__extend_x(seg_len/2, seg_deg),
      y_1 = y_0 + ft__extend_y(seg_len/2, seg_deg),
      seg_deg = seg_deg + ch_seg_deg,
      id_time = id_time + 1L,
      x_2 = x_0 + ft__extend_x(seg_len, seg_deg) ,
      y_2 = y_0 + ft__extend_y(seg_len, seg_deg),
    )

  return(shoots)
}



# the very first shoot is the "sapling"
ft__grow_sapling <- function() {

  sapling <- tibble::tibble(
    x_0 = 0, y_0 = 0,  # first shoot starts at origin
    x_1 = 0, y_1 = .5, # first shoot guide is its midpoint
    x_2 = 0, y_2 = 1,  # first shoot grow to y = 1
    seg_deg = 90,      # segment orientation is vertical
    seg_len = 1,       # segment length is 1
    id_time = 1L       # the acorn grows at "time 1"
  )
  return(sapling)
}



# the data structure that we used to grow the tree is designed to allow
# efficient computation, but is not optimal for ggplot2 so it needs to
# be reshaped into a convenient form
ft__shape_tree <- function(tree, id) {

  tree <- tree %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id_path = as.integer(1:dplyr::n())) %>%
    tidyr::pivot_longer(
      cols = x_0:y_2,
      names_to = "id_step",
      values_to = "coord"
    ) %>%
    tidyr::separate(col = id_step, into = c("axis", "id_step")) %>%
    tidyr::pivot_wider(names_from = axis, values_from = coord) %>%
    dplyr::mutate(
      id_step = as.integer(id_step),
      id_leaf = id_time == max(id_time),  # adds leaf node indicator
      id_tree = id,                       # adds tree identifier
      id_pathtree = paste(id_tree, id_path, sep = "_")
    ) %>%
    dplyr::rename(coord_x = x, coord_y = y) %>%
    dplyr::select(coord_x, coord_y, id_tree, id_time, id_path, id_leaf,
                  id_pathtree, id_step, seg_deg, seg_len)

  return(tree)
}


# convert an angle from degrees to radians
ft__radians <- function(degree) {
  2 * pi * degree / 360
}

# horizontal distance
ft__extend_x <- function(distance, angle) {
  distance * cos(ft__radians(angle))
}

# vertical distance
ft__extend_y <- function(distance, angle) {
  distance * sin(ft__radians(angle))
}



# checks user input and throws error message if
ft__check_opts <- function(x) {

  # seed must be a single integer value
  ft__check_not_null(x$seed, "seed")
  ft__check_not_na(x$seed, "seed")
  ft__check_soft_integer(x$seed, "seed")
  ft__check_length_exact(x$seed, "seed", 1)

  # time must be a integer > 1
  ft__check_not_null(x$time, "time")
  ft__check_not_na(x$time, "time")
  ft__check_soft_integer(x$time, "time")
  ft__check_length_exact(x$time, "time", 1)
  ft__check_value_minimum(x$time, "time", 2)

  # scale values must be non-negative numbers
  ft__check_not_null(x$scale, "scale")
  ft__check_not_na(x$scale, "scale")
  ft__check_length_minimum(x$scale, "scale", 2)
  ft__check_value_minimum(x$scale, "scale", 0) # also checks numeric

  # angle values must be numeric (note: range of angles is not restricted)
  ft__check_not_null(x$angle, "angle")
  ft__check_not_na(x$angle, "angle")
  ft__check_numeric(x$angle, "angle")
  ft__check_length_minimum(x$angle, "angle", 2)

  # split must be a single positive integer
  ft__check_not_null(x$split, "split")
  ft__check_not_na(x$split, "split")
  ft__check_soft_integer(x$split, "split")
  ft__check_length_exact(x$split, "split", 1)
  ft__check_value_minimum(x$split, "split", 2)
  #
  # # prune must be numeric between 0 and 1
  # ft__check_not_null(x$prune, "prune")
  # ft__check_not_na(x$prune, "prune")
  # ft__check_length_exact(x$prune, "prune", 1)
  # ft__check_value_minimum(x$prune, "prune", 0)
  # ft__check_value_maximum(x$prune, "prune", 1)

}


