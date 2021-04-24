
#' Generate the data specifying a flametree
#'
#' @param seed Integer-valued seed for the random number generator
#' @param time Number of generations to run the iterative process
#' @param scale Vector of possible values for the "size rescaling" at each iteration
#' @param angle Vector of possible angle changes (in degrees) at each iteration
#' @param trees Number of trees to generate
#' @param seg_col Function to control the segment colour
#' @param seg_wid Function to control the segment width
#' @param shift_x Function to control horizontal jitter
#' @param shift_y Function to control vertical jitter
#' @param n_shoot Function to control tree branching
#'
#' @return A tibble with the following columns: coord_x, coord_y, seg_deg,
#' seg_len, seg_col, seg_wid, id_time, id_path, id_step, id_leaf, id_tree
#'
#' The two "coord" columns
#' specify the locations of a point. The "id" columns uniquely identify each
#' point: id_time specifies the generation, id_path identifies each segment, and
#' id_step contains the three values (0, 1 or 2) for the points that define each
#' segment. The segments consist of two end points (0 and 2) and one "control"
#' point (1) that is used to define a Bezier curve. Finally, id_tree is a
#' numeric identifier indicating which tree the row belongs, and id_pathtree
#' is a character variable that pastes the id_path and id_tree identifiers to
#' create an identifier that is unique for each path in each tree.
#'
#' The three "seg" columns provide summary information about each segment:
#' seg_len is the length of the segment, seg_col is a value used to colour
#' the segment, and seg_wid is a size parameter used to define the width of
#' the segment
#'
#' @examples
#' flametree_grow()
#' flametree_grow(time = 10)
#'
#' @export
flametree_grow <- function(
  seed = 286,
  time = 6,
  scale = c(.6, .8, .9),
  angle = c(-10, 10, 20),
  trees = 1,
  seg_col = NULL,
  seg_wid = NULL,
  shift_x = NULL,
  shift_y = NULL,
  n_shoot = NULL
) {

  # collect parameters into a list
  options <- list(
    seed = seed,    # seed for the RNG
    time = time,    # time (iterations) to grow the tree
    scale = scale,  # possible values for rescaling at each time
    angle = angle,  # possible values for redirect at each time
    trees = trees,  # number of trees to include
    shift_x = shift_x %||% ft__shift_x, # function to control horizontal jitter
    shift_y = shift_y %||% ft__shift_y, # function to control vertical jitter
    seg_col = seg_col %||% ft__seg_col, # function to control segment colour
    seg_wid = seg_wid %||% ft__seg_wid, # function to control segment width
    n_shoot = n_shoot %||% ft__n_shoot  # function to control branching
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
    ft__shape_tree(id) %>%                  # reshape
    dplyr::mutate(
      coord_x = coord_x + param$shift_x(coord_x, coord_y, id, id_time, seg_deg, seg_len),
      coord_y = coord_y + param$shift_y(coord_x, coord_y, id, id_time, seg_deg, seg_len),
      seg_col = param$seg_col(coord_x, coord_y, id, id_time, seg_deg, seg_len),
      seg_wid = param$seg_wid(coord_x, coord_y, id, id_time, seg_deg, seg_len)
    )

  return(tree)
}



# to grow a "layer" of the shrub, we extend (and possibly prune) each
# existing shoot multiple times
ft__grow_layer <- function(shoots, time, param, id) {

  new_shoots <- purrr::map_dfr(
    .x = 1:param$n_shoot(id, time),
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

  # time must be a single positive integer
  ft__check_not_null(x$time, "time")
  ft__check_not_na(x$time, "time")
  ft__check_soft_integer(x$time, "time")
  ft__check_length_exact(x$time, "time", 1)
  ft__check_value_minimum(x$time, "time", 1)

  # scale values must be non-negative numbers
  ft__check_not_null(x$scale, "scale")
  ft__check_not_na(x$scale, "scale")
  ft__check_length_minimum(x$scale, "scale", 1)
  ft__check_value_minimum(x$scale, "scale", 0) # also checks numeric

  # angle values must be numeric (note: range of angles is not restricted)
  ft__check_not_null(x$angle, "angle")
  ft__check_not_na(x$angle, "angle")
  ft__check_numeric(x$angle, "angle")
  ft__check_length_minimum(x$angle, "angle", 1)

  # # split must be a single positive integer
  # ft__check_not_null(x$split, "split")
  # ft__check_not_na(x$split, "split")
  # ft__check_soft_integer(x$split, "split")
  # ft__check_length_exact(x$split, "split", 1)
  # ft__check_value_minimum(x$split, "split", 1)
  #
  # # prune must be numeric between 0 and 1
  # ft__check_not_null(x$prune, "prune")
  # ft__check_not_na(x$prune, "prune")
  # ft__check_length_exact(x$prune, "prune", 1)
  # ft__check_value_minimum(x$prune, "prune", 0)
  # ft__check_value_maximum(x$prune, "prune", 1)

}


