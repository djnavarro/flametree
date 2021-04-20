# the three user-facing functions of the package


#' Generate the data specifying a flametree
#'
#' @param seed Integer-valued seed for the random number generator
#' @param time Number of generations to run the iterative process
#' @param scale Vector of possible values for the "size rescaling" at each iteration
#' @param angle Vector of possible angle changes (in degrees) at each iteration
#' @param split Maximum number of shoots to generate from each tip at each iteration
#' @param prune Probability with which a generated shoot is pruned
#'
#' @return A tibble with the following columns: coord_x, coord_y, seg_deg,
#' seg_len, seg_col, seg_wid, id_time, id_path, id_step, id_leaf
#'
#' The two "coord" columns
#' specify the locations of a point. The "id" columns uniquely identify each
#' point: id_time specifies the generation, id_path identifies each segment, and
#' id_step contains the three values (0, 1 or 2) for the points that define each
#' segment. The segments consist of two end points (0 and 2) and one "control"
#' point (1) that is used to define a Bezier curve.
#'
#' The three "seg" columns provide summary information about each segment:
#' seg_len is the length of the segment, seg_col is a value used to colour
#' the segment, and seg_wid is a size parameter used to define the width of
#' the segment
#' @export
flametree_grow <- function(seed = 286,
                           time = 6,
                           scale = c(.8, .9),
                           angle = c(-10, 10, 20),
                           split = 2,
                           prune = 0) {

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
  tree <- grow_sapling() %>%  # sapling is the first segment
    grow_tree(param) %>%      # grow the tree with
    shape_tree()

  # add the leaf indicator
  tree$id_leaf <- tree$id_time == max(tree$id_time)

  return(tree)
}


# functions that do the bulk of the work


# the very first shoot is the "sapling"
grow_sapling <- function() {

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


# for each existing shoot on the tree, grow an additional shoot that
# extends it; then prune some of them away
grow_shoots <- function(time, shoots, param) {

  n_shoots <- nrow(shoots)
  n_pruned <- stats::rbinom(n = 1, size = n_shoots - 1, prob = param$prune)

  ch_seg_len <- sample(x = param$scale, size = n_shoots, replace = TRUE)
  ch_seg_deg <- sample(x = param$angle, size = n_shoots, replace = TRUE)

  shoots <- shoots %>%
    dplyr::mutate(
      x_0 = x_2,
      y_0 = y_2,
      seg_len = seg_len * ch_seg_len,
      x_1 = x_0 + extend_x(seg_len/2, seg_deg),
      y_1 = y_0 + extend_y(seg_len/2, seg_deg),
      seg_deg = seg_deg + ch_seg_deg,
      id_time = id_time + 1L,
      x_2 = x_0 + extend_x(seg_len, seg_deg) ,
      y_2 = y_0 + extend_y(seg_len, seg_deg),
    ) %>%
    dplyr::sample_n(size = n_shoots - n_pruned)

  return(shoots)
}


# to grow a "layer" of the shrub, we extend (and possibly prune) each
# existing shoot multiple times
grow_layer <- function(shoots, time, param) {

  new_shoots <- purrr::map_dfr(
    .x = 1:param$split,
    .f = grow_shoots,
    shoots = shoots,
    param = param
  )
  return(new_shoots)
}


# to grow the whole tree we need to "accumulate" the growth: starting with
# the sapling (a single shoot) we grow the second layer; the set of shoots
# that make the second layer are then used to grow the third later; and so on
grow_tree <- function(sapling, param) {

  tree <- purrr::accumulate(
    .x = 1:param$time,
    .f = grow_layer,
    .init = sapling,
    param = param
  )
  return(tree)
}


# the data structure that we used to grow the tree is designed to allow
# efficient computation, but is not optimal for ggplot3 so it needs to
# be reshaped into a convenient form
shape_tree <- function(tree) {

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
      seg_col = sqrt(x ^ 2 + y ^ 2) + (seg_deg - 90) / 10,
      seg_wid = exp(-id_time^2 / 10)
    ) %>%
    dplyr::rename(coord_x = x, coord_y = y) %>%
    dplyr::select(
      coord_x, coord_y, seg_deg, seg_len, seg_col, seg_wid,
      id_time, id_path, id_step
    )

  return(tree)
}

