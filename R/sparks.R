
#' Spark functions to control tree growth
#'
#' @name sparks
#' @param x Weight given to the horizontal co-ordinate
#' @param y Weight given to the horizontal co-ordinate
#' @param tree Weight given to the tree number
#' @param time Weight given to the time point
#' @param multiplier Scaling parameter that multiplies the output
#' @param constant Constant value to be added to the output
#'
#' @details The growth of the flametree is partially governed by controller
#' functions called "sparks". A spark function takes four arguments (coord_x,
#' coord_y, id_tree, and id_time) and outputs a numeric value. These functions
#' can be specified manually and used as inputs to \code{flametree_grow()},
#' or can be generated using one of the built in generator functions
#'
#' @return A function with arguments coord_x, coord_y, id_tree, and id_time
NULL

#' @rdname sparks
#' @export
spark_linear <- function(x = 0, y = 0, tree = 0, time = 0, constant = 0) {
  ft__check_spark_input(list(x = x, y = y, tree = tree, time = time,
                             constant = constant))
  function(coord_x, coord_y, id_tree, id_time) {
    (x * coord_x) + (y * coord_y) + (tree * id_tree) + (time * id_time) + constant
  }
}

#' @rdname sparks
#' @export
spark_decay <- function(x = 0, y = 0, tree = 0, time = 0, multiplier = 2, constant = 0) {
  ft__check_spark_input(list(x = x, y = y, tree = tree, time = time,
                         multiplier = multiplier, constant = constant))
  function(coord_x, coord_y, id_tree, id_time) {
    multiplier * exp(-abs((x * coord_x) + (y * coord_y) + (tree * id_tree) + (time * id_time))^2) + constant
  }
}

#' @rdname sparks
#' @export
spark_random <- function(multiplier = 3, constant = 0) {
  ft__check_spark_input(list(multiplier = multiplier, constant = constant))
  function(coord_x, coord_y, id_tree, id_time) {
    n <- length(coord_x)
    u <- stats::runif(1, min = -multiplier/2, max = multiplier/2) + constant
    return(rep(u, n))
  }
}

#' @rdname sparks
#' @export
spark_nothing <- function() {
  function(coord_x, coord_y, id_tree, id_time) {
    n <- length(coord_x)
    return(rep(0, n))
  }
}


ft__check_spark_input <- function(arg_list) {
  purrr::walk2(arg_list, names(arg_list), ~ ft__check_length_exact(.x, .y, 1))
  purrr::walk2(arg_list, names(arg_list), ft__check_not_na)
  purrr::walk2(arg_list, names(arg_list), ft__check_numeric)
}
