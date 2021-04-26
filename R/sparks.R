
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
#' @details Some arguments to \code{flametree_grow()} take numeric input, but
#' \code{seg_col}, \code{seg_wid}, \code{shift_x}, and \code{shift_y} all
#' take functions as their input, and are used to
#' control how the colours (\code{seg_col}) and width (\code{seg_wid}) of the
#' segments are created, as well as the horizontal (\code{shift_x}) and
#' vertical (\code{shift_y}) displacement of the trees are generated. Functions
#' passed to these arguments take four inputs: \code{coord_x}, \code{coord_y},
#' \code{id_tree}, and \code{id_time} as input. Any function that takes
#' these variables as input and produces a numeric vector of the same length
#' as the input can be used for this purpose. However, as a
#' convenience, four "spark" functions are provided that can be used to create
#' functions that are suitable for this purpose: \code{spark_linear()},
#' \code{spark_decay()}, \code{spark_random()}, and \code{spark_nothing()}.
#' Arguments passed to one of the spark functions determine the specific
#' function is generated. For example, \code{spark_linear()} can be used to
#' construct any linear combination of the inputs:
#' \code{spark_linear(x = 3, y = 2)} would return a function that computes
#' the sum \code{(3 * coord_x) + (2 * coord_y)}. Different values provided as
#' input produce different linear functions. Analogously, \code{spark_decay()}
#' returns functions that are exponentially decaying functions of a linear
#' combination of inputs. The \code{spark_random()} generator can be used to
#' generate functions that return random values, and \code{spark_nothing()}
#' produces a function that always returns zero regardless of input.
#'
#' @return A function that takes \code{coord_x}, \code{coord_y}, \code{id_tree},
#' and \code{id_time} as input, and returns a numeric vector as output.
#'
#' @examples
#' # returns a linear function of x and y
#' spark_linear(x = 3,  y = 2)
#'
#' # returns a function of time that decays
#' # exponentially to an asymptote
#' spark_decay(time = .1, constant = .1)
#'
#' # returns a numeric vector containing
#' # copies of the same uniform random number
#' # constrained to lie between -2.5 and 2.5
#' spark_random(multiplier = 5)
#'
#' # returns a function that always produces
#' # a vector of zeros
#' spark_nothing()
#'
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
