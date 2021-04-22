
#' Generate the data specifying an ashtree
#'
#' @param seed Integer-valued seed for the random number generator
#' @param time Number of generations to run the iterative process
#' @param scale Vector of possible values for the "size rescaling" at each iteration
#' @param angle Vector of possible angle changes (in degrees) at each iteration
#' @param split Maximum number of shoots to generate from each tip at each iteration
#'
#' @return A tibble with the following columns: ...
#' @export
#'
#' @examples
#' ashtree_grow()
#' ashtree_grow(time = 10)
ashtree_grow <- function(seed = 3,
                         time = 6,
                         scale = c(.5, .8, .9, .95),
                         angle = c(-10, -5, 0, 5, 15, 20, 25),
                         split = 2) {


  param <- list(seed = seed, time = time, scale = scale,
                angle = angle, split = split)

  set.seed(seed)

  # ashtree helper functions ------------------------------------------------

  # adjusts the length of a segment
  adjust_scale <- function(s, scales) {
    s * sample(x = scales, size = length(s), replace = TRUE)
  }

  # adjusts the orientation of a segment
  adjust_angle <- function(a, angles) {
    a + sample(x = angles, size = length(a), replace = TRUE)
  }

  # adjusts the x co-ordinate
  adjust_x <- function(x, scale, angle) {
    x + scale * cos(radians(angle))
  }

  # adjusts the t co-ordinate
  adjust_y <- function(y, scale, angle) {
    y + scale * sin(radians(angle))
  }


  # grow sapling to initiate tree growth ------------------------------------

  grow_sapling <- function() {

    sapling <- tibble::tibble(
      old_x = 0,  # sapling grows from the origin
      old_y = 0,  # sapling grows from the origin
      new_x = 0,  # sapling doesn't grow horizontally
      new_y = 1,  # sapling does grow vertically
      angle = 90, # angle from horizontal is 90 degrees
      scale = 1,  # length of the sapling is 1
    )
    return(sapling)
  }



  # grow a single tip -------------------------------------------------------

  grow_from <- function(tips, param) {

    # read off the relevant settings
    all_scales <- param$scale
    all_angles <- param$angle

    # mutate the tips tibble
    new_growth <- tips %>%
      dplyr::mutate(
        old_x = new_x,                            # begin where last seg ended
        old_y = new_y,                            # begin where last seg ended
        scale = adjust_scale(scale, all_scales),  # change segment length
        angle = adjust_angle(angle, all_angles),  # change segment angle
        new_x = adjust_x(old_x, scale, angle),    # end where this seg ends!
        new_y = adjust_y(old_y, scale, angle)     # end where this seg ends!
      )
    return(new_growth)
  }


  # grow one tip into multiple new tips (or branches) -----------------------

  grow_multi <- function(tips, param) {

    branches <- purrr::map_dfr(
      .x = 1:param$split,
      .f = ~ grow_from(tips, param)
    )
    return(branches)
  }



  # generate the ashtree data -----------------------------------------------

  tree <- purrr::accumulate(
    .x = 1:param$time,
    .f = ~ grow_multi(., param),
    .init = grow_sapling()
  )
  tree <- dplyr::bind_rows(tree)
  return(tree)

}



# draw a picture of a tree ------------------------------------------------

#' Create a plot from an ashtree data frame
#'
#' @param tree The data frame specifying the ashtree
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' dat <- ashtree_grow(time = 5)
#' ashtree_plot(dat)
ashtree_plot <- function(tree) {

  pic <- ggplot2::ggplot(
    data = tree,
    mapping = ggplot2::aes(
      x = old_x,
      y = old_y,
      xend = new_x,
      yend = new_y
    )
  ) +
    ggplot2::geom_segment() +
    ggplot2::theme_void() +
    ggplot2::coord_equal()

  return(pic)
}







