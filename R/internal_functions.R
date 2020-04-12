
# convert an angle from degrees to radians
degrees_to_radians <- function(degree) {
  radian <- 2 * pi * degree / 360
  return(radian)
}

# generate parameter values for the shoots
define_shoots <- function(n_tips, tree_param, child_number) {
  shoot_param <- tibble::tibble(
    child = child_number,
    scale_shift = sample(x = tree_param$scale_shifts, size = n_tips, replace = TRUE),
    angle_shift = sample(x = tree_param$angle_shifts, size = n_tips, replace = TRUE)
  )
}

grow_shoots <- function(tips, shoot_param, tree_param) {

  n_tips <- nrow(tips)
  n_keep <- 1 + stats::rbinom(
    n = 1,
    size = n_tips - 1,
    prob = 1 - tree_param$p_prune
  )

  new_tips <- tips %>%
    dplyr::mutate(
      x_0 = x_2,
      y_0 = y_2,
      scale = scale * shoot_param$scale_shift,
      angle = angle + shoot_param$angle_shift,
      generation = generation + 1,
      family = 1:dplyr::n(),
      child = shoot_param$child,
      x_2 = x_0 + scale * cos(degrees_to_radians(angle)),
      y_2 = y_0 + scale * sin(degrees_to_radians(angle)),
      x_1 = x_0 + (scale/2) * cos(degrees_to_radians(angle - shoot_param$angle_shift)),
      y_1 = y_0 + (scale/2) * sin(degrees_to_radians(angle - shoot_param$angle_shift))
    ) %>%
    dplyr::sample_n(size = n_keep)

  return(new_tips)
}


grow_generation <- function(tips, tree_param) {

  n_tips <- nrow(tips)
  n_shoots <- tree_param$n_shoots

  shoot_par <- purrr::map(1:n_shoots, ~define_shoots(n_tips, tree_param, .x))
  shoot_dat <- purrr::map_dfr(shoot_par, ~grow_shoots(tips, .x, tree_param))

  return(shoot_dat)
}


grow_tree <- function(tree_param) {

  acorn <- tibble::tibble(
    x_0 = 0,
    y_0 = 0,
    x_1 = 0,
    y_1 = .5,
    x_2 = 0,
    y_2 = 1,
    angle = 90,
    scale = 1,
    generation = 1L,
    family = 1L,
    child = 1L
  )

  g <- tree_param$generations
  fern <- purrr::accumulate(1:g, .f = ~grow_generation(.x, tree_param), .init = acorn)
  fern <- dplyr::bind_rows(fern)

  tree <- fern %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    tidyr::pivot_longer(cols = x_0:y_2, names_to = "type", values_to = "coord") %>%
    tidyr::separate(col = type, into = c("axis", "type")) %>%
    tidyr::pivot_wider(names_from = axis, values_from = coord) %>%
    dplyr::mutate(type = as.numeric(type))

  return(tree)
}



