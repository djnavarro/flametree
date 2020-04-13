
# convert an angle from degrees to radians
degrees_to_radians <- function(degree) {
  radian <- 2 * pi * degree / 360
  return(radian)
}

# a theme that is entirely blank, like theme_void(), with
# one exception: the user can specify the background colour
theme_mono <- function(color = "black") {
  th <- ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = color,
        colour = color
      )
    )

  # return the theme
  return(th)
}

# generate parameter values for the shoots
define_shoots <- function(n_tips, tree_param, shoot_number) {

  shoot_param <- tibble::tibble(
    scale_shift = sample(
      x = tree_param$scale_shifts,
      size = n_tips,
      replace = TRUE
    ),
    angle_shift = sample(
      x = tree_param$angle_shifts,
      size = n_tips,
      replace = TRUE
    )
  )

  return(shoot_param)
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
      seg_len = seg_len * shoot_param$scale_shift,
      seg_deg = seg_deg + shoot_param$angle_shift,
      id_time = id_time + 1L,
      x_2 = x_0 + seg_len * cos(degrees_to_radians(seg_deg)),
      y_2 = y_0 + seg_len * sin(degrees_to_radians(seg_deg)),
      x_1 = x_0 + (seg_len/2) * cos(degrees_to_radians(seg_deg - shoot_param$angle_shift)),
      y_1 = y_0 + (seg_len/2) * sin(degrees_to_radians(seg_deg - shoot_param$angle_shift))
    ) %>%
    dplyr::sample_n(size = n_keep)

  return(new_tips)
}


grow_generation <- function(tips, gen, tree_param) {

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
    seg_deg = 90,
    seg_len = 1,
    id_time = 1L
  )

  g <- tree_param$generations
  fern <- purrr::accumulate(
    .x = 1:g,
    .f = grow_generation,
    .init = acorn,
    tree_param = tree_param
  )

  fern <- dplyr::bind_rows(fern)

  tree <- fern %>%
    dplyr::mutate(id_path = as.integer(1:dplyr::n())) %>%
    tidyr::pivot_longer(cols = x_0:y_2, names_to = "id_step", values_to = "coord") %>%
    tidyr::separate(col = id_step, into = c("axis", "id_step")) %>%
    tidyr::pivot_wider(names_from = axis, values_from = coord) %>%
    dplyr::mutate(id_step = as.integer(id_step)) %>%
    dplyr::rename(coord_x = x, coord_y = y) %>%
    dplyr::select(coord_x, coord_y, seg_deg, seg_len, id_time, id_path, id_step)

  return(tree)
}



