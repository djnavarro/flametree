

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



# the flame tree "grows" from an acorn, a tibble with a
# single row that defines the very first segment
build_acorn <- function() {
  acorn <- tibble::tibble(
    x_0 = 0, y_0 = 0,  # first shoot starts at origin
    x_1 = 0, y_1 = .5, # first shoot guide is its midpoint
    x_2 = 0, y_2 = 1,  # first shoot grow to y = 1
    seg_deg = 90,      # segment orientation is vertical
    seg_len = 1,       # segment length is 1
    id_time = 1L       # the acorn grows at "time 1"
  )
  return(acorn)
}

build_layer <- function(shoots, time, param) {
  new_shoots <- purrr::map_dfr(
    .x = 1:param$split,
    .f = grow_shoots,
    shoots = shoots,
    param = param
  )
  return(new_shoots)
}


build_shrub <- function(acorn, param) {

  shrub <- purrr::accumulate(
    .x = 1:param$time,
    .f = build_layer,
    .init = acorn,
    param = param
  )
  return(shrub)
}

shape_shrub <- function(shrub) {

  ftree <- shrub %>%
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

  return(ftree)
}






