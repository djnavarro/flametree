
nativeflora_grow <- function(seed) {

  set.seed(seed)

  dat <- sample(1000, 12) %>%
    map_dfr(function(s){
      flametree_grow(
        seed = s,
        time = 14,
        scale = c(.5, .7, .9, .8),
        angle = c(-15, 15, 10, -5),
        prune = .1
      ) %>%
        mutate(
          id = s,
          x = coord_x + runif(1, min = -.3, max = .3),
          y = coord_y + runif(1, min = -.02, max = .02)
        ) %>%
        filter(
          id_path %in% sample(max(id_path), 0.5 * max(id_path)),
          id_time > 2
        )
    }) %>%
    mutate(
      id_branch = id_path + (id * max(id_path))
    ) %>%
    arrange(id)

  return(dat)
}

nativeflora_plot <- function(tree) {

  blend_shades <- function(x, y, p = .5) {
    x <- col2rgb(x)
    y <- col2rgb(y)
    z <- round(p*x + (1-p)*y)
    z <- rgb(red = z[1, ]/255, green = z[2, ]/255, blue = z[3, ]/255)
    return(z)
  }

  shades <- sample(colours(distinct = TRUE), 6)

  leaf <- tree %>%
    filter(id_time == max(id_time), id_step == 2)

  picture <- tree %>%
    ggplot(aes(
      x = x,
      y = y,
      group = id_branch,
      colour = id_branch
    )) +
    geom_bezier(
      alpha = 1,
      size = 0.3,
      show.legend = FALSE, lineend = "round") +
    geom_point(data = leaf, show.legend = FALSE, size = 1.3, stroke = 0) +
    theme_void() +
    scale_size_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_fixed(xlim = c(-2.2, 1.8), ylim = c(1.8, 5.8)) +
    scale_color_gradientn(colours = shades) +
    theme(plot.background = element_rect(fill = blend_shades(shades[1], "black", .25)))

  return(picture)
}
