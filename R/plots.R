
#' Create a plot from a flametree data frame
#'
#' @param data The data frame specifying the flametree
#' @param background The background colour of the image
#' @param palette A palette specification used by the paletteer package
#' @param style Style of tree to draw
#'
#' @return The output is ggplot2 object plots the coord_x and coord_y values
#' that define each segment as a bezier curve. To map each segment to its own
#' curve, the group aesthetic is id_path, and the geom is the geom_bezier2()
#' function in the ggforce package. The color aesthetic is mapped to seg_col,
#' and the size aesthetic is mapped to seg_wid.
#'
#' The background colour can be set using the "background" argument, and the
#' palette used to colour the segments is generated using the
#' scale_color_paletteer_c() function from the paletteer package. To select
#' the palette, the "palette" argument must take the form of a palette
#' specification understood by paletteer.
#' @export
#'
#' @examples
#' dat <- flametree_grow()
#' flametree_plot(dat)
#'
flametree_plot <- function(
  data,
  background = "black",
  palette = c("#c06014", "#eddbc0", "#000000", "#cdcdcd"),
  style = "plain"
) {

  if(style == "plain") return(ft__plot_plain(data, background, palette))
  if(style == "voronoi") return(ft__plot_voronoi(data, background, palette))
  if(style == "wisp") return(ft__plot_wisp(data, background, palette))
  if(style == "nativeflora") return(ft__plot_nativeflora(data, background, palette))

  stop('`style` argument must be "plain", "voronoi", "wisp", or "nativeflora', call. = FALSE)
}


ft__plot_plain <- function(data, background, palette) {

  # specify the mapping
  mapping <- ggplot2::aes(
    x = coord_x,      # x-coordinate
    y = coord_y,      # y-coordinate
    group = id_path,  # each segment/path is a single bezier curve
    size = seg_wid,   # the seg_wid variable is used to set line width
    color = seg_col   # the seg_col variable is used to set line colour
  )

  # build the ggplot
  picture <- ggplot2::ggplot(data = data, mapping = mapping) +
    ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
    ggplot2::scale_color_gradientn(colours = palette) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      )
    )

  return(picture)

}

ft__plot_voronoi <- function(tree, background, palette) {

  # "leaf" coordinates are at terminal locations (id_step = 2)
  # on the terminal branches (id_leaf == TRUE) in the tree
  leaf <- tree %>% dplyr::filter(id_leaf == TRUE, id_step == 2)

  # create the plot...
  picture <- ggplot2::ggplot() +

    # tree trunk is drawn using geom_bezier
    ggforce::geom_bezier(
      data = tree,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y,
        group = id_path,
        size = seg_wid
      ),
      color = "white",
      lineend = "round",
      show.legend = FALSE
    ) +

    # add points drawn at the leaves
    ggplot2::geom_point(
      data = leaf,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y
      ),
      size = 8
    ) +

    # add voronoi tiles with no perturbation
    ggforce::geom_voronoi_tile(
      data = leaf,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y
      ),
      max.radius = .2,
      fill = "#ffffffcc",
      colour = "white",
      size = 2
    ) +

    ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
    paletteer::scale_color_paletteer_c(palette = palette) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      )
    )

  return(picture)
}


ft__plot_wisp <- function(data, background, palette) {

  tree_shade <- "white"
  leaf_shade <- "white"
  background <- "black"

  # "leaf" coordinates are at terminal locations (id_step = 2)
  # on the terminal branches (id_leaf == TRUE) in the tree
  leaf <- tree %>% dplyr::filter(id_leaf == TRUE, id_step == 2)

  picture <- ggplot2::ggplot() +
    ggforce::geom_bezier(
      data = tree,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y,
        size = seg_wid * 8,
        group = id_path
      ),
      colour = tree_shade,
      show.legend = FALSE,
      lineend = "round",
      alpha = 1
    ) +
    ggplot2::geom_point(
      data = leaf,  # add leaves last
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y
      ),
      size = 1,
      colour = leaf_shade,
      stroke = 0,
      alpha = 1,
      show.legend = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(panel.background = ggplot2::element_rect(
      fill = background,
      colour = background
    )) +
    ggplot2::scale_size_identity() +
    ggplot2::coord_equal()

  return(picture)
}

ft__plot_nativeflora <- function(tree, background, palette) {

  tree <- tree %>%
    dplyr::group_by(id_tree) %>%
    dplyr::mutate(
      x = coord_x + runif(1, min = -.3, max = .3),
      y = coord_y + runif(1, min = -.02, max = .02)
    ) %>%
    dplyr::filter(
      id_path %in% sample(max(id_path), 0.5 * max(id_path)),
      id_time > 2
    ) %>%
    dplyr::mutate(
      id_branch = id_path + (id * max(id_path))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(id_tree)


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

