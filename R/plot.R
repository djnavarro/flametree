
#' Create a plot from a flametree data frame
#'
#' @param data The data frame specifying the flametree
#' @param background The background colour of the image
#' @param palette A vector of colours
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
  if(style == "minimal") return(ft__plot_minimal(data, background, palette))
  if(style == "voronoi") return(ft__plot_voronoi(data, background, palette))
  if(style == "wisp") return(ft__plot_wisp(data, background, palette))
  if(style == "nativeflora") return(ft__plot_nativeflora(data, background, palette))

  stop('`style` argument must be "plain", "minimal", "voronoi", "wisp", or "nativeflora', call. = FALSE)
}


ft__plot_plain <- function(data, background, palette) {

  # build the ggplot
  picture <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = coord_x,          # x-coordinate
      y = coord_y,          # y-coordinate
      group = id_pathtree,  # each segment/path is a single bezier curve
      size = seg_wid,       # the seg_wid variable is used to set line width
      color = seg_col       # the seg_col variable is used to set line colour
    )
  ) +
    ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
    ggplot2::scale_color_gradientn(colours = palette) +
    ggplot2::scale_size_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      )
    )

  return(picture)

}



ft__plot_minimal <- function(data, background, palette) {

  # build the ggplot
  picture <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = coord_x,          # x-coordinate
      y = coord_y,          # y-coordinate
      group = id_pathtree,  # each segment/path defines its own path
      size = seg_wid,       # the seg_wid variable is used to set line width
      color = seg_col       # the seg_col variable is used to set line colour
    )
  ) +
    ggplot2::geom_path(show.legend = FALSE) +
    ggplot2::scale_color_gradientn(colours = palette) +
    ggplot2::scale_size_identity() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      )
    )

  return(picture)

}




ft__plot_voronoi <- function(data, background, palette) {

  # "leaf" coordinates are at terminal locations (id_step = 2)
  # on the terminal branches (id_leaf == TRUE) in the tree
  leaf <- data %>%
    dplyr::filter(id_leaf == TRUE, id_step == 2) %>%
    dplyr::select(coord_x, coord_y, id_path)

  # remove duplicated rows
  leaf <- leaf[!duplicated(leaf[, c("coord_x", "coord_y")]),, drop = FALSE]

  # create the plot...
  picture <- ggplot2::ggplot() +

    # tree trunk is drawn using geom_bezier
    ggforce::geom_bezier(
      data = data,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y,
        group = id_pathtree,
        size = seg_wid
      ),
      color = "white",
      lineend = "round",
      show.legend = FALSE
    ) +

    # add voronoi tiles with no perturbation
    ggforce::geom_voronoi_tile(
      data = leaf,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y,
        fill = id_path,
        colour = id_path
      ),
      inherit.aes = FALSE,
      show.legend = FALSE,
      max.radius = .2,
      size = .1
    ) +

    ggplot2::scale_fill_gradientn(colours = palette) +
    ggplot2::scale_colour_gradientn(colours = palette) +
    ggplot2::scale_size_identity() +
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

  tree_shade <- palette[1]
  leaf_shade <- palette[2]
  background <- background

  # "leaf" coordinates are at terminal locations (id_step = 2)
  # on the terminal branches (id_leaf == TRUE) in the tree
  leaf <- data %>% dplyr::filter(id_leaf == TRUE, id_step == 2)

  picture <- ggplot2::ggplot() +
    ggforce::geom_bezier(
      data = data,
      mapping = ggplot2::aes(
        x = coord_x,
        y = coord_y,
        size = seg_wid,
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



ft__plot_nativeflora <- function(data, background, palette) {

  data <- data %>%
    dplyr::group_by(id_tree) %>%
    dplyr::filter(
      id_path %in% sample(max(id_path), 0.5 * max(id_path)),
      id_time > 2
    ) %>%
    dplyr::ungroup()

  leaf <- data %>%
    dplyr::filter(id_time == max(id_time), id_step == 2)

  picture <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = coord_x,
      y = coord_y,
      group = id_pathtree,
      colour = id_tree
    )) +
    ggforce::geom_bezier(
      alpha = 1,
      size = 0.3,
      show.legend = FALSE, lineend = "round") +
    ggplot2::geom_point(data = leaf, show.legend = FALSE, size = 1.3, stroke = 0) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_color_gradientn(colours = palette) +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    ggplot2::theme(panel.background = ggplot2::element_rect(
      fill = background,
      colour = background
    ))

  return(picture)
}

