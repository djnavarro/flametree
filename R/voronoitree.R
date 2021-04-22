

#' Create a voronoitree plot from a flametree data frame
#'
#' @param tree The data frame specifying the flametree
#' @param background The background colour of the image
#' @param palette A palette specification used by the paletteer package
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
#' dat <- flametree_grow(time = 5)
#' voronoitree_plot(dat)
#' voronoitree_plot(dat, background = "black")
#'
voronoitree_plot <- function(tree, background = "black", palette = "scico::lajolla") {

  # "leaf" coordinates are at terminal locations (id_step = 2)
  # on the terminal branches (id_leaf == TRUE) in the tree
  leaf <- tree %>% dplyr::filter(id_leaf == TRUE, id_step == 2)

  # create the plot...
  picture <- ggplot2::ggplot() +

    # tree trunk is drawn using geom_bezier2
    ggforce::geom_bezier2(
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

