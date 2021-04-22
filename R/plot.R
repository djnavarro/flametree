
#' Create a plot from a flametree data frame
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
#' The background colour can be set using the "backgroud" argument, and the
#' palette used to colour the segments is generated using the
#' scale_color_paletteer_c() function from the paletteer package. To select
#' the palette, the "palette" argument must take the form of a palette
#' specification understood by paletteer.
#' @export
#'
#' @examples
#' dat <- flametree_grow(time = 5)
#' flametree_plot(dat)
#' flametree_plot(dat, background = "black")
#'
flametree_plot <- function(tree,
                           background = "antiquewhite4",
                           palette = "viridis::inferno") {

  # specify the mapping
  mapping <- ggplot2::aes(
    x = coord_x,      # x-coordinate
    y = coord_y,      # y-coordinate
    group = id_path,  # each segment/path is a single bezier curve
    size = seg_wid,   # the seg_wid variable is used to set line width
    color = seg_col   # the seg_col variable is used to set line colour
  )

  # build the ggplot
  picture <- ggplot2::ggplot(data = tree, mapping = mapping) +
    ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
    paletteer::scale_color_paletteer_c(palette = palette) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = background,
        colour = background
      )
    )

  return(picture)
}



#' Save a flametree image
#'
#' @param plot The ggplot object to save
#' @param filename The path to file to be saved
#' @param pixels The height and width of the image in pixels
#' @param ... Other arguments passed to ggsave
#'
#' @details This function is just a wrapper to ggsave. It's not strictly
#' necessary but I find it convenient to override the default image size.
#'
#' @examples
#' \dontrun{
#' dat <- flametree_grow(time = 5)
#' pic <- flametree_plot(dat)
#' flametree_save(pic, filename = "path/to/file")
#' flametree_save(pic, filename = "path/to/file", pixels = 2000)
#' }
#'
#' @export
flametree_save <- function(plot, filename, pixels = 5000, ...) {

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = pixels/300,
    height = pixels/300,
    dpi = 300,
    ...
  )
}
