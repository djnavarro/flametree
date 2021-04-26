
#' Create a plot from a flametree data frame
#'
#' @param data The data frame specifying the flametree
#' @param background The background colour of the image
#' @param palette A vector of colours
#' @param style Style of tree to draw
#'
#' @details The \code{flametree_plot()} function provides several ways to
#' visualise the data created by the generative system implemented by
#' \code{flametree_grow()}. The \code{background} argument sets the background
#' colour of the image, and should either be a string specifying an RGB hex
#' colour (e.g., "#000000") or the of a colour recognised by R (see the
#' \code{colours()} function for details). Analogously, the \code{palette}
#' argument should be a vector of colours. However, the \code{palette} argument
#' is  interpreted slightly differently depending on which style of plot is
#' created, discussed below. To set the \code{style} of the resulting plot,
#' pass one of the following style names: "plain" (the default), "voronoi",
#' "wisp", "nativeflora", "minimal", or "themegray".
#'
#' Plots in the "plain" style have the following properties. Branches of
#' the trees vary in width using the \code{seg_wid} data column. Each branch
#' is shown as a curved segment created using \code{geom_bezier2()}, and the
#' colour of the segments is mapped to the \code{seg_col} column in the data.
#' No leaves are drawn. In this style, the elements of the \code{palette} are
#' used to create a continuous n-colour gradient using
#' \code{scale_colour_gradientn()}.
#'
#' Plots in the "voronoi" style draw the shape of the tree the same way as
#' the plain style, except that the segments do not vary in colour and are
#' rendered using \code{geom_bezier()} instead of \code{geom_bezier2()}. Unlike
#' the plain style, stylised "leaves" are drawn by constructing a Voronoi
#' tesselation of the terminal nodes in the tree. Note that computing the
#' tesselation is computationally expensive and this will likely produce
#' errors if there are too many nodes (typically when the \code{time} parameter
#' to \code{flametree_grow()} is large). The interpretation of the
#' \code{palette} argument is slightly different: the first element of the
#' palette is used to set the colour of the trees, and the rest of the palette
#' colours are used to create the gradient palette used to colour the tiles
#' depicted in the Voronoi tesselation.
#'
#' The \code{style = "nativeflora"} style creates a plot in which tree branches
#' are rendered as thin segments, with a proportion of those segments removed,
#' and small points are drawn at the end of each terminal segment. The width of
#' the branches does not vary (i.e., \code{seg_wid} is ignored) and the colour
#' of the branches is constant within tree, but does vary across trees, ignoring
#' the continuous valued \code{seg_col} variable and using only the
#' \code{id_tree} variable to do so. As with the plain style, the
#' \code{palette} colours are used to define an n-colour gradient.
#'
#' The "wisp" style is similar to nativeflora, but no segments are removed, and
#' the wdith of the branches is mapped to \code{seg_wid}. It only uses the first
#' two elements of \code{palette}: the first element specifies the colour of the
#' branches, and the second element specifies the colour of the leaf dots.
#'
#' The final two styles are simplifications of other styles. The "minimal"
#' style is similar to the plain style but does not use curved segments, relying
#' on \code{geom_path()} to draw the branches. The "themegray" style does this
#' too, but it ignores the \code{palette} argument entirely, rendering the trees
#' in black, set against the default gray background specified by the ggplot2
#' \code{theme_gray()} function.
#'
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # the default tree in the plain style
#' flametree_grow() %>% flametree_plot()
#'
#' # 10 trees drawn in the nativeflora style
#' flametree_grow(trees = 10, shift_x = spark_nothing()) %>%
#'   flametree_plot(style = "nativeflora")
#'
#' # changing the palette
#' shades <- c("#A06AB4", "#FFD743", "#07BB9C", "#D773A2")
#' flametree_grow() %>% flametree_plot(palette = shades)
#'
flametree_plot <- function(
  data,
  background = "black",
  palette = c("#1E2640", "#F3EAC0", "#DC9750", "#922C40"),
  style = "plain"
) {

  # palette: https://www.canva.com/colors/color-palettes/middle-eastern-empire/

  ft__check_plot_input(
    data = data,
    background = background,
    palette = palette,
    style = style
  )

  if(style == "plain") return(ft__plot_plain(data, background, palette))
  if(style == "minimal") return(ft__plot_minimal(data, background, palette))
  if(style == "themegray" | style == "themegrey") return(ft__plot_themegray(data, background, palette)) # secretly support correct spelling
  if(style == "voronoi") return(ft__plot_voronoi(data, background, palette))
  if(style == "wisp") return(ft__plot_wisp(data, background, palette))
  if(style == "nativeflora") return(ft__plot_nativeflora(data, background, palette))

  stop('`style` argument must be "plain", "minimal", "themegray", "voronoi", "wisp", or "nativeflora"', call. = FALSE)
}

ft__check_plot_input <- function(data, background, palette, style) {

  ft__check_flametree(data)

  # check length of non data arguments
  ft__check_length_exact(background, "background", 1)
  ft__check_length_exact(style, "style", 1)
  ft__check_length_minimum(palette, "palette", 1)

  # check inputs are not functions
  ft__check_not_closure(background, "background")
  ft__check_not_closure(style, "style")
  ft__check_not_closure(palette, "palette")

  # check arguments are atomic
  ft__check_atomic(background, "background")
  ft__check_atomic(style, "style")
  ft__check_atomic(palette, "palette")

  # check non-missingness for all arguments
  ft__check_not_na(background, "background")
  ft__check_not_na(style, "style")
  ft__check_not_na(palette, "palette")

  # check style is character
  ft__check_character(style, "style")

  # check palette and background define colours
  ft__check_colour(background, "background")
  ft__check_colour(palette, "palette")

}

# might need to be a little more precise here
ft__check_colour <- function(x, name) {
  ft__check_character(x, name)
}

ft__check_flametree <- function(data) {

  ft__check_dataframe(data, "data")
  ft__check_length_exact(data, "data", 12)

  # throw error if column types don't match
  col_names <- names(data)
  flm_names <- c("coord_x", "coord_y", "id_tree", "id_time", "id_path", "id_leaf",
                 "id_pathtree", "id_step", "seg_deg", "seg_len", "seg_col", "seg_wid")
  if(any(col_names != flm_names)) {
    stop("unexpected column names in `data` input", call. = FALSE)
  }

  # throw error if column types don't match
  ft__check_numeric(data$coord_x, "coord_x")
  ft__check_numeric(data$coord_y, "coord_y")
  ft__check_numeric(data$seg_deg, "seg_deg")
  ft__check_numeric(data$seg_len, "seg_len")
  ft__check_numeric(data$seg_col, "seg_col")
  ft__check_numeric(data$seg_wid, "seg_wid")
  ft__check_soft_integer(data$id_time, "id_time")
  ft__check_soft_integer(data$id_path, "id_path")
  ft__check_soft_integer(data$id_step, "id_step")
  ft__check_soft_integer(data$id_tree, "id_tree")
  ft__check_logical(data$id_leaf, "id_leaf")
  ft__check_character(data$id_pathtree, "id_pathtree")

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


ft__plot_themegray <- function(data, background, palette) {

  # build the ggplot
  picture <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = coord_x,          # x-coordinate
      y = coord_y,          # y-coordinate
      group = id_pathtree,  # each segment/path defines its own path
      size = seg_wid        # the seg_wid variable is used to set line width
    )
  ) +
    ggplot2::geom_path(show.legend = FALSE) +
    ggplot2::scale_size_identity() +
    ggplot2::coord_equal() +
    ggplot2::theme_gray()

  return(picture)
}




ft__plot_voronoi <- function(data, background, palette) {

  # ggforce stat_voronoi_tile requires deldir, but ggforce only lists
  # deldir as a suggest. TODO: add check here?
  requireNamespace("deldir", quietly = TRUE)

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
      color = palette[1],
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

    ggplot2::scale_fill_gradientn(colours = palette[-1]) +
    ggplot2::scale_colour_gradientn(colours = palette[-1]) +
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
        group = id_pathtree
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

