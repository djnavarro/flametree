
#' Create a wisptree plot from a flametree data frame
#'
#' @param tree tree data
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' dat <- flametree_grow()
#' wisptree_plot(dat)
wisptree_plot <- function(tree) {

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
