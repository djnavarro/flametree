
#' Save the plot
#'
#' @param plot The ggplot object
#' @param filename The path to the file
#' @param ... Other arguments to be passed to ggsave
#'
#' @details A very thin wrapper around \code{ggsave()}. The only thing it does
#' differently is extract the background colour from the ggplot object and
#' use that to set the bg colour for the output device.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flametree_grow(trees = 5, time = 8) %>%
#'   flametree_plot(style = "voronoi") %>%
#'   flametree_save(filename = "~/Desktop/myfile.png")
#' }
#'
flametree_save <- function(plot, filename, ...) {
  background <- plot$theme$panel.background$fill
  ggplot2::ggsave(
    plot = plot,
    filename = filename,
    bg = background,
    ...
  )
}
