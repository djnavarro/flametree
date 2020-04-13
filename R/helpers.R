# convenience functions that are handy to have


# convert an angle from degrees to radians
radians <- function(degree) {
  2 * pi * degree / 360
}

# horizontal distance
extend_x <- function(distance, angle) {
  distance * cos(radians(angle))
}

# vertical distance
extend_y <- function(distance, angle) {
  distance * sin(radians(angle))
}

# a blank theme with a monochromatic background
theme_mono <- function(color = "black") {
  ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(
        fill = color,
        colour = color
      )
    )
}
