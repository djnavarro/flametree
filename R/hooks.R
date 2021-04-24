

seg_col <- function(x, y, angle) {
  sqrt(x ^ 2 + y ^ 2) + (angle - 90) / 10
}

seg_wid <- function(time) {
  .05 + exp(-time^2 / 10)
}

shift_x <- function(scale = 3) {
  stats::runif(1, min = -scale/2, max = scale/2)
}

shift_y <- function(scale = 0) {
  stats::runif(1, min = -scale/2, max = scale/2)
}
