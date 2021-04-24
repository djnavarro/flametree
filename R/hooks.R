
ft__seg_col <- function(coord_x, coord_y, id_tree, id_time, seg_deg, seg_len) {
  sqrt(coord_x ^ 2 + coord_y ^ 2) + (seg_deg - 90) / 10
}

ft__seg_wid <- function(coord_x, coord_y, id_tree, id_time, seg_deg, seg_len) {
  .05 + exp(-id_time ^ 2 / 10)
}

ft__shift_x <- function(coord_x, coord_y, id_tree, id_time, seg_deg, seg_len) {
  stats::runif(1, min = -1.5, max = 1.5)
}

ft__shift_y <- function(coord_x, coord_y, id_tree, id_time, seg_deg, seg_len) {
  0
}

ft__n_shoot <- function(id_tree, id_time) {
  2
}
